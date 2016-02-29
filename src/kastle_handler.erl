%%%
%%%   Copyright (c) 2016, Klarna AB
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%

%%%=============================================================================
%%% @doc
%%% @copyright 2016 Klarna AB
%%% @end
%%%=============================================================================

-module(kastle_handler).

%%_* Exports ===================================================================

%% cowboy handler callbacks
-export([init/3]).

%% cowboy rest callbacks
-export([ rest_init/2
        , rest_terminate/2
        , allowed_methods/2
        , charsets_provided/2
        , content_types_accepted/2
        , malformed_request/2
        , handler/2
        ]).

%%_* Records ===================================================================
-record(state, { topic     :: {binary(), binary()}
               , partition :: {binary(), binary()}
               }).

%%_* Macros ====================================================================
-define(TOPIC_REQ,     topic).
-define(PARTITION_REQ, partition).
-define(DEFAULT_TOPIC, <<"undefined">>).
-define(DEFAULT_PARTITION, <<"0">>).  %% At the moment no need for default partition
                                      %% as it must be a part of path according to API
-define(MESSAGE_KEY, <<"key">>).
-define(MESSAGE_VALUE, <<"value">>).

%%_* cowboy handler callbacks ==================================================

-spec init({atom(), atom()}, cowboy_req:req(), _) ->
              {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _, _) ->
  {upgrade, protocol, cowboy_rest}.

%%_* cowboy rest callbacks =====================================================

-spec rest_init(cowboy_req:req(), _) ->
                   {ok, cowboy_req:req(), #state{}}.
rest_init(Req0, _) ->
  {Topic, Req1}    = cowboy_req:binding(?TOPIC_REQ,     Req0, ?DEFAULT_TOPIC),
  {Partition, Req} = cowboy_req:binding(?PARTITION_REQ, Req1, ?DEFAULT_PARTITION),
  {ok, Req, #state{topic = Topic, partition = Partition}}.

-spec rest_terminate(cowboy_req:req(), #state{}) -> ok.
rest_terminate(_Req, _State) ->
  %% We might need to add some logging here
  ok.

-spec allowed_methods(cowboy_req:req(), #state{}) ->
                         {[binary()], cowboy_req:req(), #state{}}.
allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

-spec charsets_provided(cowboy_req:req(), #state{}) ->
                           {[binary()], cowboy_req:req(), #state{}}.
charsets_provided(Req, State) ->
  {[<<"utf-8">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), #state{}) ->
                                {[_], cowboy_req:req(), #state{}}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handler}], Req, State}.

-spec malformed_request(cowboy_req:req(), #state{}) ->
                           {boolean(), cowboy_req:req(), #state{}}.
malformed_request(Req, State) ->
  {false, Req, State}.

-spec handler(cowboy_req:req(), #state{}) ->
                 {true | false, cowboy_req:req(), #state{}}.
handler(Req0, State = #state{topic = Topic, partition = Partition0}) ->
  {Result, Req} = do_handle(Topic, string:to_integer(binary_to_list(Partition0)), extract_json_body(Req0)),
  {Result, Req, State}.

%% =============================================================================
%% === Internal functions
%% =============================================================================
extract_json_body(Req0) ->
  {ok, Body, Req} = cowboy_req:body(Req0),
  do_decode(catch jiffy:decode(Body), Req).

do_decode({JsonBody}, Req) -> validate_json(is_valid_json_body(JsonBody), Req);
do_decode({error, _Whatever}, Req) -> {error, Req}.

do_handle(_Topic, _Partition, {error, Req}) -> {false, Req};
do_handle(_Topic, {error, no_integer}, {ok, {_Key, _Message}, Req}) -> {false, Req};
do_handle(Topic, {Partition, []}, {ok, {Key, Message}, Req}) when is_integer(Partition) ->
  {handle_produce(Topic, Partition, [{Key, Message}]), Req}.

validate_json({true, {Key, Message}}, Req) -> {ok, {Key, Message}, Req};
validate_json(false, Req) -> {error, Req}.

is_valid_json_body(JsonBody) ->
  check_message(get_message_key(JsonBody), get_message_value(JsonBody)).

get_message_key(JsonBody) ->
  get_value(lists:keyfind(?MESSAGE_KEY, 1, JsonBody)).

get_message_value(JsonBody) ->
  get_value(lists:keyfind(?MESSAGE_VALUE, 1, JsonBody)).

get_value({?MESSAGE_KEY, KeyValue}) -> KeyValue;
get_value({?MESSAGE_VALUE, MessageValue}) -> MessageValue;
get_value(false) -> false.

check_message(false, _) -> false;
check_message(_, false) -> false;
check_message(Key, Value) -> {true, {Key, Value}}.

handle_produce(Topic, Partition, [{MessageKey, MessageValue}]) ->
  do_handle_produce(get_producers_pid(Topic, Partition), MessageKey, MessageValue).

do_handle_produce({error, no_such_producer}, _Key, _Value) -> false;
do_handle_produce({ok, Producer}, Key, Value) when is_pid(Producer) ->
  do_produce_sync(brod:produce_sync(Producer, Key, Value)).

do_produce_sync(ok) -> true;
do_produce_sync({error, _}) -> false. %% TODO: handle it differently from 400 code

get_producers_pid(Topic, Partition) ->
  do_get_producer(brod:get_producer(kastle_kafka_client, Topic, Partition), Partition).

do_get_producer({ok, Producer}, _Partition) when is_pid(Producer) -> {ok, Producer};
do_get_producer({error, {producer_down, noproc}}, _Partition) -> {error, no_such_producer};
do_get_producer({error, {producer_not_found, _Topic, _Partition}}, _Partition) -> {error, no_such_producer};
do_get_producer({error, {producer_not_found, Topic}}, Partition) ->
  ProducerConfig = [ {topic_restart_delay_seconds, 10} %% topic error
                   , {partition_restart_delay_seconds, 2} %% partition error
                   , {required_acks, -1} ],
  do_start_producer(brod:start_producer(kastle_kafka_client, Topic, ProducerConfig), Topic, Partition).

do_get_producer2({ok, Producer}) when is_pid(Producer) -> {ok, Producer};
do_get_producer2({error, _Whatever}) -> {error, no_such_producer}.

do_start_producer(ok, Topic, Partition) -> do_get_producer2(brod:get_producer(kastle_kafka_client, Topic, Partition));
do_start_producer({error, _}, _Topic, _Partition) -> {error, no_such_producer}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
