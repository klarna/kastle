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
  Partition = binary_to_integer(Partition0),
  {Result, Req} = case extract_json_body(Req0) of
    {ok, {MessageKey, MessageValue}, Req1} ->
      {handle_produce(Topic, Partition, [{MessageKey, MessageValue}]), Req1};
    {error, Req1} ->
      {false, Req1}
  end,
  {Result, Req, State}.

%% =============================================================================
%% === Internal functions
%% =============================================================================
extract_json_body(Req0) ->
  {ok, Body, Req} = cowboy_req:body(Req0),
  {JsonBody}=jiffy:decode(Body),
  case is_valid_json_body(JsonBody) of
    {true, {KeyValue, MessageValue}} ->
      {ok, {KeyValue, MessageValue}, Req};
    false ->
      {error, Req}
  end.

is_valid_json_body(JsonBody) ->
  check_message(get_message_key(JsonBody), get_message_value(JsonBody)).

get_message_key(JsonBody) ->
  case lists:keyfind(?MESSAGE_KEY, 1, JsonBody) of
    {?MESSAGE_KEY, KeyValue} ->
      KeyValue;
    false ->
      false
  end.

get_message_value(JsonBody) ->
  case lists:keyfind(?MESSAGE_VALUE, 1, JsonBody) of
    {?MESSAGE_VALUE, MessageValue} ->
      MessageValue;
    false ->
      false
  end.

check_message(false, _) -> false;
check_message(_, false) -> false;
check_message(Key, Value) -> {true, {Key, Value}}.

handle_produce(Topic, Partition, [{MessageKey, MessageBody}]) ->
  case get_producers_pid(Topic, Partition) of
    {error, no_such_producer} ->
      false;
    {ok, Producer} ->
      ok = brod:produce_sync(Producer, MessageKey, MessageBody),
      true
    end.

get_producers_pid(Topic, Partition) ->
  case brod:get_producer(kastle_kafka_client, Topic, Partition) of
    {ok, Producer} ->
      {ok, Producer};
    {error, {producer_not_found, Topic}} ->
      ProducerConfig = [ {topic_restart_delay_seconds, 10} %% topic error
        , {partition_restart_delay_seconds, 2} %% partition error
        , {required_acks, -1} ],
      ok = brod:start_producer(kastle_kafka_client, Topic, ProducerConfig),
      case brod:get_producer(kastle_kafka_client, Topic, Partition) of
        {ok, Pid} ->
          {ok, Pid};
        {error, {producer_not_found, Topic, Partition}} ->
          {error, no_such_producer}
      end;
    {error, {producer_not_found, Topic, Partition}} ->
      %% No such partition on specified topic?
      {error, no_such_producer}
  end.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
