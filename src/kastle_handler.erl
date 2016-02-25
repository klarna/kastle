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
-define(DEFAULT_VALUE, <<"undefined">>).

%%_* cowboy handler callbacks ==================================================

-spec init({atom(), atom()}, cowboy_req:req(), _) ->
              {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _, _) ->
  {upgrade, protocol, cowboy_rest}.

%%_* cowboy rest callbacks =====================================================

-spec rest_init(cowboy_req:req(), _) ->
                   {ok, cowboy_req:req(), #state{}}.
rest_init(Req0, _) ->
  {Topic, Req1}    = cowboy_req:binding(?TOPIC_REQ,     Req0, ?DEFAULT_VALUE),
  {Partition, Req} = cowboy_req:binding(?PARTITION_REQ, Req1, ?DEFAULT_VALUE),
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
handler(Req, State = #state{topic = Topic, partition = Partition0}) ->
  %%TODO: put handler's code in here
  Partition = binary_to_integer(Partition0),
  case brod:get_producer(kastle_kafka_client, Topic, Partition) of
    {ok, _Producer} ->
      ok = brod:produce_sync(kastle_kafka_client, Topic, Partition, <<"Kastle">>, <<"Producing a message... test coded-in">>),
      io:format("Produced!~n");
    {error, {producer_not_found, _}} ->
%%      ok = brod_client:register_producer(kastle_kafka_client, Topic, Partition),
%%      ClientId = whereis(kastle_kafka_client),
%%      io:format("Id of the client is: ~p~n", [ClientId]),
%%      {ok,_} = gen_server:start(brod_producer, {kastle_kafka_client, Topic, Partition, []}, []),
      ok = brod:start_producer(kastle_kafka_client, Topic, []),
      io:format("Producer added!~n"),
%%      timer:sleep(500),
      {ok, Pid} = brod:get_producer(kastle_kafka_client, Topic, Partition),
      io:format("Producer's id is: ~p~n", [Pid]),
      ok = brod:produce_sync(kastle_kafka_client, Topic, Partition, <<"Kastle">>, <<"Producing a message... test coded-in">>),
      io:format("Produced on new topic!~n")
  end,
  {true, Req, State}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
