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

-module(kastle_rest).

-behaviour(gen_server).

%%_* Exports ===================================================================

%% API
-export([ start_link/0 ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%_* Includes ==================================================================
-include("kastle.hrl").

%%_* Records ===================================================================
-record(state, { listeners :: [reference()] }).

%%_* Macros ====================================================================
-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8092).
-define(DEFAULT_ACCEPTORS, 2).

%%%_* API ======================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%_* gen_server callbacks =====================================================

init([]) ->
  Schema = #{<<"type">> => <<"object">>,
             <<"properties">> =>
               #{?MESSAGE_KEY => #{ <<"type">> => <<"string">>,
                                    <<"required">> => true},
                 ?MESSAGE_VALUE => #{<<"type">> => <<"string">>,
                                     <<"required">> => true}}},
  jesse:add_schema(?KASTLE_JSON_SCHEMA, Schema),

  SslTransport = kastle:getenv(ssl_transport, no_ssl),
  TcpPort = kastle:getenv(port, no_tcp_port),
  Host =
    { '_'
      , [ {<<"/rest/kafka/v0/:topic/:partition">>, [], kastle_handler, no_opts}
        , {<<"/rest/kafka/v0/:topic">>, [], kastle_handler, no_opts}
        %, {<<"/ping">>,                            [], kastle_ping_handler, no_opts}
        ]
    },
  Protocol = [{env, [{dispatch, cowboy_router:compile([Host])}]}],
  Acceptors = kastle:getenv(acceptors, ?DEFAULT_ACCEPTORS),

  {HttpsStarted, HttpsListener} = start_https(Acceptors, SslTransport, Protocol),
  {_, HttpListener} = start_http(Acceptors, TcpPort, Protocol, HttpsStarted),

  {ok, #state{listeners = [L || L <- [HttpListener, HttpsListener], L =/= null]}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{listeners = Listeners}) ->
  [cowboy:stop_listener(L) || L <- Listeners],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%_* Internal functions ========================================================

start_https(_, no_ssl, _) ->
  {not_started, null};
start_https(Acceptors, SslTransport, Protocol) ->
  Listener = make_ref(),
  lager:info("~p HTTPS listener is using port ~p", [?APPLICATION, proplists:get_value(port, SslTransport)]),
  {cowboy:start_https(Listener, Acceptors, SslTransport, Protocol), Listener}.

start_http(Acceptors, no_tcp_port, Protocol, not_started) ->
  Listener = make_ref(),
  %% HTTPS listener not started and no TCP port configured:
  %% then start the TCP on the default port
  Transport = [{port, ?DEFAULT_PORT}],
  lager:info("~p HTTP listener is using default port ~p", [?APPLICATION, ?DEFAULT_PORT]),
  {cowboy:start_http(Listener, Acceptors, Transport, Protocol), Listener};
start_http(_, no_tcp_port, _, _) ->
  {not_started, null};
start_http(Acceptors, TcpPort, Protocol, _) ->
  Listener = make_ref(),
  lager:info("~p HTTP listener is using port ~p", [?APPLICATION, TcpPort]),
  {cowboy:start_http(Listener, Acceptors, [{port, TcpPort}], Protocol), Listener}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
