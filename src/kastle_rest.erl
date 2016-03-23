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
-record(state, { listeners   :: [reference()]
               , brod_client :: pid()}).

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

  {ok, BrodClient} =
    brod:start_link_client(kastle:getenv(kafka_endpoints),
                           ?BROD_CLIENT,
                           kastle:getenv(brod_client_config)),
  Host =
    { '_'
      , [ {<<"/rest/kafka/v0/:topic/:partition">>, [], kastle_handler, no_opts}
        , {<<"/rest/kafka/v0/:topic">>,            [], kastle_handler, no_opts}
        , {<<"/ping">>,                            [], kastle_ping_handler, no_opts}
        ]
    },
  Protocol = [{env, [{dispatch, cowboy_router:compile([Host])}]}],

  HttpListener = start_http(Protocol),

  SslEnabled = kastle:getenv(ssl_enabled, false),
  HttpsListener = maybe_start_https(SslEnabled, Protocol),

  Listeners = [L || L <- [HttpListener, HttpsListener], L =/= null],
  {ok, #state{listeners = Listeners, brod_client = BrodClient}}.

handle_call(Request, From, State) ->
  lager:warning("~p ~p got unexpected call: ~p from ~p", [?MODULE, self(), Request, From]),
  {reply, {error, {bad_call, Request}}, State}.

handle_cast(Request, State) ->
  lager:warning("~p ~p got unexpected cast: ~p", [?MODULE, self(), Request]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:warning("~p ~p got unexpected info: ~p", [?MODULE, self(), Info]),
  {noreply, State}.

terminate(_Reason, #state{listeners = Listeners, brod_client = BrodClient}) ->
  [cowboy:stop_listener(L) || L <- Listeners],
  brod:stop_client(BrodClient),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%_* Internal functions ========================================================

start_http(Protocol) ->
  HttpConfig = kastle:getenv(http, []),
  Port = proplists:get_value(port, HttpConfig, ?DEFAULT_PORT),
  Acceptors = proplists:get_value(acceptors, HttpConfig, ?DEFAULT_ACCEPTORS),
  Listener = make_ref(),
  lager:info("~p HTTP listener is using port ~p", [?APPLICATION, Port]),
  {ok, _} = cowboy:start_http(Listener, Acceptors, [{port, Port}], Protocol),
  Listener.

maybe_start_https(false, _Protocol) ->
  null;
maybe_start_https(true, Protocol) ->
  SslConfig = kastle:getenv(ssl),
  Port = proplists:get_value(port, SslConfig),
  Acceptors = proplists:get_value(acceptors, SslConfig, ?DEFAULT_ACCEPTORS),
  Listener = make_ref(),
  lager:info("~p HTTPS listener is using port ~p", [?APPLICATION, Port]),
  {ok, _} = cowboy:start_https(Listener, Acceptors, SslConfig, Protocol),
  Listener.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
