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

%% Macros
-define(SERVER, ?MODULE).

%% Records
-record(state, { http_listener :: reference() }).

%%%_* API ======================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%_* gen_server callbacks =====================================================

init([]) ->
  Listener = make_ref(),
  {ok, Port} = application:get_env(kastle, port),
  Host =
    { '_'
      , [ {<<"/rest/kafka/v0/:topic/:partition">>, [], kastle_handler, no_opts}
%%        , {<<"/rest/kafka/v0/:topic">>,            [], kastle_handler, no_opts}
        %, {<<"/health">>,                          [], kastle_ping_handler, no_opts}
        ]
    },
  Transport = [{port, Port}],
  Protocol = [{env, [{dispatch, cowboy_router:compile([Host])}]}],
  {ok, Listeners} = application:get_env(kastle, listeners),
  {ok, _} = cowboy:start_http(Listener, Listeners, Transport, Protocol),
  {ok, #state{http_listener = Listener}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{http_listener = Listener}) ->
  cowboy:stop_listener(Listener),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
