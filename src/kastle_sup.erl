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

-module(kastle_sup).
-author("kirill.zhiganov").

-behaviour(supervisor).

%% API
-export([ start_link/0 ]).

%% Supervisor callbacks
-export([ init/1 ]).

-define(SERVER, ?MODULE).
-define(MAX_RESTARTS, 3).
-define(MAX_SECONDS_BETWEEN_RESTARTS, 10).
-define(SHUTDOWN, 2000).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,

  SupFlags = {RestartStrategy, ?MAX_RESTARTS, ?MAX_SECONDS_BETWEEN_RESTARTS},

  Restart = permanent,
  Type = worker,

  AChild = {'kastle_rest', {'kastle_rest', start_link, []},
    Restart, ?SHUTDOWN, Type, ['kastle_rest']},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
