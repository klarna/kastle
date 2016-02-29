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
-module(kastle).

%% API
-export([ start/0
        , stop/0
        , getenv/1
        , getenv/2
        , get_producer_config/0
        ]).

%%%_* API ======================================================================

%% @doc Start kastle application
start() ->
  application:start(?MODULE, permanent).

%% @doc Stop kastle application
stop() ->
  application:stop(?MODULE).

getenv(Name) ->
  case application:get_env(?APPLICATION, Name) of
    {ok, Value} -> Value;
    undefined   -> erlang:throw({noenv, Name})
  end.

getenv(Name, Default) ->
  try
    getenv(Name)
  catch throw : {noenv, Name} ->
    Default
  end.

get_producer_config() ->
  getenv(producer_config, []).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
