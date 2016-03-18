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

-module(kastle_ping_handler).

%%_* Exports ===================================================================

%% cowboy handler callbacks
-export([ init/3
        , handle/2
        , terminate/3
        ]).

%%_* Includes ==================================================================
-include("kastle.hrl").

%%_* cowboy handler callbacks ==================================================

-spec init({atom(), atom()}, cowboy_req:req(), any()) ->
              {ok, cowboy_req:req(), any()}.
init({tcp, http}, Req, Opts) ->
  {ok, Req, Opts};
init({ssl, http}, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req0, State) ->
  Reply = pong_or_pang(),
  {ok, Req} = cowboy_req:reply(200, [], Reply, Req0),
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%%_* Internal functions =======================================================

%% @doc check if brod_client process is alive
pong_or_pang() ->
  case whereis(?BROD_CLIENT) of
    Pid when is_pid(Pid) -> <<"pong\n">>;
    _                    -> <<"pang\n">>
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
