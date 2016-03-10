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
%%% ============================================================================

%% @private
-module(kastle_SUITE).

-include_lib("eunit/include/eunit.hrl").

%% Test framework
-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        , suite/0
        ]).

%% Test cases
-export([ t_retry_partitions/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* ct callbacks =============================================================

suite() -> [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) -> ok.

init_per_testcase(Case, Config) ->
  try
    ?MODULE:Case({init, Config})
  catch
    error : function_clause ->
      Config
  end.

end_per_testcase(Case, Config) ->
  try
    ?MODULE:Case({'end', Config})
  catch
    error : function_clause ->
      ok
  end,
  ok.

all() -> [F || {F, _A} <- module_info(exports),
                  case atom_to_list(F) of
                    "t_" ++ _ -> true;
                    _         -> false
                  end].

%%%_* Test functions ===========================================================
t_retry_partitions({init, Config}) ->
  lists:foreach(fun(M) ->
                    meck:new(M, [passthrough, no_passthrough_cover, no_history])
                end, [brod, brod_client, cowboy_req, kastle_handler]),
  Config;
t_retry_partitions({'end', Config}) ->
  lists:foreach(fun(M) ->
                    meck:validate(M),
                    meck:unload(M)
                end, [brod, brod_client, cowboy_req, kastle_handler]),
  Config;
t_retry_partitions(Config) when is_list(Config) ->
  ok = meck:expect(brod_client, get_partitions_count, fun(_, _) -> {ok, 3} end),
  ProduceFun =
    fun(_Client, _Topic, Partition, _Key, _Value) when is_function(Partition) ->
        {error, boo};
       (_Client, _Topic, Partition, _Key, _Value) when Partition < 2 ->
        {error, boo};
       (_Client, _Topic, _Partition, _Key, _Value) ->
        ok
    end,
  ok = meck:expect(brod, produce_sync, ProduceFun),
  ok = meck:expect(cowboy_req, body, fun(Req) -> {ok, <<"foo">>, Req} end),
  ok = meck:expect(cowboy_req, binding,
                   fun(topic, Req) -> {<<"topic">>, Req};
                      (partition, Req) -> {undefined, Req} end),
  ok = meck:expect(cowboy_req, header,
                   fun(_, Req, _) -> {<<>>, Req} end),
  Self = self(),
  Ref = make_ref(),
  ok = meck:expect(cowboy_req, reply,
                   fun(Code, Req) -> Self ! {Ref, Code}, {ok, Req} end),
  ok = meck:expect(cowboy_req, reply,
                   fun(Code, _, Error, Req) -> Self ! {Ref, Code, Error}, {ok, Req} end),
  kastle_handler:handle_binary([], []),
  receive
    {Ref, 204} ->
      ok;
    {Ref, Code_, Error_} ->
      ct:fail({Code_, Error_})
  after 1000 ->
      ct:fail(timeout)
  end,
  ok.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
