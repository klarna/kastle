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
-module(integration_SUITE).

%% Test framework
-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        , suite/0
        ]).

%% Test cases
-export([ t_produce_json_to_partition_1/1
        , t_produce_json_invalid_key/1
        , t_produce_json_invalid_value/1
        , t_produce_binary_to_partition_1/1
        , t_produce_binary_to_partition_1_no_key/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(assertOK(Code, Msg), case Code >= 200 andalso Code < 300 of
                               true  -> ok;
                               false -> ct:fail("~p: ~s", [Code, Msg])
                             end).

%%%_* ct callbacks =============================================================

suite() -> [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  inets:start(),
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

t_produce_json_to_partition_1(Config) when is_list(Config) ->
  Method = post,
  URL = "http://localhost:8092/rest/kafka/v0/kastle-3-2/0",
  Header = [],
  Type = "application/json",
  Body = make_unique_message_body(),
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1", ReturnCode, _State}, _Head, ReplyBody}} = R,
  ?assertOK(ReturnCode, ReplyBody),
  ok.

t_produce_json_invalid_key(Config) when is_list(Config) ->
  Method = post,
  URL = "http://localhost:8092/rest/kafka/v0/kastle-3-2/0",
  Header = [],
  Type = "application/json",
  Body = "{\"key\": {\"\a\":\"b\"}, \"value\":\"v\"}",
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1", ReturnCode, _State}, _Head, RespBody}} = R,
  ?assertEqual(400, ReturnCode),
  ?assertEqual("{\"error\":\"json schema validation failed: wrong_type\"}", RespBody),
  ok.

t_produce_json_invalid_value(Config) when is_list(Config) ->
  Method = post,
  URL = "http://localhost:8092/rest/kafka/v0/kastle-3-2/0",
  Header = [],
  Type = "application/json",
  Body = "{\"key\": \"\k\", \"value\": {\"a\":\"b\"}}",
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1", ReturnCode, _State}, _Head, RespBody}} = R,
  ?assertEqual(400, ReturnCode),
  ?assertEqual("{\"error\":\"json schema validation failed: wrong_type\"}", RespBody),
  ok.

t_produce_binary_to_partition_1(Config) when is_list(Config) ->
  Method = post,
  URL = "http://localhost:8092/rest/kafka/v0/kastle-3-2/0",
  {K, V} = make_unique_kv(),
  Header = [{"Kafka-Key", binary_to_list(K)}],
  Body = V,
  Type = "application/binary",
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1", ReturnCode, _State}, _Head, ReplyBody}} = R,
  ?assertOK(ReturnCode, ReplyBody),
  ok.

t_produce_binary_to_partition_1_no_key(Config) when is_list(Config) ->
  Method = post,
  URL = "http://localhost:8092/rest/kafka/v0/kastle-3-2/0",
  Header = [],
  Body = crypto:rand_bytes(1000),
  Type = "application/binary",
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1", ReturnCode, _State}, _Head, ReplyBody}} = R,
  ?assertOK(ReturnCode, ReplyBody),
  ok.

%%%_* Help functions ===========================================================

make_unique_message_body() ->
  {K, V} = make_unique_kv(),
  jiffy:encode({[{key, K}, {value, V}]}).

%% os:timestamp should be unique enough for testing
make_unique_kv() ->
  { iolist_to_binary(["key-", make_ts_str()])
  , iolist_to_binary(["val-", make_ts_str()])
  }.

make_ts_str() ->
  Ts = os:timestamp(),
  {{Y,M,D}, {H,Min,Sec}} = calendar:now_to_universal_time(Ts),
  {_, _, Micro} = Ts,
  S = io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w:~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w",
                    [Y, M, D, H, Min, Sec, Micro]),
  lists:flatten(S).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
