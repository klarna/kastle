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

-module(kastle_app).

-behaviour(application).

%% Application behaviour callbacks
-export([ start/2
        , stop/1
        ]).

-include("kastle.hrl").

%%%_* Application behaviour callbacks ==========================================

start(_StartType, _StartArgs) ->
  maybe_update_env(),
  kastle_sup:start_link().

stop(_State) ->
  ok.

%%%_* Internal functions =======================================================

-define(KASTLE_KAFKA_ENDPOINTS, "KASTLE_KAFKA_ENDPOINTS").
-define(KASTLE_HTTP_PORT, "KASTLE_HTTP_PORT").
-define(KASTLE_HTTP_LISTENERS, "KASTLE_HTTP_LISTENERS").
-define(KASTLE_ENABLE_SSL, "KASTLE_ENABLE_SSL").
-define(KASTLE_SSL_PORT, "KASTLE_SSL_PORT").
-define(KASTLE_SSL_LISTENERS, "KASTLE_SSL_LISTENERS").
-define(KASTLE_SSL_CACERTFILE, "KASTLE_SSL_CACERTFILE").
-define(KASTLE_SSL_CERTFILE, "KASTLE_SSL_CERTFILE").
-define(KASTLE_SSL_KEYFILE, "KASTLE_SSL_KEYILE").
-define(KASTLE_SSL_VERIFY, "KASTLE_SSL_VERIFY").
-define(KASTLE_SSL_FAIL_IF_NO_PEER_CERT, "KASTLE_SSL_FAIL_IF_NO_PEER_CERT").

-define(l2i, fun erlang:list_to_integer/1).
-define(l2a, fun erlang:list_to_atom/1).
-define(l2l, fun(X_) -> X_ end).

maybe_update_env() ->
  KafkaEndpoints = os:getenv(?KASTLE_KAFKA_ENDPOINTS),
  ok = maybe_set_kafka_endpoints(KafkaEndpoints),

  ok = maybe_set_kastle_param(?KASTLE_HTTP_PORT, http, port, ?l2i),
  ok = maybe_set_kastle_param(?KASTLE_HTTP_LISTENERS, http, listeners, ?l2i),

  ok = maybe_set_kastle_param(?KASTLE_ENABLE_SSL, root, ssl_enabled, ?l2a),
  case application:get_env(?APPLICATION, ssl_enabled) of
    {ok, true} ->
      ok = maybe_set_kastle_param(?KASTLE_SSL_PORT, ssl, port, ?l2i),
      ok = maybe_set_kastle_param(?KASTLE_SSL_LISTENERS, ssl, listeners, ?l2i),
      ok = maybe_set_kastle_param(?KASTLE_SSL_CACERTFILE, ssl, cacertfile, ?l2l),
      ok = maybe_set_kastle_param(?KASTLE_SSL_CERTFILE, ssl, certfile, ?l2l),
      ok = maybe_set_kastle_param(?KASTLE_SSL_KEYFILE, ssl, keyfile, ?l2l),
      ok = maybe_set_kastle_param(?KASTLE_SSL_VERIFY, ssl, verify, ?l2a),
      ok = maybe_set_kastle_param(?KASTLE_SSL_FAIL_IF_NO_PEER_CERT, ssl,
                                  fail_if_no_peer_cert, ?l2a);
    {ok, false} ->
      ok
  end,
  ok.

maybe_set_kafka_endpoints(false) ->
  ok;
maybe_set_kafka_endpoints(HostsStr) when is_list(HostsStr) ->
  Endpoints = [{Host, 9092} || Host <- string:tokens(HostsStr, ",")],
  BrodClients0 = application:get_env(brod, clients),
  ClientConfig0 = proplists:get_value(?BROD_CLIENT, BrodClients0),
  ClientConfig = lists:keystore(endpoints, 1, ClientConfig0, {endpoints, Endpoints}),
  BrodClients = lists:keystore(kastle_kafka_client, 1,
                               BrodClients0, {?BROD_CLIENT, ClientConfig}),
  application:set_env(brod, clients, BrodClients).

maybe_set_kastle_param(EnvVarName, Section, Param, ConvertFun) ->
  EnvVar = os:getenv(EnvVarName),
  case EnvVar of
    false -> ok;
    X     -> set_kastle_env(Section, Param, ConvertFun(X))
  end.

set_kastle_env(root, Param, Value) ->
  application:set_env(?APPLICATION, Param, Value);
set_kastle_env(Section, Param, Value) ->
  SectionConfig0 = application:get_env(kastle, Section),
  SectionConfig = lists:keystore(Param, 1, SectionConfig0, {Param, Value}),
  application:set_env(?APPLICATION, Section, SectionConfig).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
