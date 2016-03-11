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
        , allowed_methods/2
        , charsets_provided/2
        , content_types_accepted/2
        , content_types_provided/2
        , handle_json/2
        , handle_binary/2
        ]).

%%_* Includes ==================================================================
-include("kastle.hrl").

%%_* Records ===================================================================
-record(state, {}).

%%_* Macros ====================================================================
-define(TOPIC_REQ,     topic).
-define(PARTITION_REQ, partition).

-define(KAFKA_KEY_HEADER, <<"kafka-key">>).

%%_* cowboy handler callbacks ==================================================

-spec init({atom(), atom()}, cowboy_req:req(), _) ->
              {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _, _) ->
  {upgrade, protocol, cowboy_rest}.

%%_* cowboy rest callbacks =====================================================

-spec rest_init(cowboy_req:req(), _) -> {ok, cowboy_req:req(), #state{}}.
rest_init(Req, _) ->
  {ok, Req, #state{}}.

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
  {[ {{<<"application">>, <<"json">>, []}, handle_json}
   , {{<<"application">>, <<"binary">>, []}, handle_binary}], Req, State}.

-spec content_types_provided(cowboy_req:req(), #state{}) ->
                                {[_], cowboy_req:req(), #state{}}.
content_types_provided(Req, State) ->
  %% callback will be called for GET and HEAD only
  {[{{<<"application">>, <<"json">>, []}, none}], Req, State}.

-spec handle_json(cowboy_req:req(), #state{}) ->
                     {halt, cowboy_req:req(), #state{}}.
handle_json(Req0, State) ->
  {Topic, Req1} = cowboy_req:binding(?TOPIC_REQ, Req0),
  {Partition, Req2} = cowboy_req:binding(?PARTITION_REQ, Req1),
  {ok, Body, Req3} = cowboy_req:body(Req2),
  {ok, Req} =
    case do_handle_json(Topic,
                        validate_partition(Partition),
                        validate_body(Body)) of
      ok ->
        server_log(info, Req3, 204),
        cowboy_req:reply(204, Req3);
      {error, Error} ->
        server_log(error, Req3, 400, "error: ~p", [Error]),
        cowboy_req:reply(400, [], jiffy:encode(#{error => Error}), Req3);
      {error, Code, Error} ->
        server_log(error, Req3, Code, "error: ~p", [Error]),
        cowboy_req:reply(Code, [], jiffy:encode(#{error => Error}), Req3)
    end,
  {halt, Req, State}.

-spec handle_binary(cowboy_req:req(), #state{}) ->
                       {halt, cowboy_req:req(), #state{}}.
handle_binary(Req0, State) ->
  {Topic, Req1} = cowboy_req:binding(?TOPIC_REQ, Req0),
  {Partition, Req2} = cowboy_req:binding(?PARTITION_REQ, Req1),
  {Key, Req3} = cowboy_req:header(?KAFKA_KEY_HEADER, Req2, <<>>),
  {ok, Value, Req4} = cowboy_req:body(Req3),
  {ok, Req} =
    case do_handle_binary(Topic, Partition, Key, Value) of
      ok ->
        server_log(info, Req4, 204),
        cowboy_req:reply(204, Req4);
      {error, Error} ->
        server_log(error, Req4, 400, "error: ~p", [Error]),
        cowboy_req:reply(400, [], jiffy:encode(#{error => Error}), Req4);
      {error, Code, Error} ->
        server_log(error, Req4, Code, "error: ~p", [Error]),
        cowboy_req:reply(Code, [], jiffy:encode(#{error => Error}), Req4)
    end,
  {halt, Req, State}.

%%_* Internal functions ========================================================
server_log(Level, Req, ResponseCode) ->
  server_log(Level, Req, ResponseCode, "", []).

server_log(Level, Req, ResponseCode, ExtraFmt, ExtraArgs) ->
  Format = get_server_log_fmt_fun(ExtraFmt),
  Args = get_server_log_fmt_args_fun(Req, ResponseCode, ExtraArgs),
  do_log(Level, Format, Args).

do_log(Level, Format, Args) when is_list(Args) ->
  lager:log(Level, self(), Format, Args);
do_log(Level, Format, Args) when is_function(Format), is_function(Args) ->
  case should_log_or_trace(Level) of
    true ->
      lager:log(Level, self(), Format(), Args());
    false ->
      ok
  end.

should_log_or_trace(Level) ->
  {CurrentLevel, Traces} = lager_config:get(loglevel, {?LOG_NONE, []}),
  (lager_util:level_to_num(Level) band CurrentLevel) /= 0 orelse Traces /= [].

get_server_log_fmt_fun(ExtraFmt) ->
  fun() -> "~s ~s ~s ~B, user-agent: ~s, host: ~s, content-type: ~s, content-length: ~s, " ++ ExtraFmt end.

get_server_log_fmt_args_fun(Req, ResponseCode, ExtraArgs) ->
  fun() ->
      {Method, _} = cowboy_req:method(Req),
      {Path, _} = cowboy_req:path(Req),
      {Version, _} = cowboy_req:version(Req),
      {UserAgent, _} = cowboy_req:header(<<"user-agent">>, Req),
      {Host, _} = cowboy_req:header(<<"host">>, Req),
      {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
      {ContentLength, _} = cowboy_req:header(<<"content-length">>, Req),
      [Method, Path, Version, ResponseCode, UserAgent, Host, ContentType, ContentLength] ++ ExtraArgs
  end.

validate_partition(Partition) ->
  string:to_integer(binary_to_list(Partition)).

validate_body(Body) ->
  do_validate_body(catch jiffy:decode(Body, [return_maps])).

do_validate_body({error, _Whatever}) ->
  {error, <<"invalid json">>};
do_validate_body(Data) ->
  case jesse:validate(?KASTLE_JSON_SCHEMA, Data) of
    {ok, _} = Res ->
      Res;
    {error, JesseErrors} ->
      parse_jesse_errors(JesseErrors)
  end.

parse_jesse_errors([{data_invalid, _Schema, ErrorType, _Value, _Path}]) ->
  ErrorMsg = iolist_to_binary(io_lib:format("json schema validation failed: ~p", [ErrorType])),
  {error, ErrorMsg};
parse_jesse_errors([{schema_invalid, Schema, ErrorType}]) ->
  lager:error("Invalid schema: ~p, ~p", [Schema, ErrorType]),
  {error, 500, <<"error validating json, please contact service maintainers">>};
parse_jesse_errors(Other) ->
  lager:error("Unexpected jesse error(s): ~p", [Other]),
  {error, 500, <<"error validating json, please contact service maintainers">>}.

do_handle_json(_Topic, _Partition, {error, _Any} = Error) ->
  Error;
do_handle_json(_Topic, _Partition, {error, _Code, _Any} = Error) ->
  Error;
do_handle_json(_Topic, {error, no_integer}, _Data) ->
  {error, <<"invalid partition">>};
do_handle_json(Topic, {Partition, []}, {ok, Data}) when is_integer(Partition) ->
  Key = maps:get(?MESSAGE_KEY, Data),
  Value = maps:get(?MESSAGE_VALUE, Data),
  produce(Topic, Partition, Key, Value).

do_handle_binary(Topic, undefined, Key, Value) ->
  case produce(Topic, fun get_random_partition/4, Key, Value) of
    {error, 503, <<"infrastructure down">>} = Error ->
      %% try all available partitions
      %% we're random anyway
      {ok, PartitionsCnt} = brod_client:get_partitions_count(?BROD_CLIENT, Topic),
      try_partitions(Topic, lists:seq(0, PartitionsCnt-1), Key, Value, Error);
    {error, _, _} = Error ->
      Error;
    ok ->
      ok
  end;
do_handle_binary(_Topic, {error, no_integer}, _Key, _Value) ->
  {error, <<"invalid partition">>};
do_handle_binary(Topic, {Partition, []}, Key, Value) when is_integer(Partition) ->
  produce(Topic, Partition, Key, Value).

try_partitions(_Topic, [], _Key, _Value, Error) ->
  Error;
try_partitions(Topic, [P | Partitions], Key, Value, _Error) ->
  case produce(Topic, P, Key, Value) of
    {error, 503, <<"infrastructure down">>} = Error ->
      try_partitions(Topic, Partitions, Key, Value, Error);
    {error, _, _} = Error ->
      Error;
    ok ->
      ok
  end.

produce(Topic, Partition, Key, Value) ->
  Res = brod:produce_sync(?BROD_CLIENT, Topic, Partition, Key, Value),
  case Res of
    {error, topic_not_found} ->
      {error, 404, <<"topic not found">>};
    {error, {producer_not_found, _Topic}} ->
      {error, 404, <<"topic not found">>};
    {error, {producer_not_found, _Topic, _Partition}} ->
      {error, 404, <<"partition not found">>};
    {error, _} -> % client_down or producer_down
      {error, 503, <<"infrastructure down">>};
    ok ->
      ok
  end.

get_random_partition(_Topic, PartitionsCnt, _Key, _Value) ->
  {ok, crypto:rand_uniform(0, PartitionsCnt)}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
