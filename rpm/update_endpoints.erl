-module(update_endpoints).
-mode(compile).
-compile(debug_info).

main([HostsStr, SysConfigPath]) ->
  Endpoints = [{Host, 9092} || Host <- string:tokens(HostsStr, ",")],
  {ok, [Config0]} = file:consult(SysConfigPath),
  BrodConfig0 = proplists:get_value(brod, Config0),
  BrodClients0 = proplists:get_value(clients, BrodConfig0),
  ClientConfig0 = proplists:get_value(kastle_kafka_client, BrodClients0),
  ClientConfig = lists:keystore(endpoints, 1, ClientConfig0, {endpoints, Endpoints}),
  BrodClients = lists:keystore(kastle_kafka_client, 1, BrodClients0, {kastle_kafka_client, ClientConfig}),
  BrodConfig = lists:keystore(clients, 1, BrodConfig0, {clients, BrodClients}),
  Config = lists:keystore(brod, 1, Config0, {brod, BrodConfig}),
  ok = file:write_file(SysConfigPath, io_lib:format("~p.~n", [Config])),
  {ok, _} = file:consult(SysConfigPath),
  ok;
main(_) ->
  io:format("\n"),
  io:format("Rewrites kafka endpoints in kastle sys.conifg\n"),
  io:format("\n"),
  io:format("  Usage: escript update_endpoints.erl "
           "kafka1.domain.net,kafka2.domain.net,kafka3.domain.net "
           "/etc/kastle/sys.config\n").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
