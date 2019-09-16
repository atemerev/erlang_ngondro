-module(notifier).
-author("atemerev").
-include_lib("n2o/include/n2o.hrl").
-include("../include/state.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, P};

proc({notify, Spread}, #pi{state = #notifier_state{last_notify = Last}} = P) ->
  {ok,{{"HTTP/1.1",200,"OK"}, _, Response}} =
  httpc:request(post,{"https://api02.ethercast.net/spread",
                 [{"Authorization", io_lib:format("Basic ~s",
                 [base64:encode_to_string(application:get_env(tic,user,<<"user:FitoJabaX">>))])}],
                 "application/json",jsone:encode(#{spread => Spread})},
                 [{ssl,[{versions,['tlsv1.2']}]}],[]),
  io:format("Notify: ~p~n", [jsone:decode(list_to_binary(Response))]),
  {reply, [], P};

proc(_, P) -> {reply, [], P}.
