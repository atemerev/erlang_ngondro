-module(notifier).
-author("atemerev").
-compile(export_all).

proc(init, P) ->
  {ok, P};

proc({notify, Spread}, P) ->
  {ok,{{"HTTP/1.1",200,"OK"},HDRS,Response}} =
    httpc:request(post,{"https://api02.ethercast.net/spread",
                 [{"Authorization", io_lib:format("Basic ~s",
                 [base64:encode_to_string(application:get_env(tic,user,<<"user:FitoJabaX">>))])}],
                 "application/json",jsone:encode(#{spread => Spread})},
                 [{ssl,[{versions,['tlsv1.2']}]}],[]),
  io:format("Notify: ~p~n",[jsone:decode(list_to_binary(Response))]),
  {reply, [], P};

proc(M,S) -> {reply, [], S}.
