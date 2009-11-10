-module(status_render).

-export([render_conns/0, render_queues/0, binaryse_widget/1]).
-export([escape/1, format_info_item/2, format_info/2, print/2]).

-include_lib("rabbit_common/include/rabbit.hrl").


%%--------------------------------------------------------------------

render_conns() ->
    ConnKeys = [pid, address, port, peer_address, peer_port, recv_oct, recv_cnt,
                send_oct, send_cnt, send_pend, state, 
                channels, user, vhost, timeout, frame_max],
    Conns = rabbit_networking:connection_info_all(),
    [[{Key, format_info_item(Key, Conn)} || Key <- ConnKeys] || Conn <- Conns].

render_queues() ->
    QueueKeys = [name, durable, auto_delete, arguments, pid, messages_ready, 
                 messages_unacknowledged, messages_uncommitted, messages,
                 acks_uncommitted, consumers, transactions, memory],

    Queues = lists:flatten([
                    [{Vhost, Queue} || Queue <- rabbit_amqqueue:info_all(Vhost)]
                        || Vhost <- rabbit_access_control:list_vhosts()]),
    [[{vhost, format_info(vhost, Vhost)}] ++ 
             [{Key, format_info_item(Key, Queue)} || Key <- QueueKeys]
                                                  || {Vhost, Queue} <- Queues].


binaryse_widget(A) ->
    case A of
        B when is_binary(B) -> B;
        F when is_float(F) -> print("~.3f", [F]);
        N when is_number(N) -> print("~p", [N]);
        L when is_list(L) ->
                case io_lib:printable_list(L) of
                    true -> L;
                    false -> lists:map(fun (C) -> binaryse_widget(C) end, L)
                end;
        {escape, Body} when is_binary(Body) orelse is_list(Body) ->
                print("~s", [Body]);
        {escape, Body} ->
                print("~w", Body);
        {memory, Mem} when is_number(Mem) ->
                print("~pMB", [trunc(Mem/1048576)]);
        {Form, Body} when is_list(Body) ->
                print(Form, Body);
        {Form, Body} ->
                print(Form, [Body]);
        P -> print("~p", [P])           %% shouldn't be used, better to escape.
    end.

print(Fmt, Val) when is_list(Val) ->
    escape(lists:flatten(io_lib:format(Fmt, Val)));
print(Fmt, Val) ->
    print(Fmt, [Val]).

print_no_escape(Fmt, Val) when is_list(Val) ->
    list_to_binary(lists:flatten(io_lib:format(Fmt, Val))).



escape(A) ->
    mochiweb_html:escape(A).

format_info_item(Key, Items) ->
    format_info(Key, proplists:get_value(Key, Items)).


format_info(Key, Value) ->
    case Value of
        #resource{name = Name} ->       %% queue name
            Name;
        Value when Key =:= address; Key =:= peer_address andalso
                   is_tuple(Value) ->
            list_to_binary(inet_parse:ntoa(Value));
        Value when is_number(Value) ->  %% memory stats, counters
            Value;
        Value when is_binary(Value) ->  %% vhost, username
            Value;
        Value ->                        %% queue arguments
            print_no_escape("~w", [Value])
    end.

