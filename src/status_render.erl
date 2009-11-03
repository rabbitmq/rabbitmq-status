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
    [[format_info_item(Key, Conn) || Key <- ConnKeys] || Conn <- Conns].

render_queues() ->
    QueueKeys = [name, durable, auto_delete, arguments, pid, messages_ready, 
                 messages_unacknowledged, messages_uncommitted, messages,
                 acks_uncommitted, consumers, transactions, memory],

    Queues = lists:flatten([
                    [{Vhost, Queue} || Queue <- rabbit_amqqueue:info_all(Vhost)]
                        || Vhost <- rabbit_access_control:list_vhosts()]),
    [[format_info(vhost, Vhost)] ++ 
                    [format_info_item(Key, Queue) || Key <- QueueKeys]
                                                  || {Vhost, Queue} <- Queues].


binaryse_widget(A) ->
    case A of
        B when is_binary(B) -> B;
        L when is_list(L) ->
                case io_lib:printable_list(L) of
                    true -> L;
                    false -> lists:map(fun (C) -> binaryse_widget(C) end, L)
                end;
        F when is_float(F) -> list_to_binary(lists:flatten(
                                                io_lib:format("~.3f", [F])));
        T when is_tuple(T) ->
                {Form, Body0} = T,
                Body  = case is_list(Body0) of 
                            false -> [Body0];
                            true -> Body0
                        end,
                list_to_binary(lists:flatten(io_lib:format(Form, Body)));
        P -> list_to_binary(lists:flatten(io_lib:format("~p", [P])))
    end.

print(Fmt, Val) when is_list(Val) ->
    lists:flatten(io_lib:format(Fmt, Val));
print(Fmt, Val) ->
    print(Fmt, [Val]).



escape(A) -> A.

format_info_item(Key, Items) ->
    format_info(Key, proplists:get_value(Key, Items)).


format_info(Key, Value) ->
    case Value of
        #resource{name = Name} ->
            print("~s", [Name]);
        Value when Key =:= address; Key =:= peer_address andalso
                   is_tuple(Value) ->
            inet_parse:ntoa(Value);
        Value when Key =:= recv_oct; Key =:= send_oct; Key =:= memory;
                    Key =:= send_pend ->
            if Value >= 1073741824 -> print("~pGB", [trunc(Value/1073741824)]);
               true                -> print("~pMB", [trunc(Value/1048576)])
            end;
        Value when is_pid(Value) ->
            atom_to_list(node(Value));
        Value when is_binary(Value) -> 
            escape(Value);
        Value when is_atom(Value) ->
             escape(atom_to_list(Value));
        Value -> 
            print("~w", [Value])
    end.

