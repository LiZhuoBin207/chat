-module(echatserver).


%% 熟悉Erlang的套接字编程，实现聊天室
%% 启动一个管理聊天的总进程，用于白村客户端的连接信息，并负责把没一个客户端的消息广播到所有的在线客户端
%% 服务器接受客户端连接 然后启动一条进程与他绑定，
%%
%%


%%  这个纯粹是启动项目的东西。

-export([start/0]).

start() ->
    chat_room:start_link(),
    chat_acceptor:start(3377),
    ok.
