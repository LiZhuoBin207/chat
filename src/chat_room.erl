-module(chat_room).
-behaviour(gen_server).

-include("clientinfo.hrl").
-include("message.hrl").

-export([start_link/0,init/1,get_pid/2,broadcast_msg/1,logout/1]).
-export([handle_call/3, handle_info/2, handle_cast/2, code_change/3, terminate/2]).
-record(state, {}).

start_link() ->
    get_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    id_generator:start_link(),
    ets:new(clientinfo, [public,
			 ordered_set,
			 named_table, 
			 {keypos, #clientinfo.id}
			]),
    {ok, #state{}}.

handle_call({getpid, Id}, _From, State) ->
    {ok, Pid} = client_session:start_link(Id),
    {reply, Pid, State}
;
handle_call({remove_clientinfo, Ref}, _From, State) ->
    Key = Ref#clientinfo.id,
    ets:delete(clientinfo, Key),
    {reply, ok, State};

handle_call({sendmsg, Msg}, _From, State) ->
    Key = ets:first(clientinfo),
    io:format("fechint table key is ~p ~n", [Key]),
    send_msg(Key, Msg),
    {reply, ok, State};
handle_call(Msg,_From,State) ->
    io:format("-----------------------error--------Msg = ~p------------------", [Msg]),
    {reply, none, State}.

handle_info(_Request, State) ->
    io:format("----------------------unknow--------------------------"),
    {noreply, State}.

handle_cast(_From, _state) ->
    ok.

code_change(_OldVersion, State, _Ext) ->
    {ok, State}.

terminate(_From, _Reason) ->
    ok.

get_pid(_F, _S) ->
    id = id_generator:get_new_id(client),
    Pid = gen_server:call(?MODULE, {getpid, Id}),
    io:format("id generated ~w~n", [Id]),
    #clientinfo(id = Id, pid = Pid).
    

bind_pid(Record, Socket) ->
    io:format("bindint socket---~n"),
    case gen_tcp:controlling_process(Socket, Record#clientinfo.pid) of
	{error, Reason} ->
	    io:format("--------------bind socket error reason =~p---------------------",[Reason]);
	ok ->
	    NewRec = #clientinfo{id = Record#clientinfo.id, 
				 pid = Record#clientinfo.pid,
				 socket = Socket},
	    io:format("chat_room:insert record ~p~n", [NewRec]),
	    ets:insert(clientinfo, NewRec),
	    Pid =Record#clientinfo.pid,
	    io:format("client bind   ~n")
    end.
	    
generate_name() ->
    ok.

broadcast_msg(Msg) ->
    gen_server:call(?MODULE, {sendmsg, Msg}).

send_msg(Key, Msg)  ->
    case ets:lookup(clientinfo, Key) of
	[Record] ->
	    io:format("Record found ~p ~n", [Record]),
	    Pid = Record#clientinfo.pid,
	    io:format("send message to client session ~p ~n", [Pid]),
	    Pid ! {dwmsg, Msg},
	    Next = ets:next(clientinfo, Key),
	    send_msg(Next, Msg);
	[] ->
	    io:format("no clientinfo found ~n")
    end,
    ok;
send_msg([], _Msg) ->
    ok.

get_members(_From) ->
    ok.

set_info(_ClientInfo, _From) ->
    ok.

logout(Ref) ->
    gen_server:call(?MODULE, {remove_clientinfo, Ref}),
    ok.
