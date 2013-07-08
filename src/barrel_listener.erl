%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_listener).
-behaviour(gen_server).

-export([start_listener/6, start_listener/7,
         stop_listener/1,
         get_port/1,
         info/1, info/2]).


-export([start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-record(state, {socket,
                transport,
                transport_opts,
                acceptors,
                open_reqs,
                listener_opts,
                protocol}).


start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtocolOpts) ->
        start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
                       ProtocolOpts, []).

start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol,
               ProtoOpts, ListenerOpts0) ->
    _ = code:ensure_loaded(Transport),
    case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
            ListenerOpts = [{ref, Ref} | ListenerOpts0],
            supervisor:start_child(barrel_sup,
                                   child_spec(Ref, [NbAcceptors, Transport,
                                                     TransOpts, Protocol,
                                                     ProtoOpts,
                                                     ListenerOpts]))
    end.

stop_listener(Ref) ->
    case supervisor:terminate_child(barrel_sup, Ref) of
        ok ->
            supervisor:delete_child(barrel_sup, Ref);
        Error ->
            Error
    end.

get_port(Ref) ->
    gen_server:call(Ref, get_port).

info(Ref) ->
    info(Ref, [ip, port, open_reqs, nb_acceptors]).

info(Ref, Keys) ->
    gen_server:call(Ref, {info, Keys}).

%% @doc return a child spec suitable for embeding your listener in the
%% supervisor
child_spec(Ref, Options) ->
    {Ref, {barrel_listener, start_link, [Options]},
            permanent, 5000, worker, [Ref]}.

start_link([_, _, _, _, _, ListenerOpts] = Options) ->
    Ref = proplists:get_value(ref, ListenerOpts),
    gen_server:start_link({local, Ref}, ?MODULE, Options, []).

init([NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts,
      ListenerOpts]) ->

    process_flag(trap_exit, true),

    {ok, Socket} = Transport:listen(TransOpts),

    %% launch acceptors
    Acceptors = [barrel_acceptor:start_link(self(), Transport, Socket,
                                            ListenerOpts,
                                            {Protocol, ProtoOpts})
                 || _ <- lists:seq(1, NbAcceptors)],
    {ok, #state{socket = Socket,
                transport = Transport,
                transport_opts = TransOpts,
                acceptors = Acceptors,
                open_reqs = 0,
                listener_opts = ListenerOpts,
                protocol = {Protocol, ProtoOpts}}}.


handle_call(get_port, _From, #state{socket=S, transport=Transport}=State) ->
    case Transport:sockname(S) of
        {ok, {_, Port}} ->
            {reply, {ok, Port}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({info, Keys}, _From, State) ->
    Infos = get_infos(Keys, State),
    {reply, Infos, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(accepted, State) ->
    NewState = start_new_acceptor(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    lager:error("No more file descriptors, shutting down~n", []),
    {stop, emfile, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};

handle_info({'EXIT', Pid, Reason}, State) ->
    lager:error("request (pid ~p) unexpectedly crashed:~n~p~n",
                [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internals

remove_acceptor(State, Pid) ->
    State#state{acceptors = lists:delete(Pid, State#state.acceptors),
                open_reqs = State#state.open_reqs - 1}.

start_new_acceptor(State) ->
    Pid = barrel_acceptor:start_link(self(), State#state.transport,
                                     State#state.socket,
                                     State#state.listener_opts,
                                     State#state.protocol),

    State#state{acceptors = [Pid | State#state.acceptors],
                open_reqs = State#state.open_reqs + 1}.

get_infos(Keys, #state{transport=Transport, socket=Socket}=State) ->
    IpPort = case Transport:sockname(Socket) of
        {ok, IpPort1} ->
            IpPort1;
        Error ->
            {{error, Error}, {error, Error}}
    end,
    get_infos(Keys, IpPort, State, []).

get_infos([], _IpPort, _State, Acc) ->
    lists:reverse(Acc);
get_infos([ip|Rest], {Ip, _}=IpPort, State, Acc) ->
    get_infos(Rest, IpPort, State, [{ip, Ip}|Acc]);
get_infos([port|Rest], {_, Port}=IpPort, State, Acc) ->
    get_infos(Rest, IpPort, State, [{port, Port}|Acc]);
get_infos([open_reqs|Rest], IpPort, #state{open_reqs=OpenReqs}=State,
         Acc) ->
    get_infos(Rest, IpPort, State, [{open_reqs, OpenReqs}|Acc]);
get_infos([nb_acceptors|Rest], IpPort, #state{acceptors=Acceptors}=State,
         Acc) ->
    get_infos(Rest, IpPort, State, [{acceptors, length(Acceptors)}|Acc]).
