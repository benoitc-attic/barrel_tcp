%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_listener).
-behaviour(gen_server).

-export([get_port/1,
         info/1, info/2,
         set_max_clients/2, get_max_clients/1,
         set_nb_acceptors/2, get_nb_acceptors/1]).


%% internal API
-export([start_link/1]).
-export([start_accepting/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-record(state, {socket,
                transport,
                transport_opts,
                nb_acceptors,
                acceptors = [],
                reqs,
                open_reqs = 0,
                max_clients=300000,
                sleepers=[],
                listener_opts,
                protocol}).


get_port(Ref) ->
    gen_server:call(Ref, get_port).

info(Ref) ->
    info(Ref, [ip, port, open_reqs, nb_acceptors, max_clients]).

info(Ref, Keys) ->
    gen_server:call(Ref, {info, Keys}).

set_max_clients(Ref, Nb) ->
    gen_server:call(Ref, {set_max_clients, Nb}).

get_max_clients(Ref) ->
    [{max_clients, Max}] = info(Ref, [max_connection]),
    Max.

set_nb_acceptors(Ref, Nb) ->
    gen_server:call(Ref, {set_nb_acceptors, Nb}).

get_nb_acceptors(Ref) ->
    [{nb_acceptors, Nb}] = info(Ref, [nb_acceptors]),
    Nb.



start_accepting(Ref) ->
    gen_server:call(Ref, start_accepting, infinity).

start_link([_, _, _, _, _, ListenerOpts] = Options) ->
    Ref = proplists:get_value(ref, ListenerOpts),
    case gen_server:start_link({local, Ref}, ?MODULE, Options, []) of
        {ok, Pid} ->
            ok = barrel_server:set_listener(Ref, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

init([NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts,
      ListenerOpts]) ->

    process_flag(trap_exit, true),

    {ok, Socket} = Transport:listen(TransOpts),

    %% launch acceptors
    Spawn = is_request_spawned(Protocol),
    Acceptors = [barrel_acceptor:start_link(self(), Transport, Socket,
                                            ListenerOpts,
                                            {Protocol, ProtoOpts, Spawn})
                 || _ <- lists:seq(1, NbAcceptors)],
    {ok, #state{socket = Socket,
                transport = Transport,
                transport_opts = TransOpts,
                acceptors = Acceptors,
                nb_acceptors = NbAcceptors,
                reqs = gb_trees:empty(),
                open_reqs = 0,
                max_clients = 300000,
                sleepers = [],
                listener_opts = ListenerOpts,
                protocol = {Protocol, ProtoOpts, Spawn}}}.


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

handle_call({set_max_clients, Nb}, _From, State) ->
    {reply, ok, State#state{max_clients=Nb}};

handle_call({set_nb_acceptors, Nb}, _From,  State) ->

    NewState = manage_acceptors(State#state{nb_acceptors=Nb}),

    {reply, ok, NewState};

handle_call(start_accepting, From, #state{open_reqs=NbReqs,
                                          max_clients=Max,
                                          sleepers=Sleepers}=State)
       when NbReqs =:= Max ->
    {no_reply, State#state{sleepers=[From, Sleepers]}};
handle_call(start_accepting, _From, State) ->
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({accepted, Pid}, #state{acceptors=Acceptors}=State) ->
    %% accept a request and start a new acceptor
    %%
    NewState = case lists:member(Pid, Acceptors) of
        true ->
            start_new_acceptor(accept_request(Pid, State));
        false ->
            %% acceptor isn't exited only monitor the spawned request
            %% process.
            monitor_request(Pid, State)
    end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _, Pid, _}, #state{reqs=Reqs,
                                               open_reqs=NbReqs}=State) ->
    State1 = case State#state.sleepers of
        [] -> State;
        [Sleeper | Rest] ->
            gen_server:reply(Sleeper, ok),
            State#state{sleepers=Rest}
    end,

    {noreply, State1#state{reqs=gb_trees:delete_any(Pid, Reqs),
                           open_reqs=NbReqs-1}};

handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    error_logger:error_msg("No more file descriptors, shutting down:
                           ~p~n", [?MODULE]),
    {stop, emfile, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:info_msg("request (pid ~p) unexpectedly crashed:~n~p~n",
                [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internals
%%
%%
monitor_request(Pid, #state{reqs=Reqs, open_reqs=NbReqs}=State) ->
    MRef = erlang:monitor(process, Pid),
    NewReqs = gb_trees:enter(Pid, MRef, Reqs),
    State#state{reqs=NewReqs, open_reqs=NbReqs+1}.


accept_request(Pid, #state{acceptors=Acceptors}=State) ->
    %% remove acceptor from the list of acceptor and increase state
    unlink(Pid),

    %% trap premature exit
    receive
        {'EXIT', Pid, _} ->
            true
    after 0 ->
            true
    end,

    %% remove the acceptor from the list and start to monitor it as a
    %% request.
    monitor_request(Pid, State#state{acceptors=lists:delete(Pid, Acceptors)}).

remove_acceptor(#state{acceptors=Acceptors, nb_acceptors=N}=State, Pid)
        when length(Acceptors) < N->
    NewPid = barrel_acceptor:start_link(self(), State#state.transport,
                                        State#state.socket,
                                        State#state.listener_opts,
                                        State#state.protocol),
    Acceptors1 = [NewPid | lists:delete(Pid, Acceptors)],
    State#state{acceptors = Acceptors1};
remove_acceptor(State, Pid) ->
    State#state{acceptors = lists:delete(Pid, State#state.acceptors)}.

start_new_acceptor(State) ->
    Pid = barrel_acceptor:start_link(self(), State#state.transport,
                                     State#state.socket,
                                     State#state.listener_opts,
                                     State#state.protocol),

    State#state{acceptors = [Pid | State#state.acceptors]}.

manage_acceptors(#state{nb_acceptors=N, acceptors=Acceptors}=State) ->
    AcceptorsLen = length(Acceptors),
    if N > AcceptorsLen ->
            NewAcceptors = spawn_acceptors(N - AcceptorsLen, State),
            State#state{acceptors=Acceptors ++ NewAcceptors};
        true ->
            NewAcceptors = murder_acceptors(lists:reverse(Acceptors),
                                            AcceptorsLen - N),
            State#state{acceptors=NewAcceptors}
    end.

spawn_acceptors(Nb, State) ->
    [barrel_acceptor:start_link(self(), State#state.transport,
                                State#state.socket,
                                State#state.listener_opts,
                                State#state.protocol)
     || _ <- lists:seq(1, Nb)].


murder_acceptors(Acceptors, 0) ->
    lists:reverse(Acceptors);
murder_acceptors([], _N) ->
    [];
murder_acceptors([Pid | Rest], N) ->
    exit(Pid, normal),
    murder_acceptors(Rest, N-1).

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
get_infos([open_reqs|Rest], IpPort, #state{open_reqs=NbReqs}=State, Acc) ->
    get_infos(Rest, IpPort, State, [{open_reqs, NbReqs}|Acc]);
get_infos([nb_acceptors|Rest], IpPort, #state{acceptors=Acceptors}=State,
         Acc) ->
    get_infos(Rest, IpPort, State, [{acceptors, length(Acceptors)}|Acc]);
get_infos([max_clients|Rest], IpPort, #state{max_clients=Max}=State,
        Acc) ->
    get_infos(Rest, IpPort, State, [{max_clients, Max} | Acc]).


is_request_spawned(Handler) ->
    _ = code:ensure_loaded(Handler),

    case erlang:function_exported(Handler, start_link, 4) of
        true ->
            true;
        false ->
            case erlang:function_exported(Handler, init, 4) of
                true ->
                    false;
                false ->
                    io:format("error bad proto~n", []),
                    throw({error, bad_proto})
            end
    end.
