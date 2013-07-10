%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT licens.
%%% See the NOTICE for more information.
%%%
-module(barrel_connections).

-behaviour(gen_server).

-record(state, {
        listener,
        age,
        reqs,
        reqs_by_age,
        status,
        graceful_timer}).


-export([start/0]).
-export([add_connection/2,
        stop/1,
        shutdown/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


add_connection(Pid, ConnPid) ->
    gen_server:cast(Pid, {add_connection, ConnPid}).

stop(Pid) ->
    gen_server:call(Pid, stop).

shutdown(Pid, Timeout) ->
    gen_server:call(Pid, {shutdown, Timeout}).

start() ->
    Listener = self(),
    case gen_server:start_link(?MODULE, [Listener], []) of
        {ok, Pid} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef}};
        Error ->
            Error
    end.

init([Listener]) ->
    {ok, #state{listener=Listener,
                age = 0,
                reqs = dict:new(),
                reqs_by_age = gb_trees:empty(),
                status = active}}.


handle_call({shutdown, Timeout}, _From, State) ->
    NewState = case Timeout of
        none ->
            State#state{status=stopping};
        _ ->
            {ok, TRef} = timer:send_after(Timeout, shutdown),
            State#state{status=stopping, graceful_timer=TRef}
    end,
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({add_connection, Pid}, #state{reqs=Reqs,
                                          reqs_by_age=ReqsByAge,
                                          age=Age}=State) ->
    MRef = erlang:monitor(process, Pid),
    NewReqs = dict:store(Pid, {Age, MRef}, Reqs),
    ReqsByAge1 =  gb_trees:enter(Age, {Pid, MRef}, ReqsByAge),
    {noreply, State#state{reqs=NewReqs,
                          reqs_by_age=ReqsByAge1,
                          age=Age+1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _, Pid, _}, #state{listener=ListenerPid,
                                               reqs=Reqs,
                                               reqs_by_age=ReqsByAge,
                                               status=Status}=State) ->
    case dict:find(Pid, Reqs) of
        {ok, {Age, _}} ->
            Reqs1 = dict:erase(Pid, Reqs),
            ReqsByAge1 = gb_trees:delete_any(Age, ReqsByAge),

            %% tell the listener the connection is down
            ListenerPid ! {req_down, self()},

            NbReqs = gb_trees:size(ReqsByAge1),
            NewState = State#state{reqs=Reqs1,
                                    reqs_by_age=ReqsByAge1},

            case Status of
                active ->
                    {noreply, NewState};
                stopping when NbReqs /= 0 ->
                    {noreply, NewState};
                _ ->
                    timer:cancel(NewState#state.graceful_timer),
                    {stop, NewState#state{graceful_timer=nil}}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(shutdown, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener=ListenerPid,
                          reqs_by_age=ReqsByAge}) ->
    Iterator = gb_trees:iterator(ReqsByAge),
    kill_connections(gb_trees:next(Iterator), ListenerPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

kill_connections(none, _ListenerPid) ->
    ok;
kill_connections({_Age, {Pid, MRef}, Iterator}, ListenerPid) ->
    try
        catch exit(Pid, kill),
        receive
            {'DOWN', MRef, _, _, _} ->
                ok
        end
    after
        erlang:demonitor(MRef)
    end,
    if pid /= nil ->
        ListenerPid ! {req_down, self()}
    end,
    kill_connections(gb_trees:next(Iterator), ListenerPid).
