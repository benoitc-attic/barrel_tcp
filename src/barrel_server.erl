-module(barrel_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([set_listener/2, get_listener/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-define(TAB, ?MODULE).
-record(state, {monitors = [] }).


%% @doc Set the listener associated to the ref.
-spec set_listener(barrel:ref(), pid()) -> ok.
set_listener(Ref, Pid) ->
	true = gen_server:call(?MODULE, {set_listener, Ref, Pid}),
	ok.

%% @doc Return the listener associated to the ref.
-spec get_listener(ranch:ref()) -> pid().
get_listener(Ref) ->
	ets:lookup_element(?TAB, {listeners, Ref}, 2).

%% @doc Start the barell_sever.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} ||
		[Ref, Pid] <- ets:match(?TAB, {{conns_sup, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.


handle_call({set_listener, Ref, Pid}, _From,
            #state{monitors=Monitors}=State) ->
    case ets:insert_new(?TAB, {{listeners, Ref}, Pid}) of
		true ->
			MRef = erlang:monitor(process, Pid),
			{reply, true,
				State#state{monitors=[{{MRef, Pid}, Ref}|Monitors]}};
		false ->
			{reply, false, State}
	end;

handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MRef, process, Pid, _},
            State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MRef, Pid}, 1, Monitors),
	true = ets:delete(?TAB, {conns_sup, Ref}),
	Monitors2 = lists:keydelete({MRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
