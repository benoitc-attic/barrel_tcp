%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% initialize the table keeping infos on listeners. This table
    %% should be public so we will be able to monitor listeners already
    %% started if the server is stopped.
    barrel_server = ets:new(barrel_server, [ordered_set, public,
                                            named_table]),
	Server = {barrel_server, {barrel_server, start_link, []},
			permanent, 5000, worker, [barrel_server]},

    {ok, { {one_for_one, 10, 10}, [Server]} }.

