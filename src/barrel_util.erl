%%% -*- erlang -*-
%%%
%%% This file is part of barrel released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_util).

-export([filter_props/2, filter_props/3,
         propmerge/2,
         fix_ip/1,
         ipv6_supported/0,
         require/1]).

%% @doc filter a list of properties and removed n
filter_props(Props, Allowed) ->
    filter_props(Props, Allowed, []).

filter_props([], _Allowed, Acc) ->
    lists:reverse(Acc);
filter_props([{K, _}=KV|Rest], Allowed, Acc) ->
    case lists:member(K, Allowed) of
        true ->
            filter_props(Rest, Allowed, [KV|Acc]);
        false ->
            filter_props(Rest, Allowed, Acc)
    end.


%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge(L1, L2) ->
    propmerge1(fun(_, V1, _) -> V1 end, L1, L2).

propmerge1(F, L1, L2) ->
	dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).


fix_ip(Opts) ->
    {Opts1, ParsedIp} = case proplists:get_value(ip, Opts) of
        undefined ->
            {[{ip, any}|Opts], any};
        any ->
            {Opts, any};
        Ip when is_tuple(Ip) ->
            {Opts, Ip};
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            {[{ip, IpTuple}|proplists:delete(ip, Opts)], IpTuple}
    end,

    case ParsedIp of
        any ->
            case ipv6_supported() of % IPv4, and IPv6 if supported
                true -> [inet, inet6 | Opts1];
                _ -> Opts1
            end;
        {_, _, _, _} -> % IPv4
            [inet | Opts1];
        {_, _, _, _, _, _, _, _} -> % IPv6
            [inet6 | Opts1]
    end.

ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.


%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Rest]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Rest).
