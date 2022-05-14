%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%     Sup
%%% @end
%%% Created : 15 Okt 2020 by Tony Rogvall <tony@rogvall.se>

-module(uconf_sup).

-behaviour(supervisor).

%% external exports
-export([start_link/0, start_link/1, stop/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Args) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop() ->
    exit(normal).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init(Args) ->
    UConfDenon = {uconf_denon, {uconf_denon, start_link, [Args]},
		 permanent, 5000, worker, [uconf_denon]},
    {ok,{{one_for_all, 1, 10}, [UConfDenon]}}.

