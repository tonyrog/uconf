%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Main 
%%% @end
%%% Created : 14 May 2022 by Tony Rogvall <tony@rogvall.se>

-module(uconf).

-export([start/0]).

start() ->
    application:ensure_all_started(uconf).
