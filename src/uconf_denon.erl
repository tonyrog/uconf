%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Denon API
%%% @end
%%% Created : 23 Jun 2019 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(uconf_denon).

-behaviour(gen_server).

%% API
-export([start/1]).
-export([start_link/1]).

-export([power/1]).
-export([user/1]).
-export([mute/1]).
-export([master_volume/1]).
-export([channel_volume/2]). 
-export([input_source/1]).
-export([get_param/1]).
-export([get_all_params/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-type channel_name() ::
	front_left | front_right | center | subwoofer |
	surround_left | surround_right |
	surround_back_left | surround_back_right |
	surround_back.

-define(SERVER, ?MODULE).

-define(dbg(F,A), io:format("debug: " F , A)).
-define(warn(F,A), io:format("warn: " F , A)).

-record(state, {
		s,           %% socket
		ip,          %% ip address
		port = 23,   %% telnet port 
		model,
		buf = [],
		params = #{}  %% current state
	       }).

-define(MAP(Key,Value),
	Key => Value,
	Value => Key).

input_source_() ->
    #{ 
       ?MAP(phono, "PHONO"),
       ?MAP(cd,    "CD"),
       ?MAP(tuner, "TUNER"),
       ?MAP(dvd,   "DVD"),
       ?MAP(vdp,   "VDP"),
       ?MAP(tv,    "TV"),
       ?MAP(dbs,   "DBS"),
       ?MAP(vcr_1, "VCR-1"),
       ?MAP(vcr_2, "VCR-2"),
       ?MAP(vcr_3, "VCR-3"),
       ?MAP(v_aux, "V.AUX"),
       ?MAP(cdr_tape, "CDR/TAPE"),
       ?MAP(auxnet, "AUXNET"),
       ?MAP(auxipod, "AUXIPOD"),
       ?MAP(query, "?")
     }.
input_source_(Source) ->
    maps:get(Source, input_source_()).

-spec channel_name_() -> #{ channel_name() => string(),
			    string() => channel_name() }.

channel_name_() ->
    #{
      ?MAP(front_left, "FL"),
      ?MAP(front_right, "FR"),
      ?MAP(center, "C"),
      ?MAP(subwoofer, "SW"),
      ?MAP(surround_left, "SL"),
      ?MAP(surround_right, "SR"),
      ?MAP(surround_back_left,"SBL"),
      ?MAP(surround_back_right, "SBR"),
      ?MAP(surround_back, "SB")
     }.
channel_name_(Channel) ->
    maps:get(Channel, channel_name_()).

-record(range,
	{
	 min = 0,
	 max = 99,
	 zdb = 80,
	 off
	}).

channel_range_() ->
    #{ 
       master      => #range{min=0,  max=99, zdb=80},
       front_left  => #range{min=38, max=62, zdb=50},
       front_right => #range{min=38, max=62, zdb=50},
       center      => #range{min=38, max=62, zdb=50},
       subwoofer   => #range{min=38, max=62, zdb=50, off=0},
       surround_left  => #range{min=38, max=62, zdb=50},
       surround_right => #range{min=38, max=62, zdb=50},
       surround_back_left  => #range{min=38, max=62, zdb=50},
       surround_back_right => #range{min=38, max=62, zdb=50},
       surround_back => #range{min=0, max=99, zdb=50}
     }.
channel_range_(Channel) when is_atom(Channel) ->
    maps:get(Channel, channel_range_()).

%% FIXME: delay 4 seconds after power on!
power(on) ->      command("PW", ["ON"]);
power(standby) -> command("PW", ["STANDBY"]);
power(query) -> command("PW", ["?"]).

user(0) -> command("MS", ["USER1"]);
user(1) -> command("MS", ["USER1"]);
user(2) -> command("MS", ["USER2"]);
user(3) -> command("MS", ["USER3"]);
user(query) -> command("MS", ["?"]).
    
mute(on) -> command("MU", ["ON"]);
mute(off) -> command("MU", ["OFF"]);
mute(query) -> command("MU", ["?"]).

-spec master_volume(Vol :: 0..99) -> ok.
master_volume(Vol) ->
    R = channel_range_(master),
    command("MV", [volume_(Vol,R)]).

-spec channel_volume(Channel :: channel_name(), Vol :: integer()) ->
	  ok.
channel_volume(Channel, Vol) when is_atom(Channel) ->
    R = channel_range_(Channel),
    CH = channel_name_(Channel),
    command("CV", [CH, volume_(Vol,R)]).

input_source(Source) when is_atom(Source) ->
    SI = input_source_(Source),
    command("SI", [SI]).

volume_(up,_R) -> "UP";
volume_(down,_R) -> "DOWN";
volume_(query,_R) -> "?";
volume_(Level,#range{min=Min,max=Max,off=Off}) 
  when Level >= Min, Level =< Max;
       Level =:= Off ->
    %% fixme handle float Db values! 
    %% handle three digit levels....
    tl(integer_to_list(100+Level)).

command(Cmd, Params) ->
    gen_server:call(?SERVER, {command, Cmd, Params}).

%% Get parameter value
get_param(Name) ->
    gen_server:call(?SERVER, {get_param, Name}).

%% Get paramter map
get_all_params() ->
    gen_server:call(?SERVER, get_all_params).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Opts::[term()]) -> {ok, Pid :: pid()} |
		      {error, Error :: {already_started, pid()}} |
		      {error, Error :: term()} |
		      ignore.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec start(Opts::[term()]) -> {ok, Pid :: pid()} |
		 {error, Error :: {already_started, pid()}} |
		 {error, Error :: term()} |
		 ignore.
start(Opts) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init(Args0) ->
    Args = Args0 ++ application:get_all_env(uconf),
    process_flag(trap_exit, true),
    IP = proplists:get_value(ip, Args),
    Port = proplists:get_value(port, Args, 23),
    Model = proplists:get_value(model, Args),
    case gen_tcp:connect(IP, Port, []) of
	{ok,S} ->
	    %% send query commands 
	    send(S, "PW", ["?"]),
	    timer:sleep(200),
	    send(S, "MV", ["?"]),
	    timer:sleep(200),
	    send(S, "CV", ["?"]),
	    timer:sleep(200),
	    send(S, "MU", ["?"]),
	    timer:sleep(200),
	    send(S, "SI", ["?"]),
	    timer:sleep(200),
	    %% ...
	    send(S, "MS", ["USER", "?"]),
	    timer:sleep(200),

	    {ok, #state{ s=S, ip=IP, port=Port, model=Model }};
	Error ->
	    %% fixme: retry!
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.

handle_call({command,Cmd,Ps}, _From, State) ->
    Reply = send_command(State, Cmd, Ps),
    {reply, Reply, State};

handle_call({get_param,Name}, _From, State) ->
    P = State#state.params,
    {reply, maps:get(Name, P, undefined), State};
handle_call(get_all_params, _From, State) ->
    {reply, State#state.params, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({tcp,S,Data}, State) when S =:= State#state.s ->
    ?dbg("Tcp: ~s\n", [Data]),
    State1 = event(State,Data),
    {noreply, State1};
handle_info({tcp_closed,S,Data}, State) when S =:= State#state.s ->
    ?dbg("Close: ~s\n", [Data]),
    {stop, closed, State};

handle_info(_Info, State) ->
    ?warn("Got: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

event(State, Data) ->
    Buf = State#state.buf ++ Data,
    event_buf(State#state { buf = "" }, Buf).

event_buf(State, Buf) ->
    case string:chr(Buf, $\r) of
	0 -> 
	    State#state { buf = Buf };
	I ->
	    {Line,[$\r|Buf1]} = lists:split(I-1,Buf),
	    State1 = event_line(State#state{buf=Buf1}, Line),
	    event_buf(State1, Buf1)
    end.

event_line(State, Line=[C1,C2|Ps]) ->
    ?dbg("Got event: ~p\n", [Line]),
    P1 = event_param([C1,C2],string:tokens(Ps, " "), State#state.params),
    State#state { params = P1 }.

event_param("PW", Pow, P) ->
    case Pow of
	["ON"] ->
	    P#{ power => on };
	["STANDBY"] ->
	    P#{ power => standby };
	_ ->
	    P
    end;
event_param("MV", [Vol], P) ->
    try list_to_integer(Vol) of
	V -> 
	    P#{ master_volume => V }
    catch
	error:_ ->
	    P
    end;
event_param("MV", ["MAX",Vol], P) ->
    try list_to_integer(Vol) of
	V -> 
	    P#{ master_volume_max => V }
    catch
	error:_ ->
	    P
    end;
event_param("CV", [CH,Vol], P) ->
    try list_to_integer(Vol) of
	V ->
	    ChannelName = channel_name_(CH),
	    P#{ {channel_volume, ChannelName} => V }
    catch
	error:_ ->
	    P
    end;
event_param("MU", [ONOFF], P) ->
    case ONOFF of
	"ON"  -> P#{ mute => true };
	"OFF" ->  P#{ mute => false };
	_ -> P
    end;
event_param("MS", ["USER0"], P) ->
    P#{ user => 0 };
event_param("MS", ["USER1"], P) ->
    P#{ user => 1 };
event_param("MS", ["USER2"], P) ->
    P#{ user => 2 };
event_param("MS", ["USER3"], P) ->
    P#{ user => 3 };
event_param("SI", [IN], P) ->
    Source = input_source_(IN),
    P#{ input_source => Source };
event_param(Event, Params, P) ->
    ?warn("unknown event ~s ~p\n", [Event, Params]),
    P.

send_command(State, Cmd, Ps) ->
    send(State#state.s, Cmd, Ps).

send(Socket, Cmd, Ps) ->
    Buf = [Cmd,lists:join(" ", Ps)],
    ?dbg("SEND command: ~s\n", [Buf]),
    gen_tcp:send(Socket, [Buf,"\r"]).    
