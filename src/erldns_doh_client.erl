%%%-------------------------------------------------------------------
%%% @author Taavi Talvik <taavi@uninet.ee>
%%% @copyright (C) 2018, Taavi Talvik
%%% @doc DNS over HTTP client
%%%
%%% @end
%%% Created : 24 Oct 2018 by Taavi Talvik <taavi@uninet.ee>
%%%-------------------------------------------------------------------
-module(erldns_doh_client).

-behaviour(gen_server).

-include("erldns.hrl").

%% API
-export([start_link/3]).
-export([query/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  server,
	  port,
	  query_string,
	  connection,
	  gun_monitor}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Server, Port, QueryString) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Server, Port, QueryString], []).

query(Query) ->
    gen_server:call(?SERVER, {query, Query}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Server, Port, QueryString]) ->
    process_flag(trap_exit, true),
    {ok, ConnPid} = gun:open(Server, Port),
    {ok, http2} = gun:await_up(ConnPid),
    MonitorRef = erlang:monitor(process, ConnPid),
    {ok, #state{ server = Server,
		 port = Port,
		 query_string = QueryString,
		 connection = ConnPid,
		 gun_monitor = MonitorRef
	   }}.

handle_call({query, _Query}, _From, #state{connection=ConnPid, query_string=QueryString} = State) ->
    Questions = [{dns_query,<<"www.delfi.ee">>,1,?DNS_TYPE_SOA}],
    % google supports ECS, Cloudflare does not
    DnsOpt = {dns_optrr,4096,0,0,false, [{dns_opt_ecs,inet,25,0,<<1,2,3,0>>}]},
    Message = #dns_message{id = dns:random_id(), qr = false, oc = ?DNS_OPCODE_QUERY,
			   aa = false, tc = false, rd = true, ra = false, ad = false, cd = false,
			   rc = ?DNS_RCODE_NOERROR,
			   qc = 1, anc = 0, auc = 0, adc = 0,
			   questions = Questions, 
			   answers = [],
			   authority = [], additional = []},
    Encoded = dns:encode_message(Message),
    % Jama = <<171,205,001,000,000,001,000,000,000,000,000,000,003,119,119,119,007,101,120,097,109,112,108,101,003,099,111,109,000,000,001,000,001>>,
    Body = Encoded,
    lager:debug("Encoded => ~p~n", [Encoded]),
    StreamRef = gun:post(ConnPid, QueryString, 
			 [{<<"content-type">>, "application/dns-message"},
			  {<<"accept">>, "application/dns-message"}],
			 Body),
    Reply = {ok, StreamRef},
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Msg, State]),
    {noreply, State}.

% Stream has finished sending
handle_info({gun_response, _ConnPid, _StreamRef, fin, _Status, _Headers} = Info, State) ->
    lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Info, State]),
    {noreply, State};
% We got response on stream, headers are here, get data
handle_info({gun_response, ConnPid, StreamRef, nofin, 200, _Headers} =  _Info, State) ->
    % lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Info, State]),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    DnsMessage = dns:decode_message(Body),
    lager:debug("Got DnsMessage => ~p~n", [DnsMessage]),
    {noreply, State};
% Gun process dying??
handle_info({'DOWN', _Mref, process, _ConnPid, _Reason} = Info, State) ->
    lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Info, State]),
    {noreply, State};
% Something else
handle_info(Info, State) ->
    lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("~s:~s(~p, ~p)~n", [?MODULE, ?FUNCTION_NAME, Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
