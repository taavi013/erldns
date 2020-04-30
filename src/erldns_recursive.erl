%%%-------------------------------------------------------------------
%%% @author Taavi Talvik <taavi@uninet.ee>
%%% @copyright (C) 2018, Taavi Talvik
%%% @doc Recursively resolve a DNS query
%%%
%%% @end
%%% Created :  2 Oct 2018 by Taavi Talvik <taavi@uninet.ee>
%%%-------------------------------------------------------------------
-module(erldns_recursive).

-behaviour(gen_server).

-include_lib("dns_erlang/include/dns.hrl").
-include("erldns.hrl").

-export([start_link/0, resolve/3]).

% Gen server hooks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

%% @doc Resolve the question in the message
-spec resolve(Messaga :: dns:message(), AuthorityRecords :: [dns:rr()], Host :: dns:ip()) -> dns:message().
resolve(Message, AuthorityRecords, Host) ->
    resolve(Message, AuthorityRecords, Host, Message#dns_message.questions).

%% There were no questions in the message so just return it.
-spec resolve(dns:message(), [dns:rr()], dns:ip(), dns:questions() | dns:query()) -> dns:message().
resolve(Message, _AuthorityRecords, _Host, []) -> Message;
%% There is one question in the message; resolve it.
resolve(Message, AuthorityRecords, Host, [Question]) -> resolve(Message, AuthorityRecords, Host, Question);
%% Resolve the first question. Additional questions will be thrown away for now.
resolve(Message, AuthorityRecords, Host, [Question|_]) -> resolve(Message, AuthorityRecords, Host, Question);

%% Start the resolution process on the given message
resolve(Message, AuthorityRecords, Host, Question) ->
    lager:debug("Recursive resolution: Message => ~p~n", [Message]),
    lager:debug("Recursive resolution: AuthorityRecords => ~p~n", [AuthorityRecords]),
    lager:debug("Recursive resolution: Host => ~p~n", [Host]),
    lager:debug("Recursive resolution: Question => ~p~n", [Question]),
    gen_server:call(?MODULE, {resolve, Message, AuthorityRecords, Host, Question}),
    Message#dns_message{ra = false, ad = false, cd = false, rc = ?DNS_RCODE_REFUSED}.

%% @doc Start recursive resolver
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks
init([]) ->
    lager:info("Starting ~p", [?MODULE]),
    {ok, #state{}}.

handle_call({resolve, Message, AuthorityRecords, Host, Question} = Msg, _, State) ->
    lager:debug("Got => ~p, State => ~p", [Msg, State]),
    {reply, {error, not_implemented}, State}.

handle_cast(_, State) ->
  {noreply, State}.
handle_info(_, State) ->
  {noreply, State}.
terminate(_, _) ->
  ok.
code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.
