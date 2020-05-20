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

%% Supress some warnings/errors
-compile([{nowarn_unused_function, [{rewrite_ttl,2},
                                    {google_query,0}
                                    ]
          }]).

% Gen server hooks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(DNS_PORT, 53).

-record(state, {
                upstream_resolver_socket,
                from,
                record_defs
               }).

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
    {ok, Result} = gen_server:call(?MODULE, {resolve, Message, AuthorityRecords, Host, Question}, infinity),
    lager:debug("RESULT => ~p", [Result]),
    Result.

%% @doc Start recursive resolver
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks
init([]) ->
    lager:info("Starting ~p, XXXXXX ~p", [?MODULE, file:get_cwd()]),
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    {ok, RecordDefs} = pp_record:read("../../apps/dns_erlang/include/dns_records.hrl"),
    {ok, #state{upstream_resolver_socket = Socket,
                record_defs = RecordDefs}}.

% resolve, original query in variable Message
handle_call({resolve, Message, _AuthorityRecords, _Host, _Question} = _Msg, From, State) ->
    MessageToSend = Message#dns_message{qr=false,
                                        aa=false,
                                        tc=false,
                                        rd=true,
                                        ra=false,
                                        ad=true,
                                        cd=false,
                                        qc = 1,
                                        additional=[#dns_optrr{}]
                                       },
    Packet = dns:encode_message(MessageToSend),
    lager:debug("Packet => ~p", [Packet]),
    Decoded = dns:decode_message(Packet),
    Nice = pp_record:print(Decoded, State#state.record_defs),
    lager:debug("Nice to send => ~s", [Nice]),
    ok = gen_udp:send(State#state.upstream_resolver_socket, "1.1.1.1", ?DNS_PORT, Packet),
    %ok = gen_udp:send(State#state.upstream_resolver_socket, "8.8.8.8", ?DNS_PORT, Packet),
    {noreply, State#state{from=From}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _IP, _InPortNo, Packet} = Info, State) ->
    lager:debug("Got info => ~p", [Info]),
    Decoded = dns:decode_message(Packet),
    lager:debug("Decoded raw => ~p", [Decoded]),
    Nice = pp_record:print(Decoded, State#state.record_defs),
    lager:debug("Decoded Response Message => ~s", [Nice]),
    gen_server:reply(State#state.from, {ok, Decoded}),
    cache_response(Decoded),
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Got info2 => ~p", [Info]),
    {noreply, State}.

terminate(_, _) ->
    ok.
code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.

%% @doc For testing purpose ensure that TTL is always 120
rewrite_ttl(Packet, NewTtl) ->
    Answers = Packet#dns_message.answers,
    NewAnswers = lists:map( fun(R) when is_record(R, dns_rr)->
                                    R#dns_rr{ttl = NewTtl};
                               (X) ->
                                    X
                            end, Answers),
    Packet#dns_message{answers = NewAnswers}.

google_query() ->
    X = [
    16#1e,16#e3,16#01,16#20,16#00,16#01,16#00,16#00,16#00,16#00,16#00,16#01,16#02,16#6e,16#73,16#06,
    16#75,16#6e,16#69,16#6e,16#65,16#74,16#02,16#65,16#65,16#00,16#00,16#01,16#00,16#01,16#00,16#00,
    16#29,16#10,16#00,16#00,16#00,16#00,16#00,16#00,16#00],
    Y = list_to_binary(X),
    Decoded = dns:decode_message(Y),
    Decoded.

%% Put response message to cache
cache_response(Message) ->
    erldns_packet_cache:put({Message#dns_message.questions, hd(Message#dns_message.additional)}, Message),
    Message.
