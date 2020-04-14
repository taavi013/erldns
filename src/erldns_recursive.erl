%%%-------------------------------------------------------------------
%%% @author Taavi Talvik <taavi@uninet.ee>
%%% @copyright (C) 2018, Taavi Talvik
%%% @doc Recursively resolve a DNS query
%%%
%%% @end
%%% Created :  2 Oct 2018 by Taavi Talvik <taavi@uninet.ee>
%%%-------------------------------------------------------------------
-module(erldns_recursive).

-include_lib("dns_erlang/include/dns.hrl").
-include("erldns.hrl").

-export([resolve/3]).

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
    Message#dns_message{ra = false, ad = false, cd = false, rc = ?DNS_RCODE_REFUSED}.
