%% Author: vvorobyov
%% Created: Mar 16, 2012
%% Description: TODO: Add description to disk_log_tool
-module(error_log_tool).

%%
%% Exported Functions
%%
-export([main/1]).
-export([cat/1, cat/2, visit_log/2, filter/2, print_log/3, print_zlist/3]).

-define(REPORT_SEPARATOR,"~n### ~p -----------------------------------------------------------------------~n").

main([_|_]=Args) ->
    [LogFilename|Options]=lists:reverse(Args),
    try error_log_tool:cat([ list_to_atom(Opt) || [$-,$-|Opt] <- Options],LogFilename)
    catch
        _:Reason ->
            io:format(standard_error,"error: ~p~n",[Reason]),
            halt(1)
    end;
main(_) ->
    usage().

usage() ->
    io:format(standard_error,"usage: error_log_cat OPTIONS LogFilename~n~n",[]),
    io:format(standard_error,"OPTIONS~n",[]),
    io:format(standard_error,"\t--sasl          - only sasl reports~n",[]),
    io:format(standard_error,"\t--sasl-error    - only sasl error reports~n",[]),
    io:format(standard_error,"\t--error         - all standard or sasl error reports~n",[]),
    io:format(standard_error,"\t--warn          - all standard or sasl error or warning reports~n",[]),
    halt(1).

%%
%% API Functions
%%

cat(LogFilename) ->
    cat([], LogFilename).

cat(Options, LogFilename) ->
    print_log(
        Options, 
        fun(IoList) ->
            io:format(standard_io, "~s",[IoList])
        end, 
        LogFilename).

print_log(Options, PrintFun, LogFilename) when is_function(PrintFun, 1) ->
    visit_log(
      fun(LogZList)->
        print_zlist(Options, PrintFun, LogZList)
      end, LogFilename).

print_zlist(Options, PrintFun, LogZList) when is_function(PrintFun, 1) ->
    LogZList1=filter(Options, 
                     zlists:ziph(zlists:seq(1, 1000000000000, 1), 
                                 LogZList)),
    LogZList2=zlists:map(
                fun({_N,{NTime,Evt}})->
                        {_N,{calendar:now_to_universal_time(NTime),Evt}}
                end, LogZList1),
    case lists:keyfind(max_limit, 1, Options) of
        {max_limit, Limit} -> LogZList3=zlists:take2(Limit, LogZList2);
        false          -> LogZList3=LogZList2
    end,
    zlists:foreach(
      fun(E) -> print_evt(PrintFun,E) end,
      LogZList3 ).

print_evt(PrintFun, {N, TaggedEvt}) ->
    PrintFun(io_lib:format(?REPORT_SEPARATOR, [N])),
    Report=error_log_tool_fmt:format_event(TaggedEvt),
    PrintFun(Report).

visit_log(Fun, LogFilename) when is_function(Fun, 1) ->
    {ok,Log}=disk_log:open(
               [{name,LogFilename},
                {file,LogFilename},
                {mode,read_only}]),
    try Z=zlists_disk_log:read(Log),
        Fun(Z)
    after
        disk_log:close(Log)
    end.

filter([]=_Options,LogZList) ->
    LogZList;
filter([Opt|Tail]=_Options,LogZList) ->
    case Opt of
        'all'  ->
            Z=LogZList;
        'sasl' ->
            Z=zlists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [supervisor_report,progress,crash_report]);
                   (_) -> false
                end, LogZList);
        'sasl-crash' ->
            Z=zlists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [crash_report]);
                   (_) -> false
                end, LogZList);
        'error' ->
            Z=zlists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_msg,error_report]);
                   (_) -> false
                end, LogZList);
        'warn' ->
            Z=zlists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_report,warning_report,warning_msg]);
                   (_) -> false
                end, LogZList);
        {nodes_inc, 'all'} ->
            Z=LogZList;
        {nodes_inc, Nodes} when is_list(Nodes) ->
            Z=zlists:filter(
                fun({_N,{_, {_, Pid, {_, _, _}} }})->
                        lists:member(node(Pid), Nodes);
                   (_) -> false
                end, LogZList);
        {nodes_exc, Nodes} when is_list(Nodes) ->
            Z=zlists:filter(
                fun({_N,{_, {_, Pid, {_, _, _}} }})->
                        not lists:member(node(Pid), Nodes);
                   (_) -> false
                end, LogZList);
        _ -> 
            Z=LogZList
    end,
    filter(Tail,Z).

%%
%% Local Functions
%%


