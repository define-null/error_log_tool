%% Author: vvorobyov
%% Created: Mar 16, 2012
%% Description: TODO: Add description to disk_log_tool
-module(error_log_tool).

%%
%% Exported Functions
%%
-export([main/1]).
-export([cat/1, cat/2, visit_log/2, filter/2, print_log/3, print_zlist/3]).
-export([worker_fun/4]).

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
    print_log(Options,
              fun(IoList) ->
                      io:format(standard_io, "~s",[IoList])
              end, 
              LogFilename).

print_log(Options, PrintFun, LogFilename) when is_function(PrintFun, 1) ->
    visit_log( fun(LogZList)->
                       print_zlist(Options, PrintFun, LogZList)
               end, LogFilename).

print_zlist(Options, PrintFun, LogZList) when is_function(PrintFun, 1) ->
    LogZList1 = zlists:ziph(zlists:seq(1, 1000000000000, 1), LogZList),

    BunchedList = bunch_zlist(100, LogZList1),
    Self = self(),
    Reader = spawn(fun() ->
                           print_work(1, PrintFun, undefined),
                           Self ! ok end),
    
    _WorkMan= spawn(fun() -> worker_manager(Reader,
                                           erlang:system_info(logical_processors),
                                           BunchedList,
                                           Options) end),
    receive
        ok ->
            ok
    end.
                           
    
    
print_evt(PrintFun, {N, Report}) ->
    PrintFun(io_lib:format(?REPORT_SEPARATOR, [N])),
    PrintFun(Report).

visit_log(Fun, LogFilename) when is_function(Fun, 1) ->
    {ok,Log}=disk_log:open([{name,LogFilename}, {file,LogFilename}, {mode,read_only}]),
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
            Z=lists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [supervisor_report,progress,crash_report]);
                   (_) -> false
                end, LogZList);
        'sasl-crash' ->
            Z=lists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [crash_report]);
                   (_) -> false
                end, LogZList);
        'error' ->
            Z=lists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_msg,error_report]);
                   (_) -> false
                end, LogZList);
        'warn' ->
            Z=lists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_report,warning_report,warning_msg]);
                   (_) -> false
                end, LogZList);
        {nodes_inc, 'all'} ->
            Z=LogZList;
        {nodes_inc, Nodes} when is_list(Nodes) ->
            Z=lists:filter(
                fun({_N,{_, {_, Pid, {_, _, _}} }})->
                        lists:member(node(Pid), Nodes);
                   (_) -> false
                end, LogZList);
        {nodes_exc, Nodes} when is_list(Nodes) ->
            Z=lists:filter(
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
print_work(Num, _, Num) ->
    ok;

print_work(Num, PrintFun, undefined) ->
    receive
        {max, Num} ->
            ok;
        {max, Last} ->
            print_work(Num, PrintFun, Last);
        {Pid, Num, Work} ->
            Pid ! ok,
            [ print_evt(PrintFun, {N, W}) || {N,W} <- Work ],
            print_work(Num + 1, PrintFun, undefined)
    end;

print_work(Num, PrintFun, Last) ->
    receive
        {Pid, Num, Work} ->
            Pid ! ok,
            [ print_evt(PrintFun, {N, W}) || {N,W} <- Work ],
            print_work(Num + 1, PrintFun, Last)
    end.            
    

worker(List, Options) ->
    lists:map( fun({_N,{NTime,Evt}})->
                       Event = {calendar:now_to_universal_time(NTime),Evt},
                       {_N, error_log_tool_fmt:format_event(Event)}
               end, filter(Options, List)).

%%------------------------------------------------------------------------------

worker_manager(Reader, ProcNum, Zlist, Options) ->
    process_flag(trap_exit, true),
    
    {Tasks, Later} = zlists:scroll(ProcNum, Zlist),
    lists:foldl(fun(Task, Acc) ->
                        proc_lib:spawn_link(fun() -> worker_fun(Reader, Acc, Task, Options) end),
                        Acc+1
               end, 1, Tasks),
   
    worker_manager_i(Reader, ProcNum, Later, Options).

worker_manager_i(Reader, Last, [], _) ->
    Reader ! {max, Last};

worker_manager_i(Reader, Last, [Bunch | Zlist], Opts) ->
    receive
        {'EXIT', _, _} ->
            ok
    end,
    proc_lib:spawn_link(fun() -> worker_fun(Reader, Last + 1, Bunch, Opts) end),
    worker_manager_i(Reader, Last + 1, Zlist(), Opts).

worker_fun(Reader, Num, Task, Options) ->
    Reader ! {self(), Num, worker(Task, Options)},
    receive
        ok ->
            ok
    end.

%%------------------------------------------------------------------------------

bunch_zlist(Num, Zlist) ->
    case zlists:scroll(Num, Zlist) of
        {[], _} ->
            [];
        {L, T} ->
            [L | fun() -> bunch_zlist(Num, T) end]
    end.
                 
