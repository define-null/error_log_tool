%% Author: vvorobyov
%% Created: Mar 16, 2012
%% Description: TODO: Add description to disk_log_tool
-module(error_log_tool).

%%
%% Exported Functions
%%
-export([main/1]).
-export([cat/1, cat/2, visit_log/2, filter/2, print_log/2, print_zlist/2]).
-export([worker_fun/4]).

-define(REPORT_SEPARATOR,"~n### ~p -----------------------------------------------------------------------~n").
-define(FILE_SEPARATOR(File),"~n### ~p " ++ lists:flatten(io_lib:format("### ~100..-s ###", [File])) ++ "~n").
-define(OPTS, [
               {sasl,         undefined, "sasl",       undefined, "Only sasl reports"},
               {'sasl-error', undefined, "sasl-error", undefined, "Only sasl error reports"},
               {error,        undefined, "error",      undefined, "All standard or sasl error reports"},
               {warn,         undefined, "warn",       undefined, "All standard or sasl error or warning reports"}
              ]).
        
main([_|_]=Args) ->
    {ok, {Options, Files}} = getopt:parse(?OPTS, Args),

    Format = case length(Files) of
                 1 -> {format, plain};
                 _ -> {format, with_filename}
             end,
    
    try cat_logs(Files, [Format | Options])
    catch
        _:Reason ->
            io:format(standard_error,"error: ~p~n",[Reason]),
            halt(1)
    end;
main(_) ->
    getopt:usage(?OPTS, "error-log-tool"),
    halt(1).

%%
%% API Functions
%%

-record(print, {format}).

cat_logs([Log | Tail], Opts) ->
    cat(Opts, Log),
    cat_logs(Tail, Opts);
cat_logs([], _) ->
    ok.

cat(LogFilename) ->
    cat([], LogFilename).

cat(Options, LogFilename) ->
    print_log([{file, LogFilename} | Options], LogFilename).

print_log(Options, LogFilename) ->
    visit_log(fun(LogZList)-> print_zlist(Options, LogZList) end, LogFilename).

print_zlist(Options, LogZList) ->
    LogZList1 = zlists:ziph(zlists:seq(1, 1000000000000, 1), LogZList),

    BunchedList = bunch_zlist(50, LogZList1),
    Self = self(),
    Reader = spawn(fun() ->
                           print_work(1, undefined, Options),
                           Self ! ok
                   end),
    
    _WorkMan= spawn(fun() -> worker_manager(Reader,
                                            erlang:system_info(logical_processors),
                                            BunchedList,
                                            Options)
                    end),
    receive
        ok ->
            ok
    end.
    
print_evt({N, Report}, Format) ->
    file:write(standard_io, io_lib:format(Format, [N])),
    file:write(standard_io, Report),
    ok.

visit_log(Fun, LogFilename) when is_function(Fun, 1) ->
    {ok,Log}=disk_log:open([{name,LogFilename}, {file,LogFilename}, {mode,read_only}]),
    try Z = zlists_disk_log:read(Log),
         Fun(Z)
    after
        disk_log:close(Log)
    end.

%%
%% Local Functions
%%
print_work(1, undefined, Options)
  when is_list(Options) ->
    File = proplists:get_value(file, Options),
    Format = case proplists:get_value(format, Options) of
                 plain ->
                     ?REPORT_SEPARATOR;
                 with_filename ->
                     ?FILE_SEPARATOR(File)
             end,
    
    print_work(1, undefined, #print{format = Format});

print_work(Num, Num, #print{}) ->
    ok;

print_work(Num, undefined, #print{format = Format}=Print) ->
    receive
        {Pid, Num, Work} ->
            Pid ! ok,
            [ print_evt({N, W}, Format) || {N,W} <- Work ],
            print_work(Num + 1, undefined, Print);
        {max, Num} ->
            ok;
        {max, Last} ->
            print_work(Num, Last, Print)
    end;

print_work(Num, Last, #print{format = Format}=Print)
  when is_integer(Last) ->
    receive
        {Pid, Num, Work} ->
            Pid ! ok,
            [ print_evt({N, W}, Format) || {N,W} <- Work ],
            print_work(Num + 1, Last, Print)
    end.            
    
%%------------------------------------------------------------------------------

worker_manager(Reader, ProcNum, Zlist, Options) ->
    process_flag(trap_exit, true),
    
    {Tasks, Later} = zlists:scroll(ProcNum, Zlist),
    lists:foldl(fun(Task, Acc) ->
                        spawn_link(fun() ->
                                           worker_fun(Reader, Acc, Task, Options)
                                   end),
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
    spawn_link(fun() -> worker_fun(Reader, Last + 1, Bunch, Opts) end),
    worker_manager_i(Reader, Last + 1, Zlist(), Opts).

worker_fun(Reader, Num, Task, Options) ->
    Result = worker(Task, Options),
    Reader ! {self(), Num, Result},
    receive
        ok ->
            ok
    end.

worker(List, Options) ->
    lists:map( fun({_N,{NTime,Evt}})->
                       Event = {calendar:now_to_universal_time(NTime),Evt},
                       {_N, error_log_tool_fmt:format_event(Event)}
               end, filter(Options, List)).

%%------------------------------------------------------------------------------

bunch_zlist(Num, Zlist) ->
    case zlists:scroll(Num, Zlist) of
        {[], _} ->
            [];
        {L, T} ->
            [L | fun() -> bunch_zlist(Num, T) end]
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
