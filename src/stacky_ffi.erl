-module(stacky_ffi).

-export([stacky_erlang_stack_trace/0]).

enumerate_reversed(List) ->
    I = length(List),
    enumerate_reversed(I, 1, List).

enumerate_reversed(I, S, List) ->
    {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc-S} end, I, List),
    List1.

stacky_erlang_stack_trace() ->
    FullStackTrace =
        try
            throw(tracing_crash)
        catch
            _Class:_Reason:Stacktrace ->
                Stacktrace
        end,
    [_FfiCallFrame, _StackyCallFrame | RestStacktrace] = FullStackTrace,
        StackTrace = lists:map(fun(StackFrame) ->
            case StackFrame of
                     {ModuleName, FunctionName, Arity, [{file, Filename}, {line, LineNumber}]} ->
                         {atom_to_binary(ModuleName),
                          atom_to_binary(FunctionName),
                          Arity,
                          iolist_to_binary(Filename),
                          LineNumber};
                     {ModuleName, FunctionName, Arity, []} ->
                         {atom_to_binary(ModuleName),
                          atom_to_binary(FunctionName),
                          Arity,
                          <<"">>,
                          -1};
                     Other ->
                         erlang:display(Other),
						 % TODO: pass this through to the gleam layer and somehow dump the information on the user
                         throw("unexpected stack frame data - please report this as a potential bug")
                 end
              end,
              RestStacktrace),
        enumerate_reversed(StackTrace).
