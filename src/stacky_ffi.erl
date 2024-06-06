-module(stacky_ffi).

-export([stacky_erlang_stack_trace/0, stacky_erlang_call_and_catch_panics/1]).

enumerate_reversed(List) ->
    I = length(List),
    enumerate_reversed(I, 1, List).

enumerate_reversed(I, S, List) ->
    {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc-S} end, I, List),
    List1.

stacky_erlang_call_and_catch_panics(Function) when is_function(Function) ->
        try
            {ok, Function()}
        catch
            _Class:Reason:Stacktrace -> {error, handle_full_stack_trace({Reason, Stacktrace})}
        end.

stacky_erlang_stack_trace() ->
    FullStackTrace =
        try
            throw(trace)
        catch
            _Class:Reason:Stacktrace ->
                {Reason, Stacktrace}
        end,
    handle_full_stack_trace(FullStackTrace).

handle_full_stack_trace({Reason, FullStackTrace}) ->
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
						 % through Results
					     % Also wrap everything here in try catch and return it as a result
					     % to be handled by stacky lib
                         throw("unexpected stack frame data - please report this as a potential bug")
                 end
              end,
              RestStacktrace),
        {Reason, enumerate_reversed(StackTrace)}.
