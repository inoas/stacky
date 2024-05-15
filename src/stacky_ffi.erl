-module(stacky_ffi).

-export([stacky_erlang_stacktrace/0]).

stacky_erlang_stacktrace() ->
    FullStacktrace =
        try
            throw(test)
        catch
            _Class:_Reason:Stacktrace ->
                Stacktrace
        end,
    [_FfiCallFrame, _GleamCallFrame | RestStacktrace] = FullStacktrace,
    lists:map(fun(Stackframe) ->
                 case Stackframe of
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
                          <<"not-a-file">>,
                          -1};
                     Other ->
                         erlang:display(Other),
                         throw("unhandled stackframe format")
                 end
              end,
              RestStacktrace).
