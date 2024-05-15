import gleam/list

pub fn trace() -> StackTrace {
  let erlang_stack_trace: List(StackFrameTuple) = stacky_erlang_stacktrace()
  erlang_stack_trace
  |> list.map(fn(frame: StackFrameTuple) {
    let #(
      module_name,
      function_name,
      function_arity,
      file_name,
      erlang_line_number,
    ) = frame
    StackFrame(
      module_name: ModuleName(module_name),
      function_name: FunctionName(function_name),
      function_arity: FunctionArity(function_arity),
      file_name: FileName(file_name),
      erlang_line_number: ErlangLineNumber(erlang_line_number),
    )
  })
  |> StackTrace
}

pub fn frame(stacktrace: StackTrace, index: Int) -> StackFrame {
  let StackTrace(stacktrace) = stacktrace
  let assert Ok(stackframe) = stacktrace |> list.at(index)
  stackframe
}

pub fn module_name(stack_frame: StackFrame) -> String {
  let ModuleName(module_name) = stack_frame.module_name
  module_name
}

pub fn function_name(stack_frame: StackFrame) -> String {
  let FunctionName(function_name) = stack_frame.function_name
  function_name
}

pub fn function_arity(stack_frame: StackFrame) -> Int {
  let FunctionArity(function_arity) = stack_frame.function_arity
  function_arity
}

pub fn file_name(stack_frame: StackFrame) -> String {
  let FileName(file_name) = stack_frame.file_name
  file_name
}

pub fn erlang_line_number(stack_frame: StackFrame) -> Int {
  let ErlangLineNumber(erlang_line_number) = stack_frame.erlang_line_number
  erlang_line_number
}

pub type StackTrace {
  StackTrace(List(StackFrame))
}

pub type StackFrame {
  StackFrame(
    module_name: ModuleName,
    function_name: FunctionName,
    function_arity: FunctionArity,
    file_name: FileName,
    erlang_line_number: ErlangLineNumber,
  )
}

pub type ModuleName {
  ModuleName(String)
}

pub type FunctionName {
  FunctionName(String)
}

pub type FunctionArity {
  FunctionArity(Int)
}

pub type FileName {
  FileName(String)
}

pub type ErlangLineNumber {
  ErlangLineNumber(Int)
}

pub type StackFrameTuple =
  #(String, String, Int, String, Int)

@external(erlang, "stacky_ffi", "stacky_erlang_stacktrace")
fn stacky_erlang_stacktrace() -> List(StackFrameTuple)
