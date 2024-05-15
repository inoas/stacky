import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/string

pub type StackTrace {
  StackTrace(List(StackFrame))
}

pub type StackFrame {
  StackFrame(
    erlang_module_name: ErlangModuleName,
    function_name: FunctionName,
    function_arity: FunctionArity,
    erlang_file_name: ErlangFileName,
    erlang_line_number: ErlangLineNumber,
  )
}

pub type ErlangModuleName {
  ErlangModuleName(String)
}

pub type FunctionName {
  FunctionName(String)
}

pub type FunctionArity {
  FunctionArity(Int)
}

pub type ErlangFileName {
  ErlangFileName(String)
}

pub type ErlangLineNumber {
  ErlangLineNumber(Int)
}

pub type StackFrameTuple =
  #(String, String, Int, String, Int)

pub fn trace() -> StackTrace {
  let erlang_stack_trace: List(StackFrameTuple) = stacky_erlang_stacktrace()
  erlang_stack_trace
  |> list.map(fn(frame: StackFrameTuple) {
    let #(
      erlang_module_name,
      function_name,
      function_arity,
      erlang_file_name,
      erlang_line_number,
    ) = frame
    StackFrame(
      erlang_module_name: ErlangModuleName(erlang_module_name),
      function_name: FunctionName(function_name),
      function_arity: FunctionArity(function_arity),
      erlang_file_name: ErlangFileName(erlang_file_name),
      erlang_line_number: ErlangLineNumber(erlang_line_number),
    )
  })
  |> StackTrace
}

pub fn frame(stacktrace: StackTrace, index: Int) -> StackFrame {
  let StackTrace(stacktrace) = stacktrace
  case stacktrace |> list_at(index) {
    Ok(stackframe) -> stackframe
    Error(_) -> {
      let panic_msg = "No stack frame at index " <> int.to_string(index) <> "."
      panic as panic_msg
    }
  }
}

pub fn erlang_module_name(stack_frame: StackFrame) -> String {
  let ErlangModuleName(erlang_module_name) = stack_frame.erlang_module_name
  erlang_module_name
}

pub fn gleam_module_name(stack_frame: StackFrame) -> String {
  let erlang_module_name = stack_frame |> erlang_module_name()

  let assert Ok(double_at_re) = regex.from_string("@@")
  let assert Ok(single_at_re) = regex.from_string("@")
  let has_double_ats =
    erlang_module_name |> regex.scan(with: double_at_re) |> list.is_empty
    == False
  let has_ats =
    erlang_module_name |> regex.scan(with: single_at_re) |> list.is_empty
    == False

  case has_double_ats, has_ats {
    True, _ -> erlang_module_name <> " (gleam internal module)"
    False, False -> erlang_module_name
    False, True -> erlang_module_name |> string.replace(each: "@", with: "/")
  }
}

pub fn function_name(stack_frame: StackFrame) -> String {
  let FunctionName(function_name) = stack_frame.function_name
  function_name
}

pub fn function_arity(stack_frame: StackFrame) -> Int {
  let FunctionArity(function_arity) = stack_frame.function_arity
  function_arity
}

pub fn erlang_file_name(stack_frame: StackFrame) -> String {
  let ErlangFileName(erlang_file_name) = stack_frame.erlang_file_name
  erlang_file_name
}

pub fn erlang_line_number(stack_frame: StackFrame) -> Int {
  let ErlangLineNumber(erlang_line_number) = stack_frame.erlang_line_number
  erlang_line_number
}

pub fn frame_to_string(stack_frame: StackFrame) -> String {
  let erlang_module_name = stack_frame |> gleam_module_name()
  let function_name = stack_frame |> function_name()
  let function_arity = stack_frame |> function_arity() |> int.to_string
  let erlang_file_name = stack_frame |> erlang_file_name()
  let erlang_line_number = stack_frame |> erlang_line_number() |> int.to_string

  erlang_module_name
  <> "."
  <> function_name
  <> " - arity: "
  <> function_arity
  <> " - erlang file: "
  <> erlang_file_name
  <> ":"
  <> erlang_line_number
}

pub fn print_frame_ln(stack_frame: StackFrame) {
  process.sleep(100)

  stack_frame
  |> frame_to_string()
  |> io.println

  process.sleep(100)
}

fn list_at(in list: List(a), get index: Int) -> Result(a, Nil) {
  case index >= 0 {
    True ->
      list
      |> list.drop(index)
      |> list.first
    False -> Error(Nil)
  }
}

@external(erlang, "stacky_ffi", "stacky_erlang_stacktrace")
fn stacky_erlang_stacktrace() -> List(StackFrameTuple)

pub fn main() {
  io.println("\nFor example stack traces, run:\n")
  io.println("  gleam run -m stacky/internal/example\n")
  io.println("...or...\n")
  io.println("  gleam run -m stacky/internal/sub_dir/example_in_sub_dir\n")
}
