import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/string
import pprint

/// Gets the stack trace of the current process.
///
pub fn trace() -> StackTrace {
  let erlang_stack_trace: List(FFIStackFrameTuple) = stacky_erlang_stacktrace()
  erlang_stack_trace
  |> list.map(fn(frame: FFIStackFrameTuple) {
    let #(
      stack_index,
      #(
        erlang_module_name,
        function_name,
        function_arity,
        erlang_file_name,
        erlang_line_number,
      ),
    ) = frame
    StackFrame(
      index: StackIndex(stack_index),
      erlang_module_name: ErlangModuleName(erlang_module_name),
      function_name: FunctionName(function_name),
      function_arity: FunctionArity(function_arity),
      erlang_file_name: ErlangFileName(erlang_file_name),
      erlang_line_number: ErlangLineNumber(erlang_line_number),
    )
  })
  |> StackTrace
}

/// Gets the stack frame at the given list index.
///
/// Latest stack frame is at index 0.
/// The StackFrame itself has an inverse index field
/// that represents the index of the frame within the stack.
///
pub fn frame(stacktrace: StackTrace, index: Int) -> StackFrame {
  let StackTrace(stacktrace) = stacktrace
  case
    stacktrace
    |> list_at(index)
  {
    Ok(stackframe) -> stackframe
    Error(_) -> {
      let panic_msg = "No stack frame at index " <> int.to_string(index) <> "."
      panic as panic_msg
    }
  }
}

/// Converts a stack frame to a string.
///
pub fn frame_to_string(stack_frame: StackFrame) -> String {
  let stack_index =
    stack_frame
    |> stack_index()
    |> int.to_string
  let erlang_module_name =
    stack_frame
    |> gleam_module_name()
  let function_name =
    stack_frame
    |> function_name()
  let function_arity =
    stack_frame
    |> function_arity()
    |> int.to_string
  let erlang_file_name =
    stack_frame
    |> erlang_file_name()
  let erlang_line_number =
    stack_frame
    |> erlang_line_number()
    |> int.to_string

  let line =
    "#"
    <> stack_index
    <> " - "
    <> erlang_module_name
    <> "."
    <> function_name
    <> " - /"
    <> function_arity

  line
  <> case erlang_file_name {
    "unknown" -> ""
    _ -> " - " <> erlang_file_name <> ":" <> erlang_line_number
  }
}

/// Converts a stack trace to a string.
///
pub fn trace_to_string(stack_frame: StackTrace) -> String {
  let StackTrace(stack_trace) = stack_frame
  stack_trace
  |> list.map(fn(stack_frame) {
    stack_frame
    |> frame_to_string()
  })
  |> string.join(with: "\n")
}

/// Print a stack frame.
///
pub fn print_frame(stack_frame: StackFrame) {
  process.sleep(100)

  stack_frame
  |> frame_to_string()
  |> io.println

  process.sleep(100)
}

/// Print a stack frame with context.
///
pub fn print_frame_with(stack_frame: StackFrame, context c: c) {
  process.sleep(100)

  stack_frame
  |> frame_to_string()
  |> io.print

  io.print("\nwith context: ")
  pprint.debug(c)

  process.sleep(100)
}

/// Print a stack trace.
///
pub fn print_trace(stack_trace: StackTrace) {
  process.sleep(100)

  stack_trace
  |> trace_to_string()
  |> io.println

  process.sleep(100)
}

/// Print a stack trace with context.
///
pub fn print_trace_with(stack_trace: StackTrace, context c: c) {
  process.sleep(100)

  stack_trace
  |> trace_to_string()
  |> io.print

  io.print("\nwith context: ")
  pprint.debug(c)

  process.sleep(100)
}

/// Gets the erlang module name of the stack frame.
///
pub fn erlang_module_name(stack_frame: StackFrame) -> String {
  let ErlangModuleName(erlang_module_name) = stack_frame.erlang_module_name
  erlang_module_name
}

/// Gets the potential gleam module name from the erlang stack frame.
///
pub fn gleam_module_name(stack_frame: StackFrame) -> String {
  let erlang_module_name =
    stack_frame
    |> erlang_module_name()

  let assert Ok(double_at_re) = regex.from_string("@@")
  let assert Ok(single_at_re) = regex.from_string("@")
  let has_double_ats =
    erlang_module_name
    |> regex.scan(with: double_at_re)
    |> list.is_empty
    == False
  let has_ats =
    erlang_module_name
    |> regex.scan(with: single_at_re)
    |> list.is_empty
    == False

  case has_double_ats, has_ats {
    True, _ -> "gleam entrypoint | " <> erlang_module_name
    False, False -> erlang_module_name
    False, True ->
      erlang_module_name
      |> string.replace(each: "@", with: "/")
  }
}

/// Gets the stack index within its parent stack frame.
///
pub fn stack_index(stack_frame: StackFrame) -> Int {
  let StackIndex(stack_index) = stack_frame.index
  stack_index
}

/// Gets the function name of the stack frame.
///
pub fn function_name(stack_frame: StackFrame) -> String {
  let FunctionName(function_name) = stack_frame.function_name
  function_name
}

/// Gets the function arity of the stack frame.
///
pub fn function_arity(stack_frame: StackFrame) -> Int {
  let FunctionArity(function_arity) = stack_frame.function_arity
  function_arity
}

/// Gets the erlang file name of the stack frame.
///
pub fn erlang_file_name(stack_frame: StackFrame) -> String {
  let ErlangFileName(erlang_file_name) = stack_frame.erlang_file_name
  erlang_file_name
}

/// Gets the ang erlline number of the stack frame.
///
pub fn erlang_line_number(stack_frame: StackFrame) -> Int {
  let ErlangLineNumber(erlang_line_number) = stack_frame.erlang_line_number
  erlang_line_number
}

pub type StackTrace {
  StackTrace(List(StackFrame))
}

pub type StackFrame {
  StackFrame(
    index: StackIndex,
    erlang_module_name: ErlangModuleName,
    function_name: FunctionName,
    function_arity: FunctionArity,
    erlang_file_name: ErlangFileName,
    erlang_line_number: ErlangLineNumber,
  )
}

pub type StackIndex {
  StackIndex(Int)
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

type FFIStackFrameTuple =
  #(Int, #(String, String, Int, String, Int))

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
fn stacky_erlang_stacktrace() -> List(FFIStackFrameTuple)

/// This is a library and the main function
/// exists as a placeholder if called as a function
/// from the command line.
///
pub fn main() {
  io.println("\nFor example stack traces, run:\n")
  io.println("    gleam run --module stacky/internal/example\n")
  io.println("...or...\n")
  io.println(
    "    gleam run --module stacky/internal/sub_dir/example_in_sub_dir\n",
  )
  io.println("...or...\n")
  io.println("    gleam run --module my_gleam_module\n")
}
