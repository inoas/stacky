import pprint
import stacky

pub fn some_function_that_calls_stacky() {
  let stack =
    stacky.trace()
    // See the strack trace:
    |> pprint.debug

  let frame =
    stack
    |> stacky.frame(0)
    // See the top most stack frame, from this call site:
    |> pprint.debug

  frame
  |> stacky.qualified_module_name
  // See the current qualified module name, from this call site:
  |> pprint.debug

  frame
  |> stacky.function_name
  // See the current function name, from this call site:
  |> pprint.debug

  // prints gleam module.function and erlang with line number to stdout
  frame
  |> stacky.print_frame
}
