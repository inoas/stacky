import gleam/io
import pprint
import stacky

pub fn main() {
  let stacktrace = stacky.trace()

  io.print("\n")

  io.println("Trace to string:")
  stacktrace
  |> stacky.print_trace

  io.print("\n")

  io.println("Frame:")
  let stackframe =
    stacktrace
    // get the top most stack frame
    |> stacky.frame(0)
    |> pprint.debug

  io.print("\n")

  io.print("Qualified module name: ")
  stackframe
  |> stacky.qualified_module_name
  |> io.println

  io.print("\n")

  io.println("Frame with context:")
  stackframe
  |> stacky.print_frame_with(context: #("my_context"))

  io.print("\n")

  io.println("Internal stack frame:")
  stacktrace
  |> pprint.debug
}
