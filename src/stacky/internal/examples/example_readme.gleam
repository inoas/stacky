import gleam/io
import stacky

pub fn main() {
  let stack_trace = stacky.trace()

  io.print("\n")

  io.println("Trace to string:")
  stack_trace
  |> stacky.print_trace

  io.print("\n")

  let stack_frame =
    stack_trace
    |> stacky.frame(0)

  io.println("Frame with context:")
  stack_frame
  |> stacky.print_frame_with(context: #("my_context"))
}
