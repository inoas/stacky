import gleam/io
import gleam/string
import stacky

pub fn main() {
  string.inspect(["hallo"])
  case happy_function |> stacky.call_and_catch_panics {
    Ok(_) -> Nil
    Error(stack_trace) -> stack_trace |> stacky.print_trace
  }

  case panic_function |> stacky.call_and_catch_panics {
    Ok(_) -> Nil
    Error(stack_trace) -> stack_trace |> stacky.print_trace
  }
}

fn happy_function() {
  io.print("\n")
  io.println("I'm a happy function and thus not going to crash!")
}

fn panic_function() {
  io.print("\n")
  io.println("I'm a sad function and thus I am going to crash:")
  panic as "panic at the disco"
}
