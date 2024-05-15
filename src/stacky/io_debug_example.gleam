import gleam/io
import stacky

pub fn main() {
  // You should proabably use `pprint.debug()` instead of `io.debug()`:
  stacky.trace()
  |> io.debug
  // See the strack trace
  |> stacky.frame(0)
  |> io.debug
  // See the top most stack frame, from this call site
  |> stacky.module_name
  |> io.debug
  // See the current module name, from this call site
}
