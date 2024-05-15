import pprint
import stacky

pub fn main() {
  stacky.trace()
  |> pprint.debug
  // See the strack trace
  |> stacky.frame(0)
  |> pprint.debug
  // See the top most stack frame, from this call site
  |> stacky.gleam_module_name
  |> pprint.debug
  // See the current module name, from this call site
}
