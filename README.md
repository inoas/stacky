# Stacky ☆ BEAM stack trace in Gleam: a stack trace of stack frames with module name, function name, arity, file name and line number

[![Package Version](https://img.shields.io/hexpm/v/stacky)](https://hex.pm/packages/stacky)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/stacky/)

```shell
gleam add stacky
```

Best used with [pprint](https://hexdocs.pm/pprint/).

## Usage

```gleam
import gleam/io
import stacky

pub fn main() {
  // You should proabably use pprint.debug()` instead of `io.debug():
  stacky.trace()
  |> io.debug // See the strack trace
  |> stacky.frame(0)
  |> io.debug // See the top most stack frame, from this call site
  |> stacky.module_name
  |> io.debug // See the current module name, from this call site
}
```

## Demo

```shell
gleam run -m stacky/io_debug_example # io.debug example
gleam run -m stacky/pprint_debug_example # pprint.debug example
```

Further documentation can be found at <https://hexdocs.pm/stacky>.
