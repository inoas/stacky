# Stacky ☆ BEAM stack trace in Gleam

## a stack trace of stack frames with module name, function name, arity, file name and line number

[![Package Version](https://img.shields.io/hexpm/v/stacky)](https://hex.pm/packages/stacky)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/stacky/)

```shell
gleam add stacky
```

Best used with [pprint](https://hexdocs.pm/pprint/).

## Usage

### io.debug

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

### pprint.debug

```gleam
import pprint
import stacky

pub fn main() {
  stacky.trace()
  |> pprint.debug
  // See the strack trace
  |> stacky.frame(0)
  |> pprint.debug
  // See the top most stack frame, from this call site
  |> stacky.module_name
  |> pprint.debug
  // See the current module name, from this call site
}
```

StdOut:

```text
StackTrace([
  StackFrame(
    ModuleName("stacky@pprint_debug_example"),
    FunctionName("main"),
    FunctionArity(0),
    FileName("/Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@pprint_debug_example.erl"),
    ErlangLineNumber(8),
  ),
  StackFrame(
    ModuleName("stacky@@main"),
    FunctionName("run"),
    FunctionArity(1),
    FileName("/Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@@main.erl"),
    ErlangLineNumber(11),
  ),
  StackFrame(
    ModuleName("erl_eval"),
    FunctionName("do_apply"),
    FunctionArity(7),
    FileName("erl_eval.erl"),
    ErlangLineNumber(746),
  ),
  StackFrame(
    ModuleName("init"),
    FunctionName("start_it"),
    FunctionArity(1),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
  StackFrame(
    ModuleName("init"),
    FunctionName("start_em"),
    FunctionArity(1),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
  StackFrame(
    ModuleName("init"),
    FunctionName("do_boot"),
    FunctionArity(3),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
])
StackFrame(
  ModuleName("stacky@pprint_debug_example"),
  FunctionName("main"),
  FunctionArity(0),
  FileName("/Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@pprint_debug_example.erl"),
  ErlangLineNumber(8),
)

"stacky@pprint_debug_example"
```

## Demo

```shell
gleam run -m stacky/io_debug_example # io.debug example
gleam run -m stacky/pprint_debug_example # pprint.debug example
```

Further documentation can be found at <https://hexdocs.pm/stacky>.
