# Stacky ☆ BEAM stack trace in Gleam

## a stack trace of stack frames with module name, function name, arity, file name and line number

[![Package
<a href="https://github.com/inoas/stacky/releases"><img src="https://img.shields.io/github/release/inoas/stacky" alt="GitHub release"></a>
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
![CI](https://github.com/inoas/stacky/workflows/test/badge.svg?branch=main)
Version](https://img.shields.io/hexpm/v/stacky)](https://hex.pm/packages/stacky)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/stacky/)
![Erlang-compatible](https://img.shields.io/badge/target-erlang-b83998)

Best used with [pprint](https://hexdocs.pm/pprint/).

```shell
gleam add stacky
gleam add pprint
```

## Usage

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
  |> stacky.gleam_module_name
  |> pprint.debug
  // See the current gleam module name, from this call site
}
```

`stdout`:

```gleam
StackTrace([
  StackFrame(
    ErlangModuleName("stacky@example"),
    FunctionName("main"),
    FunctionArity(0),
    FileName("/Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@example.erl"),
    ErlangLineNumber(8),
  ),
  StackFrame(
    ErlangModuleName("stacky@@main"),
    FunctionName("run"),
    FunctionArity(1),
    FileName("/Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@@main.erl"),
    ErlangLineNumber(11),
  ),
  StackFrame(
    ErlangModuleName("erl_eval"),
    FunctionName("do_apply"),
    FunctionArity(7),
    FileName("erl_eval.erl"),
    ErlangLineNumber(746),
  ),
  StackFrame(
    ErlangModuleName("init"),
    FunctionName("start_it"),
    FunctionArity(1),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
  StackFrame(
    ErlangModuleName("init"),
    FunctionName("start_em"),
    FunctionArity(1),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
  StackFrame(
    ErlangModuleName("init"),
    FunctionName("do_boot"),
    FunctionArity(3),
    FileName([
      110, 111, 45, 102, 105, 108, 101,
    ]),
    ErlangLineNumber(-1),
  ),
])
```

```
StackFrame(
  ErlangModuleName("erl_eval"),
  FunctionName("do_apply"),
  FunctionArity(7),
  FileName("erl_eval.erl"),
  ErlangLineNumber(746),
)
```

```
"erl_eval"
```

## Demo

```shell
gleam run -m stacky/sub_dir/example_in_sub_dir.gleam
```

Further documentation can be found at <https://hexdocs.pm/stacky>.
