# Stacky ☆ BEAM stack trace in Gleam

![Erlang-compatible](https://img.shields.io/badge/target-erlang-b83998)
<a href="https://hexdocs.pm/stacky"><img src="https://img.shields.io/badge/hex-docs-ffaff3" alt="Documentation" /></a>
<a href="https://hex.pm/packages/stacky"><img src="https://img.shields.io/hexpm/v/stacky" alt="Available on Hex" /></a>
![CI](https://github.com/inoas/stacky/actions/workflows/test.yml/badge.svg?branch=main)
<a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>

**A stack trace of stack frames with module name, function name, arity, file name and line number!**

## Installation

```shell
gleam add stacky
```

## Usage

```gleam
let stacktrace = stacky.trace()

io.print("\n")

io.println("Trace to string:")
stacktrace
|> stacky.print_trace

io.print("\n")

io.println("Frame with context:")
stackframe
|> stacky.print_frame_with(context: #("my_context"))
```

`stdout` example:

```plaintext
Trace to string:
#6 - stacky/internal/example.main - /0 - SOME_PATH/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@internal@example.erl:8
#5 - gleam entrypoint | stacky@@main.run - /1 - SOME_PATH/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@@main.erl:11
#4 - erl_eval.do_apply - /7 - erl_eval.erl:746
#3 - init.start_it - /1
#2 - init.start_em - /1
#1 - init.do_boot - /3

Frame with context:
#6 - stacky/internal/example.main - /0 - SOME_PATH/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@internal@example.erl:8
with context: #("my_context")
```

## Demo

```shell
gleam run --module stacky/internal/sub_dir/example_in_sub_dir.gleam
```

Further documentation can be found at <https://hexdocs.pm/stacky>.
