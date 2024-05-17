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
# 06	main() of src/stacky/internal/examples/example_readme.gleam
    	in /SOME_PATH/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@internal@examples@example_readme.erl:8
# 05	stacky@@main:run/1
    	in /SOME_PATH/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@@main.erl:11
# 04	erl_eval:do_apply/7
    	in erl_eval.erl:746
# 03	init:start_it/1
# 02	init:start_em/1
# 01	init:do_boot/3

Frame with context:
# 06	main() of src/stacky/internal/examples/example_readme.gleam
    	in /Users/leo/local-dev/gleam/stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky@internal@examples@example_readme.erl:8
    	context: #("my_context")
```

## Demos

```shell
gleam run --module stacky/internal/examples/example_1
gleam run --module stacky/internal/examples/example_2
gleam run --module stacky/internal/examples/example_3
```

Further documentation can be found at <https://hexdocs.pm/stacky>.
