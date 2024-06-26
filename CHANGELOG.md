# Changelog

## 2.0.0 - 2024-06-06

- Added `call_and_catch_panics` to catch panics and return a `Result`.
  Please run `gleam run -m stacky/internal/examples/example_4`
  to see what it does.
- Returning reason, and by that the included panic message from gleams panics.

## 1.4.0 - 2024-06-06

- Traverse the stack trace only once to enumerate it.
- Updated for Erlang/OTP 27.
- Updated for Gleam 1.2.0.

## 1.3.0 - 2024-05-17

- Improved formatting.
- Make gleam source modules clickable.

## 1.2.0 - 2024-05-17

- Added index lookup functions from last to first (list index)
  and/or first to last (stack frame index).

## 1.1.0 - 2024-05-16

- Addded inverse trace index.
- Added better stdout printing.

## 1.0.3 - 2024-05-15

- Fixes "not a file".

## 1.0.2 - 2024-05-15

- Pinned Erlang and Gleam versions and fix CI.
- Added changelog.

## 1.0.1 - 2024-05-15

- Improved readme.

## 1.0.0 - 2024-05-15

- Released initial version.
