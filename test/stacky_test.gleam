import gleeunit
import gleeunit/should
import stacky.{
  ErlangFileName, ErlangLineNumber, ErlangModuleName, FunctionArity,
  FunctionName, StackFrame, StackIndex, StackTrace,
}

pub fn main() {
  gleeunit.main()
}

const erlang_file_name = "stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky_test.erl"

pub fn stacky_test() {
  let expected_trace =
    StackTrace([
      StackFrame(
        StackIndex(6),
        ErlangModuleName("stacky_test"),
        FunctionName("stacky_test"),
        FunctionArity(0),
        ErlangFileName(
          "stacky/build/dev/erlang/stacky/_gleam_artefacts/stacky_test.erl",
        ),
        ErlangLineNumber(64),
      ),
      StackFrame(
        StackIndex(5),
        ErlangModuleName("eunit_test"),
        FunctionName("-mf_wrapper/2-fun-0-"),
        FunctionArity(2),
        ErlangFileName("eunit_test.erl"),
        ErlangLineNumber(273),
      ),
      StackFrame(
        StackIndex(4),
        ErlangModuleName("eunit_test"),
        FunctionName("run_testfun"),
        FunctionArity(1),
        ErlangFileName("eunit_test.erl"),
        ErlangLineNumber(71),
      ),
      StackFrame(
        StackIndex(3),
        ErlangModuleName("eunit_proc"),
        FunctionName("run_test"),
        FunctionArity(1),
        ErlangFileName("eunit_proc.erl"),
        ErlangLineNumber(543),
      ),
      StackFrame(
        StackIndex(2),
        ErlangModuleName("eunit_proc"),
        FunctionName("with_timeout"),
        FunctionArity(3),
        ErlangFileName("eunit_proc.erl"),
        ErlangLineNumber(368),
      ),
      StackFrame(
        StackIndex(1),
        ErlangModuleName("eunit_proc"),
        FunctionName("handle_test"),
        FunctionArity(2),
        ErlangFileName("eunit_proc.erl"),
        ErlangLineNumber(526),
      ),
    ])

  let expected_frame =
    StackFrame(
      StackIndex(6),
      ErlangModuleName("stacky_test"),
      FunctionName("stacky_test"),
      FunctionArity(0),
      ErlangFileName(erlang_file_name),
      ErlangLineNumber(64),
    )

  // The ErlangFileName is relative to wherever this test runs so we need to replace it
  // let StackTrace(trace) = trace
  let trace = stacky.trace()
  let assert StackTrace([head_frame, ..frames]) = trace
  let head_frame =
    StackFrame(..head_frame, erlang_file_name: ErlangFileName(erlang_file_name))
  let trace = StackTrace([head_frame, ..frames])

  let frame =
    trace
    |> stacky.frame(0)

  let gleam_module_name =
    frame
    |> stacky.gleam_module_name

  trace
  |> should.equal(expected_trace)

  frame
  |> should.equal(expected_frame)

  gleam_module_name
  |> should.equal("stacky_test")
}
