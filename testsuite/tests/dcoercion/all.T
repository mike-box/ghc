
## Correctness tests: these programs should pass Core Lint.
test('DCo_Hetero', normal, compile, [''])
test('DCo_InScope', normal, compile, [''])
test('DCo_LiftTyped', normal, compile, [''])
test('DCo_Phantom', normal, compile, ['-fdefer-type-errors'])
test('DCo_Specialise', normal, compile, [''])
test('DCo_TypeRep', normal, compile, [''])

## The following program relies on coercion optimisation in order
## to compile in a timely manner.
test('DCo_Read', compile_timeout_multiplier(0.1), compile, [''])

## Check that transitive directed coercions are properly optimised
## by mkTransDCo.
test('DCo_TransOpt', normal, compile_and_run, ['-package ghc'])

## Performance tests.
test ('T15703',
      [ collect_compiler_stats('bytes allocated',2) ],
      multimod_compile,
      ['T15703', '-v0 -O'])
test ('T8095',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])
test ('T13386',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O0'])
