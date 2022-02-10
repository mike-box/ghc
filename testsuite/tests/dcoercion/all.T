
## Correctness tests: these programs should compile and pass Core Lint.
test('DCo_Hetero', normal, compile, [''])
test('DCo_HsBinds', normal, compile, ['-O'])
test('DCo_HsType', normal, compile, [''])
test('DCo_InScope', normal, compile, [''])
test('DCo_LiftTyped', normal, compile, [''])
test('DCo_Phantom', normal, compile, ['-fdefer-type-errors'])
test('DCo_PostProcess', normal, compile, [''])
test('DCo_Specialise', normal, compile, [''])
test('DCo_Typeable', normal, compile, ['-O'])
test('DCo_TypeRep', normal, compile, [''])

## The following tests that we optimise away certain transitive coercions.
## However, this optimisation was too slow, so we comment this out for now.
#test('DCo_TransOpt', normal, compile_and_run, ['-package ghc'])

test ('DCo_Read',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])
