
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

## Performance tests.
test ('T15703',
      [ collect_compiler_stats('bytes allocated',2)
      , extra_files(['T15703_aux.hs'])
      ],
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
test ('DCo_Read',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])
test ('DCo_Record',
      [ collect_compiler_stats('bytes allocated',2)
      , extra_files(['SuperRecord.hs'])
      ],
      multimod_compile,
      ['DCo_Record', '-v0 -O'])
