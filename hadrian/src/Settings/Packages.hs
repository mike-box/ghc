module Settings.Packages (packageArgs) where

import Expression
import Flavour
import Oracles.Setting
import Oracles.Flag
import Packages
import Settings

-- | Package-specific command-line arguments.
packageArgs :: Args
packageArgs = do
    stage        <- getStage
    path         <- getBuildPath
    compilerPath <- expr $ buildPath (vanillaContext stage compiler)
    let -- Do not bind the result to a Boolean: this forces the configure rule
        -- immediately and may lead to cyclic dependencies.
        -- See: https://gitlab.haskell.org/ghc/ghc/issues/16809.
        cross = flag CrossCompiling

        -- Check if the bootstrap compiler has the same version as the one we
        -- are building. This is used to build cross-compilers
        bootCross = (==) <$> ghcVersionStage Stage0 <*> ghcVersionStage Stage1

    mconcat
        --------------------------------- base ---------------------------------
        [ package base ? mconcat
          [ builder (Cabal Flags) ? notStage0 `cabalFlag` (pkgName ghcBignum)

          -- This fixes the 'unknown symbol stat' issue.
          -- See: https://github.com/snowleopard/hadrian/issues/259.
          , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]

        --------------------------------- cabal --------------------------------
        -- Cabal is a large library and slow to compile. Moreover, we build it
        -- for Stage0 only so we can link ghc-pkg against it, so there is little
        -- reason to spend the effort to optimise it.
        , package cabal ?
          stage0 ? builder Ghc ? arg "-O0"

        ------------------------------- compiler -------------------------------
        , package compiler ? mconcat
          [ builder Alex ? arg "--latin1"

          , builder (Ghc CompileHs) ? mconcat
            [ inputs ["**/GHC.hs", "**/GHC/Driver/Make.hs"] ? arg "-fprof-auto"
            , input "**/Parser.hs" ?
              pure ["-fno-ignore-interface-pragmas", "-fcmm-sink"]
            -- These files take a very long time to compile with -O1,
            -- so we use -O0 for them just in Stage0 to speed up the
            -- build but not affect Stage1+ executables
            , inputs ["**/GHC/Hs/Instances.hs", "**/GHC/Driver/Session.hs"] ? stage0 ?
              pure ["-O0"] ]

          , builder (Cabal Setup) ? mconcat
            [ arg "--disable-library-for-ghci"
            , anyTargetOs ["openbsd"] ? arg "--ld-options=-E"
            , flag GhcUnregisterised ? arg "--ghc-option=-DNO_REGS"
            , notM targetSupportsSMP ? arg "--ghc-option=-DNOSMP"
            , notM targetSupportsSMP ? arg "--ghc-option=-optc-DNOSMP"
            , ghcWithInterpreter ?
              ghciWithDebugger <$> flavour ?
              notStage0 ? arg "--ghc-option=-DDEBUGGER"
            , ghcProfiled <$> flavour ?
              notStage0 ? arg "--ghc-pkg-option=--force" ]

          , builder (Cabal Flags) ? mconcat
            [ andM [expr ghcWithInterpreter, notStage0] `cabalFlag` "internal-interpreter"
            , notM cross `cabalFlag` "terminfo"
            ]

          , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]

        ---------------------------------- ghc ---------------------------------
        , package ghc ? mconcat
          [ builder Ghc ? arg ("-I" ++ compilerPath)

          , builder (Cabal Flags) ? mconcat
            [ andM [expr ghcWithInterpreter, notStage0] `cabalFlag` "internal-interpreter"
            , notM cross `cabalFlag` "terminfo"
            , ifM stage0
                  -- We build a threaded stage 1 if the bootstrapping compiler
                  -- supports it.
                  (threadedBootstrapper `cabalFlag` "threaded")

                  -- We build a threaded stage N, N>1 if the configuration calls
                  -- for it.
                  ((ghcThreaded <$> expr flavour) `cabalFlag` "threaded")
            ]
          ]

        -------------------------------- ghcPkg --------------------------------
        , package ghcPkg ?
          builder (Cabal Flags) ? notM cross `cabalFlag` "terminfo"

        -------------------------------- ghcPrim -------------------------------
        , package ghcPrim ? mconcat
          [ builder (Cabal Flags) ? arg "include-ghc-prim"

          , builder (Cc CompileC) ? (not <$> flag CcLlvmBackend) ?
            input "**/cbits/atomic.c"  ? arg "-Wno-sync-nand" ]

        --------------------------------- ghci ---------------------------------
        , package ghci ? mconcat
          [
          -- The use case here is that we want to build @iserv-proxy@ for the
          -- cross compiler. That one needs to be compiled by the bootstrap
          -- compiler as it needs to run on the host. Hence @libiserv@ needs
          -- @GHCi.TH@, @GHCi.Message@ and @GHCi.Run@ from @ghci@. And those are
          -- behind the @-finternal-interpreter@ flag.
          --
          -- But it may not build if we have made some changes to ghci's
          -- dependencies (see #16051).
          --
          -- To fix this properly Hadrian would need to:
          --   * first build a compiler for the build platform (stage1 is enough)
          --   * use it as a bootstrap compiler to build the stage1 cross-compiler
          --
          -- The issue is that "configure" would have to be executed twice (for
          -- the build platform and for the cross-platform) and Hadrian would
          -- need to be fixed to support two different stage1 compilers.
          --
          -- The workaround we use is to check if the bootstrap compiler has
          -- the same version as the one we are building. In this case we can
          -- avoid the first step above and directly build with
          -- `-finternal-interpreter`.
          --
          -- TODO: Note that in that case we also do not need to build most of
          -- the Stage1 libraries, as we already know that the bootstrap
          -- compiler comes with the same versions as the one we are building.
          --
            builder (Cabal Flags) ? ifM stage0
              (andM [cross, bootCross] `cabalFlag` "internal-interpreter")
              (arg "internal-interpreter")

          ]

        --------------------------------- iserv --------------------------------
        -- Add -Wl,--export-dynamic enables GHCi to load dynamic objects that
        -- refer to the RTS.  This is harmless if you don't use it (adds a bit
        -- of overhead to startup and increases the binary sizes) but if you
        -- need it there's no alternative.
        --
        -- The Solaris linker does not support --export-dynamic option. It also
        -- does not need it since it exports all dynamic symbols by default
        , package iserv
          ? expr isElfTarget
          ? notM (expr $ anyTargetOs ["freebsd", "solaris2"])? mconcat
          [ builder (Ghc LinkHs) ? arg "-optl-Wl,--export-dynamic" ]

        -------------------------------- haddock -------------------------------
        , package haddock ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------- haskeline ------------------------------
        -- Hadrian doesn't currently support packages containing both libraries
        -- and executables. This flag disables the latter.
        , package haskeline ?
          builder (Cabal Flags) ? arg "-examples"
        -- Don't depend upon terminfo when cross-compiling to avoid unnecessary
        -- dependencies.
        -- TODO: Perhaps the user should rather be responsible for this?
        , package haskeline ?
          builder (Cabal Flags) ? notM cross `cabalFlag` "terminfo"

        -------------------------------- hsc2hs --------------------------------
        , package hsc2hs ?
          builder (Cabal Flags) ? arg "in-ghc-tree"

        ------------------------------ ghc-bignum ------------------------------
        , ghcBignumArgs

        ---------------------------------- rts ---------------------------------
        , package rts ? rtsPackageArgs -- RTS deserves a separate function

        -------------------------------- runGhc --------------------------------
        , package runGhc ?
          builder Ghc ? input "**/Main.hs" ?
          (\version -> ["-cpp", "-DVERSION=" ++ show version]) <$> getSetting ProjectVersion
        ]

ghcBignumArgs :: Args
ghcBignumArgs = package ghcBignum ? do
    -- These are only used for non-in-tree builds.
    librariesGmp <- getSetting GmpLibDir
    includesGmp <- getSetting GmpIncludeDir

    backend <- getBignumBackend
    check   <- getBignumCheck

    mconcat
          [ -- select BigNum backend
            builder (Cabal Flags) ? arg backend

          , -- check the selected backend against native backend
            builder (Cabal Flags) ? check `cabalFlag` "check"

            -- backend specific
          , case backend of
               "gmp" -> mconcat
                   [ builder (Cabal Setup) ? mconcat

                       -- enable GMP backend: configure script will produce
                       -- `ghc-bignum.buildinfo` and `include/HsIntegerGmp.h`
                     [ arg "--configure-option=--with-gmp"

                       -- enable in-tree support: don't depend on external "gmp"
                       -- library
                     , flag GmpInTree ? arg "--configure-option=--with-intree-gmp"

                       -- prefer framework over library (on Darwin)
                     , flag GmpFrameworkPref ?
                       arg "--configure-option=--with-gmp-framework-preferred"

                       -- Ensure that the ghc-bignum package registration includes
                       -- knowledge of the system gmp's library and include directories.
                     , notM (flag GmpInTree) ? mconcat
                       [ if not (null librariesGmp) then arg ("--extra-lib-dirs=" ++ librariesGmp) else mempty
                       , if not (null includesGmp) then arg ("--extra-include-dirs=" ++ includesGmp) else mempty
                       ]
                     ]
                  ]
               _ -> mempty
          ]

-- | RTS-specific command line arguments.
rtsPackageArgs :: Args
rtsPackageArgs = package rts ? do
    projectVersion <- getSetting ProjectVersion
    hostPlatform   <- getSetting HostPlatform
    hostArch       <- getSetting HostArch
    hostOs         <- getSetting HostOs
    hostVendor     <- getSetting HostVendor
    buildPlatform  <- getSetting BuildPlatform
    buildArch      <- getSetting BuildArch
    buildOs        <- getSetting BuildOs
    buildVendor    <- getSetting BuildVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    ghcUnreg       <- expr $ yesNo <$> flag GhcUnregisterised
    ghcEnableTNC   <- expr $ yesNo <$> flag TablesNextToCode
    rtsWays        <- getRtsWays
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    libffiName     <- expr libffiLibraryName
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    libdwIncludeDir   <- getSetting LibdwIncludeDir
    libdwLibraryDir   <- getSetting LibdwLibDir
    libnumaIncludeDir <- getSetting LibnumaIncludeDir
    libnumaLibraryDir <- getSetting LibnumaLibDir

    -- Arguments passed to GHC when compiling C and .cmm sources.
    let ghcArgs = mconcat
          [ arg "-Irts"
          , arg $ "-I" ++ path
          , arg $ "-DRtsWay=\"rts_" ++ show way ++ "\""
          -- Set the namespace for the rts fs functions
          , arg $ "-DFS_NAMESPACE=rts"
          , arg $ "-DCOMPILING_RTS"
          , notM targetSupportsSMP           ? arg "-DNOSMP"
          , way `elem` [debug, debugDynamic] ? pure [ "-DTICKY_TICKY"
                                                    , "-optc-DTICKY_TICKY"]
          , Profiling `wayUnit` way          ? arg "-DPROFILING"
          , Threaded  `wayUnit` way          ? arg "-DTHREADED_RTS"
          , notM targetSupportsSMP           ? pure [ "-DNOSMP"
                                                    , "-optc-DNOSMP" ]
          ]

    let cArgs = mconcat
          [ rtsWarnings
          , flag UseSystemFfi ? arg ("-I" ++ ffiIncludeDir)
          , flag WithLibdw ? arg ("-I" ++ libdwIncludeDir)
          , arg "-fomit-frame-pointer"
          -- RTS *must* be compiled with optimisations. The INLINE_HEADER macro
          -- requires that functions are inlined to work as expected. Inlining
          -- only happens for optimised builds. Otherwise we can assume that
          -- there is a non-inlined variant to use instead. But RTS does not
          -- provide non-inlined alternatives and hence needs the function to
          -- be inlined. See https://github.com/snowleopard/hadrian/issues/90.
          , arg "-O2"
          , arg "-g"

          , arg "-Irts"
          , arg $ "-I" ++ path

          , Debug     `wayUnit` way          ? pure [ "-DDEBUG"
                                                    , "-fno-omit-frame-pointer"
                                                    , "-g3"
                                                    , "-O0" ]

          , inputs ["**/RtsMessages.c", "**/Trace.c"] ?
            arg ("-DProjectVersion=" ++ show projectVersion)

          , input "**/RtsUtils.c" ? pure
            [ "-DProjectVersion="            ++ show projectVersion
            , "-DHostPlatform="              ++ show hostPlatform
            , "-DHostArch="                  ++ show hostArch
            , "-DHostOS="                    ++ show hostOs
            , "-DHostVendor="                ++ show hostVendor
            , "-DBuildPlatform="             ++ show buildPlatform
            , "-DBuildArch="                 ++ show buildArch
            , "-DBuildOS="                   ++ show buildOs
            , "-DBuildVendor="               ++ show buildVendor
            , "-DTargetPlatform="            ++ show targetPlatform
            , "-DTargetArch="                ++ show targetArch
            , "-DTargetOS="                  ++ show targetOs
            , "-DTargetVendor="              ++ show targetVendor
            , "-DGhcUnregisterised="         ++ show ghcUnreg
            , "-DTablesNextToCode="          ++ show ghcEnableTNC
            ]

          -- We're after pur performance here. So make sure fast math and
          -- vectorization is enabled.
          , input "**/Hash.c" ? pure
            [ "-O3" ]

            , inputs ["**/Evac.c", "**/Evac_thr.c"] ? arg "-funroll-loops"

            , speedHack ?
              inputs [ "**/Evac.c", "**/Evac_thr.c"
                     , "**/Scav.c", "**/Scav_thr.c"
                     , "**/Compact.c", "**/GC.c" ] ? arg "-fno-PIC"
            -- @-static@ is necessary for these bits, as otherwise the NCG
            -- generates dynamic references.
            , speedHack ?
              inputs [ "**/Updates.c", "**/StgMiscClosures.c"
                     , "**/PrimOps.c", "**/Apply.c"
                     , "**/AutoApply.c" ] ? pure ["-fno-PIC", "-static"]

            -- inlining warnings happen in Compact
            , inputs ["**/Compact.c"] ? arg "-Wno-inline"

            -- emits warnings about call-clobbered registers on x86_64
            , inputs [ "**/StgCRun.c"
                     , "**/win32/ConsoleHandler.c", "**/win32/ThrIOManager.c"] ? arg "-w"
            -- The above warning suppression flags are a temporary kludge.
            -- While working on this module you are encouraged to remove it and fix
            -- any warnings in the module. See:
            -- https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions#Warnings

            , (not <$> flag CcLlvmBackend) ?
              inputs ["**/Compact.c"] ? arg "-finline-limit=2500"

            , input "**/RetainerProfile.c" ? flag CcLlvmBackend ?
              arg "-Wno-incompatible-pointer-types"
            ]

    mconcat
        [ builder (Cabal Flags) ? mconcat
          [ any (wayUnit Profiling) rtsWays `cabalFlag` "profiling"
          , any (wayUnit Debug) rtsWays     `cabalFlag` "debug"
          , any (wayUnit Logging) rtsWays   `cabalFlag` "logging"
          , any (wayUnit Dynamic) rtsWays   `cabalFlag` "dynamic"
          , useLibffiForAdjustors           `cabalFlag` "libffi-adjustors"
          , Debug `wayUnit` way             `cabalFlag` "find-ptr"
          ]
        , builder (Cabal Setup) ? mconcat
          [ if not (null libdwLibraryDir) then arg ("--extra-lib-dirs="++libdwLibraryDir) else mempty
          , if not (null libdwIncludeDir) then arg ("--extra-include-dirs="++libdwIncludeDir) else mempty
          , if not (null libnumaLibraryDir) then arg ("--extra-lib-dirs="++libnumaLibraryDir) else mempty
          , if not (null libnumaIncludeDir) then arg ("--extra-include-dirs="++libnumaIncludeDir) else mempty
          ]
        , builder (Cc (FindCDependencies CDep)) ? cArgs
        , builder (Cc (FindCDependencies  CxxDep)) ? cArgs
        , builder (Ghc CompileCWithGhc) ? map ("-optc" ++) <$> cArgs
        , builder (Ghc CompileCppWithGhc) ? map ("-optcxx" ++) <$> cArgs
        , builder Ghc ? ghcArgs

        , builder HsCpp ? pure
          [ "-DTOP="             ++ show top
          , "-DFFI_INCLUDE_DIR=" ++ show ffiIncludeDir
          , "-DFFI_LIB_DIR="     ++ show ffiLibraryDir
          , "-DFFI_LIB="         ++ show libffiName
          , "-DLIBDW_LIB_DIR="   ++ show libdwLibraryDir ]

        , builder HsCpp ? flag WithLibdw ? arg "-DUSE_LIBDW"
        , builder HsCpp ? flag HaveLibMingwEx ? arg "-DHAVE_LIBMINGWEX" ]

-- Compile various performance-critical pieces *without* -fPIC -dynamic
-- even when building a shared library.  If we don't do this, then the
-- GC runs about 50% slower on x86 due to the overheads of PIC.  The
-- cost of doing this is a little runtime linking and less sharing, but
-- not much.
--
-- On x86_64 this doesn't work, because all objects in a shared library
-- must be compiled with -fPIC (since the 32-bit relocations generated
-- by the default small memory can't be resolved at runtime).  So we
-- only do this on i386.
--
-- This apparently doesn't work on OS X (Darwin) nor on Solaris.
-- On Darwin we get errors of the form
--
--  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast
--      from rts/dist/build/Apply.dyn_o not allowed in slidable image
--
-- and lots of these warnings:
--
--  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image
--      from loading in dyld shared cache
--
-- On Solaris we get errors like:
--
-- Text relocation remains                         referenced
--     against symbol                  offset      in file
-- .rodata (section)                   0x11        rts/dist/build/Apply.dyn_o
--   ...
-- ld: fatal: relocations remain against allocatable but non-writable sections
-- collect2: ld returned 1 exit status
speedHack :: Action Bool
speedHack = do
    i386   <- anyTargetArch ["i386"]
    goodOS <- not <$> anyTargetOs ["darwin", "solaris2"]
    return $ i386 && goodOS

-- See @rts/ghc.mk@.
rtsWarnings :: Args
rtsWarnings = mconcat
    [ arg "-Wall"
    , arg "-Wextra"
    , arg "-Wstrict-prototypes"
    , arg "-Wmissing-prototypes"
    , arg "-Wmissing-declarations"
    , arg "-Winline"
    , arg "-Wpointer-arith"
    , arg "-Wmissing-noreturn"
    , arg "-Wnested-externs"
    , arg "-Wredundant-decls"
    , arg "-Wundef"
    , arg "-fno-strict-aliasing" ]
