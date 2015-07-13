module Settings.GhcCabal (
    cabalSettings, bootPackageDbSettings, customPackageSettings
    ) where

import Base hiding (arg, args)
import Oracles.Base
import Oracles.Builder
import Ways
import Util
import Package
import Targets
import Switches
import Expression hiding (liftIO)
import Settings.Ways
import Settings.Util
import Settings.Packages
import Settings.TargetDirectory
import UserSettings

cabalSettings :: Settings
cabalSettings = builder GhcCabal ? do
    stage <- asks getStage
    pkg   <- asks getPackage
    mconcat [ arg "configure"
            , arg $ pkgPath pkg
            , arg $ targetDirectory stage pkg
            , dllSettings
            , argWith $ Ghc stage
            , argWith $ GhcPkg stage
            , stage0 ? bootPackageDbSettings
            , librarySettings
            , configKeyNonEmpty "hscolour" ? argWith HsColour
            , configureSettings
            , stage0 ? packageConstraints
            , argWith $ Gcc stage
            , notStage Stage0 ? argWith Ld
            , argWith Ar
            , argWith Alex
            , argWith Happy ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
librarySettings :: Settings
librarySettings = do
    ways            <- fromDiffExpr Settings.Ways.ways
    ghcInterpreter  <- ghcWithInterpreter
    dynamicPrograms <- dynamicGhcPrograms
    append [ if vanilla `elem` ways
             then  "--enable-library-vanilla"
             else "--disable-library-vanilla"
           , if vanilla `elem` ways && ghcInterpreter && not dynamicPrograms
             then  "--enable-library-for-ghci"
             else "--disable-library-for-ghci"
           , if profiling `elem` ways
             then  "--enable-library-profiling"
             else "--disable-library-profiling"
           , if dynamic `elem` ways
             then  "--enable-shared"
             else "--disable-shared" ]

configureSettings :: Settings
configureSettings = do
    stage <- asks getStage
    let conf key = appendSubD $ "--configure-option=" ++ key
        cFlags   = mconcat [ ccSettings
                           , remove ["-Werror"]
                           , argStagedConfig "conf-cc-args" ]
        ldFlags  = ldSettings <> argStagedConfig "conf-gcc-linker-args"
        cppFlags = cppSettings <> argStagedConfig "conf-cpp-args"
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , appendSubD "--gcc-options" $ cFlags <> ldFlags
        , conf "--with-iconv-includes"  $ argConfig "iconv-include-dirs"
        , conf "--with-iconv-libraries" $ argConfig "iconv-lib-dirs"
        , conf "--with-gmp-includes"    $ argConfig "gmp-include-dirs"
        , conf "--with-gmp-libraries"   $ argConfig "gmp-lib-dirs"
        -- TODO: why TargetPlatformFull and not host?
        , crossCompiling ? (conf "--host" $ argConfig "target-platform-full")
        , conf "--with-cc" . argM . showArg $ Gcc stage ]

bootPackageDbSettings :: Settings
bootPackageDbSettings = do
    sourcePath <- lift $ askConfig "ghc-source-path"
    arg $ "--package-db=" ++ sourcePath </> "libraries/bootstrapping.conf"

-- This is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument;
-- * otherwise, we must collapse it into one space-separated string.
dllSettings :: Settings
dllSettings = arg ""

packageConstraints :: Settings
packageConstraints = do
    pkgs <- fromDiffExpr packages
    constraints <- lift $ forM pkgs $ \pkg -> do
        let cabal  = pkgPath pkg </> pkgCabal pkg
            prefix = dropExtension (pkgCabal pkg) ++ " == "
        need [cabal]
        content <- lines <$> liftIO (readFile cabal)
        let vs = filter (("ersion:" `isPrefixOf`) . drop 1) content
        case vs of
            [v] -> return $ prefix ++ dropWhile (not . isDigit) v
            _   -> redError $ "Cannot determine package version in '"
                            ++ cabal ++ "'."
    args $ concatMap (\c -> ["--constraint", c]) $ constraints

-- TODO: should be in a different file
-- TODO: put all validating options together in one file
ccSettings :: Settings
ccSettings = validating ? do
    let gccGe46 = liftM not gccLt46
    mconcat [ arg "-Werror"
            , arg "-Wall"
            , gccIsClang ??
              ( arg "-Wno-unknown-pragmas" <>
                gccGe46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
              , gccGe46 ? arg "-Wno-error=inline" )]

ldSettings :: Settings
ldSettings = mempty

cppSettings :: Settings
cppSettings = mempty

customPackageSettings :: Settings
customPackageSettings = mconcat
    [ package integerGmp2 ?
      mconcat [ windowsHost ? builder GhcCabal ?
                arg "--configure-option=--with-intree-gmp"
              , ccArgs ["-Ilibraries/integer-gmp2/gmp"] ]

    , package base ?
      builder GhcCabal ? arg ("--flags=" ++ pkgName integerLibrary)

    , package ghcPrim ?
      builder GhcCabal ? arg "--flag=include-ghc-prim" ]
