-- | The stg to cmm code generator configuration

module GHC.StgToCmm.Config
  ( StgToCmmConfig(..)
  , stgToCmmPlatform
  ) where

import GHC.Platform.Profile
import GHC.Platform
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Utils.TmpFs

import GHC.Prelude


-- This config is static and contains information only passed *downwards* by StgToCmm.Monad
data StgToCmmConfig = StgToCmmConfig
  ----------------------------- General Settings --------------------------------
  { stgToCmmProfile       :: !Profile            -- ^ Current profile
  , stgToCmmThisModule    :: Module              -- ^ The module being compiled. This field kept lazy for
                                                 -- Cmm/Parser.y which preloads it with a panic
  , stgToCmmTmpDir        :: !TempDir            -- ^ Temp Dir for files used in compilation
  , stgToCmmContext       :: !SDocContext        -- ^ Context for StgToCmm phase
  , stgToCmmDebugLevel    :: !Int                -- ^ The verbosity of debug messages
  , stgToCmmBinBlobThresh :: !Word               -- ^ Binary literals (e.g. strings) whose size is above this
                                                 -- threshold will be dumped in a binary file by the assembler
                                                 -- code generator (0 to disable)
  , stgToCmmMaxInlAllocSize :: !Int              -- ^ Max size, in bytes, of inline array allocations.
  ------------------------------ Ticky Options ----------------------------------
  , stgToCmmDoTicky        :: !Bool              -- ^ Ticky profiling enabled (cf @-ticky@)
  , stgToCmmTickyAllocd    :: !Bool              -- ^ True indicates ticky prof traces allocs of each named
                                                 -- thing in addition to allocs _by_ that thing
  , stgToCmmTickyLNE       :: !Bool              -- ^ True indicates ticky uses name-specific counters for
                                                 -- join-points (let-no-escape)
  , stgToCmmTickyDynThunk  :: !Bool              -- ^ True indicates ticky uses name-specific counters for
                                                 -- dynamic thunks
  ---------------------------------- Flags --------------------------------------
  , stgToCmmLoopification  :: !Bool              -- ^ Loopification enabled (cf @-floopification@)
  , stgToCmmAlignCheck     :: !Bool              -- ^ Insert alignment check (cf @-falignment-sanitisation@)
  , stgToCmmOptHpc         :: !Bool              -- ^ perform code generation for code coverage
  , stgToCmmFastPAPCalls   :: !Bool              -- ^
  , stgToCmmSCCProfiling   :: !Bool              -- ^ Check if cost-centre profiling is enabled
  , stgToCmmEagerBlackHole :: !Bool              -- ^
  , stgToCmmInfoTableMap   :: !Bool              -- ^ true means generate C Stub for IPE map, See note [Mapping
                                                 -- Info Tables to Source Positions]
  , stgToCmmOmitYields     :: !Bool              -- ^ true means omit heap checks when no allocation is performed
  , stgToCmmOmitIfPragmas  :: !Bool              -- ^ true means don't generate interface programs (implied by -O0)
  , stgToCmmPIC            :: !Bool              -- ^ true if @-fPIC@
  , stgToCmmPIE            :: !Bool              -- ^ true if @-fPIE@
  , stgToCmmExtDynRefs     :: !Bool              -- ^ true if @-fexternal-dynamic-refs@, meaning generate
                                                 -- code for linking against dynamic libraries
  , stgToCmmDoBoundsCheck  :: !Bool              -- ^ decides whether to check array bounds in StgToCmm.Prim
                                                 -- or not
  , stgToCmmDoTagCheck     :: !Bool              -- ^ Verify tag inference predictions.
  ------------------------------ Backend Flags ----------------------------------
  , stgToCmmAllowBigArith             :: !Bool   -- ^ Allowed to emit larger than native size arithmetic (only LLVM and C backends)
  , stgToCmmAllowQuotRemInstr         :: !Bool   -- ^ Allowed to generate QuotRem instructions
  , stgToCmmAllowQuotRem2             :: !Bool   -- ^ Allowed to generate QuotRem
  , stgToCmmAllowExtendedAddSubInstrs :: !Bool   -- ^ Allowed to generate AddWordC, SubWordC, Add2, etc.
  , stgToCmmAllowIntMul2Instr         :: !Bool   -- ^ Allowed to generate IntMul2 instruction
  , stgToCmmAllowFabsInstrs           :: !Bool   -- ^ Allowed to generate Fabs instructions
  , stgToCmmTickyAP                   :: !Bool   -- ^ Disable use of precomputed standard thunks.
  ------------------------------ SIMD flags ------------------------------------
  -- Each of these flags checks vector compatibility with the backend requested
  -- during compilation. In essence, this means checking for @-fllvm@ which is
  -- the only backend that currently allows SIMD instructions, see
  -- Ghc.StgToCmm.Prim.checkVecCompatibility for these flags only call site.
  , stgToCmmVecInstrsErr   :: Maybe String       -- ^ Error (if any) to raise when vector instructions are
                                                 -- used, see @StgToCmm.Prim.checkVecCompatibility@
  , stgToCmmAvx            :: !Bool              -- ^ check for Advanced Vector Extensions
  , stgToCmmAvx2           :: !Bool              -- ^ check for Advanced Vector Extensions 2
  , stgToCmmAvx512f        :: !Bool              -- ^ check for Advanced Vector 512-bit Extensions
  }


stgToCmmPlatform :: StgToCmmConfig -> Platform
stgToCmmPlatform = profilePlatform . stgToCmmProfile
