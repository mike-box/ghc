{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception.Type
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-----------------------------------------------------------------------------

module GHC.Exception.Type
       ( Exception(..)    -- Class
       , SomeExceptionWithBacktrace(..), SomeException(..), ArithException(..)
        , addBacktrace
       , divZeroException, overflowException, ratioZeroDenomException
       , underflowException
       ) where

import Data.Maybe
import Data.Typeable (Typeable, cast)
   -- loop: Data.Typeable -> GHC.Err -> GHC.Exception
import GHC.Base
import GHC.Show
import GHC.Exception.Backtrace (Backtrace)

{- |
The @SomeExceptionWithBacktrace@ type is the root of the exception type hierarchy.
When an exception of type @e@ is thrown, behind the scenes it is
encapsulated in a @SomeException@ which is wrapped by @SomeExceptionWithBacktrace@.
This additional layer is used to provide a list of 'Backtrace's.

@since 4.16.0.0
-}
data SomeExceptionWithBacktrace = SomeExceptionWithBacktrace SomeException [Backtrace]

-- | Former root of 'Exception's
-- Now 'SomeException' is usually wrapped by 'SomeExceptionWithBacktrace'.
-- 'SomeException' has been kept as a type for backwards compatibility.
data SomeException = forall e . Exception e => SomeException e

-- @since 4.16.0.0
instance Show SomeExceptionWithBacktrace where
    -- | Just delegate to the wrapped 'Exception' @e@.
    showsPrec p (SomeExceptionWithBacktrace (SomeException e) _) = showsPrec p e

-- | @since 3.0
instance Show SomeException where
   -- | Just delegate to the wrapped 'Exception' @e@.
   showsPrec p (SomeException e) = showsPrec p e

-- | Add a 'Backtrace' to the list of backtraces.
--
-- @since 4.16.0.0
addBacktrace :: Backtrace -> SomeExceptionWithBacktrace -> SomeExceptionWithBacktrace
addBacktrace bt (SomeExceptionWithBacktrace e bts) =
    SomeExceptionWithBacktrace e (bt : bts)

{- |
Any type that you wish to throw or catch as an exception must be an
instance of the @Exception@ class. The simplest case is a new exception
type directly below the root:

> data MyException = ThisException | ThatException
>     deriving Show
>
> instance Exception MyException

The default method definitions in the @Exception@ class do what we need
in this case. You can now throw and catch @ThisException@ and
@ThatException@ as exceptions:

@
*Main> throw ThisException \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: MyException))
Caught ThisException
@

In more complicated examples, you may wish to define a whole hierarchy
of exceptions:

> ---------------------------------------------------------------------
> -- Make the root exception type for all the exceptions in a compiler
>
> data SomeCompilerException = forall e . Exception e => SomeCompilerException e
>
> instance Show SomeCompilerException where
>     show (SomeCompilerException e) = show e
>
> instance Exception SomeCompilerException
>
> compilerExceptionToException :: Exception e => e -> SomeExceptionWithBacktrace
> compilerExceptionToException = toException . SomeCompilerException
>
> compilerExceptionFromException :: Exception e => SomeExceptionWithBacktrace -> Maybe e
> compilerExceptionFromException x = do
>     SomeCompilerException a <- fromException x
>     cast a
>
> ---------------------------------------------------------------------
> -- Make a subhierarchy for exceptions in the frontend of the compiler
>
> data SomeFrontendException = forall e . Exception e => SomeFrontendException e
>
> instance Show SomeFrontendException where
>     show (SomeFrontendException e) = show e
>
> instance Exception SomeFrontendException where
>     toException = compilerExceptionToException
>     fromException = compilerExceptionFromException
>
> frontendExceptionToException :: Exception e => e -> SomeExceptionWithBacktrace
> frontendExceptionToException = toException . SomeFrontendException
>
> frontendExceptionFromException :: Exception e => SomeExceptionWithBacktrace -> Maybe e
> frontendExceptionFromException x = do
>     SomeFrontendException a <- fromException x
>     cast a
>
> ---------------------------------------------------------------------
> -- Make an exception type for a particular frontend compiler exception
>
> data MismatchedParentheses = MismatchedParentheses
>     deriving Show
>
> instance Exception MismatchedParentheses where
>     toException   = frontendExceptionToException
>     fromException = frontendExceptionFromException

We can now catch a @MismatchedParentheses@ exception as
@MismatchedParentheses@, @SomeFrontendException@ or
@SomeCompilerException@, but not other types, e.g. @IOException@:

@
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: MismatchedParentheses))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeFrontendException))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeCompilerException))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: IOException))
*** Exception: MismatchedParentheses
@

-}
class (Typeable e, Show e) => Exception e where
    -- | Represent the exception as 'SomeExceptionWithBacktrace'
    -- If @e@ isn't already of type 'SomeExceptionWithBacktrace' this means some kind of wrapping.
    toException   :: e -> SomeExceptionWithBacktrace
    -- | Extract and cast the exception from it's wrapped representation
    -- If the exception cannot be casted to the expected type then the result is 'Nothing'.
    fromException :: SomeExceptionWithBacktrace -> Maybe e

    toException e = SomeExceptionWithBacktrace (SomeException e) []
    fromException (SomeExceptionWithBacktrace (SomeException e) _) = cast e

    -- | Render this exception value in a human-friendly manner.
    --
    -- Default implementation: @'show'@.
    --
    -- @since 4.8.0.0
    displayException :: e -> String
    displayException = show

instance Exception SomeException where
  toException e = SomeExceptionWithBacktrace e []
  fromException (SomeExceptionWithBacktrace e _) = Just e

instance Exception SomeExceptionWithBacktrace where
    toException se = se
    fromException = Just
    displayException (SomeExceptionWithBacktrace e _) = displayException e

-- |Arithmetic exceptions.
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator -- ^ @since 4.6.0.0
  deriving ( Eq  -- ^ @since 3.0
           , Ord -- ^ @since 3.0
           )

divZeroException, overflowException, ratioZeroDenomException, underflowException  :: SomeExceptionWithBacktrace
divZeroException        = toException DivideByZero
overflowException       = toException Overflow
ratioZeroDenomException = toException RatioZeroDenominator
underflowException      = toException Underflow

-- | @since 4.0.0.0
instance Exception ArithException

-- | @since 4.0.0.0
instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"
  showsPrec _ RatioZeroDenominator = showString "Ratio has zero denominator"
