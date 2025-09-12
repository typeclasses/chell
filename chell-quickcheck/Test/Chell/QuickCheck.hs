{-# LANGUAGE CPP #-}
module Test.Chell.QuickCheck (property) where

import Test.Chell qualified as Chell
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Random qualified as QCRandom
import Test.QuickCheck.State qualified as State
import Test.QuickCheck.Test qualified as Test
import Test.QuickCheck.Text qualified as Text

-- | Convert a QuickCheck property to a Chell 'Chell.Test'.
--
-- @
-- import Test.Chell
-- import Test.Chell.QuickCheck
-- import Test.QuickCheck hiding (property)
--
-- test_NullLength :: Test
-- test_NullLength = property \"null-length\"
--    (\xs -> not (null xs) ==> length xs > 0)
-- @
property :: QuickCheck.Testable prop => String -> prop -> Chell.Test
property name prop = Chell.test name $ \opts -> Text.withNullTerminal $ \term ->
  do
    let seed = Chell.testOptionSeed opts

        args = QuickCheck.stdArgs

        state =
          State.MkState
            { State.terminal = term,
              State.maxSuccessTests = QuickCheck.maxSuccess args,
              State.maxDiscardedRatio = QuickCheck.maxDiscardRatio args,
#if MIN_VERSION_QuickCheck(2,15,0)
              State.replayStartSize = Nothing,
              State.maxTestSize = QuickCheck.maxSize args,
#else
              State.computeSize = computeSize (QuickCheck.maxSize args) (QuickCheck.maxSuccess args),
#endif
              State.numSuccessTests = 0,
              State.numDiscardedTests = 0,
              State.classes = mempty,
              State.tables = mempty,
              State.requiredCoverage = mempty,
              State.expected = True,
              State.coverageConfidence = Nothing,
              State.randomSeed = QCRandom.mkQCGen seed,
              State.numSuccessShrinks = 0,
              State.numTryShrinks = 0,
              State.numTotTryShrinks = 0,
              State.numRecentlyDiscardedTests = 0,
              State.labels = mempty,
              State.numTotMaxShrinks = QuickCheck.maxShrinks args
            }

    result <- Test.test state (QuickCheck.property prop)
    let output = Test.output result
        notes = [("seed", show seed)]
        failure = Chell.failure {Chell.failureMessage = output}

    return $
      case result of
        Test.Success {} -> Chell.TestPassed notes
        Test.Failure {} -> Chell.TestFailed notes [failure]
        Test.GaveUp {} -> Chell.TestAborted notes output
        Test.NoExpectedFailure {} -> Chell.TestFailed notes [failure]

#if !MIN_VERSION_QuickCheck(2,15,0)
-- copied from quickcheck-2.4.1.1/src/Test/QuickCheck/Test.hs
computeSize :: Int -> Int -> Int -> Int -> Int
computeSize maxSize maxSuccess n d
  -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
  -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
  | n
      `roundTo` maxSize
      + maxSize
      <= maxSuccess
      || n
      >= maxSuccess
      || maxSuccess
      `mod` maxSize
      == 0 =
      n `mod` maxSize + d `div` 10
  | otherwise =
      (n `mod` maxSize) * maxSize `div` (maxSuccess `mod` maxSize) + d `div` 10

roundTo :: Int -> Int -> Int
roundTo n m = (n `div` m) * m
#endif
