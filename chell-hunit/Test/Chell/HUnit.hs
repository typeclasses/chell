module Test.Chell.HUnit
	( hunit
	) where

import qualified Test.Chell as Chell
import           Test.HUnit.Lang (Assertion, performTestCase)

-- | Convert a sequence of HUnit assertions (embedded in IO) to a Chell
-- 'Chell.Test'.
--
-- @
--import Test.Chell
--import Test.Chell.HUnit
--import Test.HUnit
--
--test_Addition :: Test
--test_addition = hunit \"addition\" $ do
--    1 + 2 \@?= 3
--    2 + 3 \@?= 5
-- @
hunit :: String -> Assertion -> Chell.Test
hunit name io = Chell.test name chell_io where
	chell_io _ = do
		result <- performTestCase io
		return $ case result of
			Nothing -> Chell.TestPassed []
			Just err -> parseError err
	parseError (True, msg) = Chell.TestFailed [] [Chell.failure { Chell.failureMessage = msg }]
	parseError (False, msg) = Chell.TestAborted [] msg
