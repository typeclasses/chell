{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Chell
	(
	
	-- * Main
	  defaultMain
	
	-- ** Tests
	, Test (..)
	, TestOptions (..)
	, TestResult (..)
	, Failure (..)
	, Location (..)
	, skipIf
	, skipWhen
	
	-- ** Suites
	, Suite
	, suite
	, test
	, suiteTests
	
	-- * Basic testing library
	-- $doc-basic-testing
	, Assertion (..)
	, AssertionResult (..)
	, IsAssertion
	, Assertions
	, TestM
	, assertions
	, assert
	, expect
	, Test.Chell.fail
	, trace
	, note
	
	-- ** Assertions
	, equal
	, notEqual
	, equalWithin
	, just
	, nothing
	, throws
	, throwsEq
	, greater
	, greaterEqual
	, lesser
	, lesserEqual
	) where

import qualified Control.Exception
import           Control.Exception (Exception)
import           Control.Monad (foldM, forM_, liftM, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char (ord)
import           Data.List (intercalate)
import           Data.Maybe (isJust, isNothing)
import           Data.IORef (newIORef, readIORef, atomicModifyIORef)
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import qualified System.Console.GetOpt as GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (Handle, stderr, hPutStr, hPutStrLn)
import qualified System.IO as IO
import           System.Random (randomIO)
import           Text.Printf (printf)

import qualified Language.Haskell.TH as TH

data Test = Test Text (TestOptions -> IO TestResult)

data TestOptions = TestOptions
	{ testOptionSeed :: Int
	}

testName :: Test -> Text
testName (Test name _) = name

runTest :: Test -> TestOptions -> IO TestResult
runTest (Test _ io) = io

data TestResult
	= TestPassed [(Text, Text)]
	| TestSkipped
	| TestFailed [(Text, Text)] [Failure]
	| TestAborted [(Text, Text)] Text

data Failure = Failure (Maybe Location) Text

data Location = Location
	{ locationFile :: Text
	, locationModule :: Text
	, locationLine :: Integer
	}

-- | A test which is always skipped. Use this to avoid commenting out tests
-- which are currently broken, or do not work on the current platform.
--
-- @
--tests = 'suite' \"tests\"
--    [ 'test' ('skipIf' onWindows test_WindowsSpecific)
--    ]
-- @
--
skipIf :: Bool -> Test -> Test
skipIf skip t@(Test name _) = if skip
	then Test name (\_ -> return TestSkipped)
	else t

-- | Potentially skip a test, depending on the result of a runtime check.
--
-- @
--tests = 'suite' \"tests\"
--    [ 'test' ('skipWhen' noNetwork test_PingGoogle)
--    ]
-- @
skipWhen :: IO Bool -> Test -> Test
skipWhen p (Test name io) = Test name $ \options -> do
	skipThis <- p
	if skipThis
		then return TestSkipped
		else io options

-- | Running a 'Test' requires it to be contained in a 'Suite'. This gives
-- the test a name, so users know which test failed.
data Suite = Suite Text [Suite]
           | SuiteTest Test

test :: Test -> Suite
test = SuiteTest

suite :: Text -> [Suite] -> Suite
suite = Suite

suiteName :: Suite -> Text
suiteName (Suite name _) = name
suiteName (SuiteTest t) = testName t

-- | The full list of 'Test's contained within this 'Suite'. Each 'Test'
-- is returned with its name modified to include the name of its parent
-- 'Suite's.
suiteTests :: Suite -> [Test]
suiteTests = loop "" where
	loop prefix s = let
		name = if Data.Text.null prefix
			then suiteName s
			else Data.Text.concat [prefix, ".", suiteName s]
		in case s of
			Suite _ suites -> concatMap (loop name) suites
			SuiteTest (Test _ io) -> [Test name io]

-- $doc-basic-testing
--
-- This library includes a few basic JUnit-style assertions, for use in
-- simple test suites where depending on a separate test framework is too
-- much trouble.

newtype Assertion = Assertion (IO AssertionResult)

data AssertionResult
	= AssertionPassed
	| AssertionFailed Text

class IsAssertion a where
	toAssertion :: a -> Assertion

instance IsAssertion Assertion where
	toAssertion = id

instance IsAssertion Bool where
	toAssertion x = Assertion (return (if x
		then AssertionPassed
		else AssertionFailed "boolean assertion failed"))

type Assertions = TestM ()
type TestState = ([(Text, Text)], [Failure])
newtype TestM a = TestM { unTestM :: TestState -> IO (Maybe a, TestState) }

instance Functor TestM where
	fmap = liftM

instance Monad TestM where
	return x = TestM (\s -> return (Just x, s))
	m >>= f = TestM (\s -> do
		(maybe_a, s') <- unTestM m s
		case maybe_a of
			Nothing -> return (Nothing, s')
			Just a -> unTestM (f a) s')

instance MonadIO TestM where
	liftIO io = TestM (\s -> do
		x <- io
		return (Just x, s))

-- | Convert a sequence of pass/fail assertions into a runnable test.
--
-- @
-- test_Equality :: Test
-- test_Equality = assertions \"equality\" $ do
--     $assert (1 == 1)
--     $assert (equal 1 1)
-- @
assertions :: Text -> Assertions -> Test
assertions name testm = Test name io where
	io _ = do
		tried <- Control.Exception.try (unTestM testm ([], []))
		return $ case tried of
			Left exc -> TestAborted [] (errorExc exc)
			Right (_, (notes, [])) -> TestPassed (reverse notes)
			Right (_, (notes, fs)) -> TestFailed (reverse notes) (reverse fs)
	
	errorExc :: Control.Exception.SomeException -> Text
	errorExc exc = Data.Text.pack ("Test aborted due to exception: " ++ show exc)

addFailure :: Maybe TH.Loc -> Bool -> Text -> Assertions
addFailure maybe_loc fatal msg = TestM $ \(notes, fs) -> do
	let loc = do
		th_loc <- maybe_loc
		return $ Location
			{ locationFile = Data.Text.pack (TH.loc_filename th_loc)
			, locationModule = Data.Text.pack (TH.loc_module th_loc)
			, locationLine = toInteger (fst (TH.loc_start th_loc))
			}
	return ( if fatal then Nothing else Just ()
	       , (notes, Failure loc msg : fs))

-- | Cause a test to immediately fail, with a message.
--
-- 'fail' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $fail :: 'Text' -> 'Assertions'
-- @
fail :: TH.Q TH.Exp -- :: Text -> Assertions
fail = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| addFailure (Just $qloc) True |]

-- | Print a message from within a test. This is just a helper for debugging,
-- so you don't have to import @Debug.Trace@.
trace :: Text -> Assertions
trace msg = liftIO (Data.Text.IO.putStrLn msg)

-- | Attach metadata to a test run. This will be included in reports.
note :: Text -> Text -> Assertions
note key value = TestM (\(notes, fs) -> return (Just (), ((key, value) : notes, fs)))

liftLoc :: TH.Loc -> TH.Q TH.Exp
liftLoc loc = [| TH.Loc filename package module_ start end |] where
	filename = TH.loc_filename loc
	package = TH.loc_package loc
	module_ = TH.loc_module loc
	start = TH.loc_start loc
	end = TH.loc_end loc

assertAt :: IsAssertion assertion => TH.Loc -> Bool -> assertion -> Assertions
assertAt loc fatal assertion = do
	let Assertion io = toAssertion assertion
	result <- liftIO io
	case result of
		AssertionPassed -> return ()
		AssertionFailed err -> addFailure (Just loc) fatal err

-- | Run an 'Assertion'. If the assertion fails, the test will immediately
-- fail.
--
-- 'assert' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $assert :: 'IsAssertion' assertion => assertion -> 'Assertions'
-- @
assert :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions
assert = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc True |]

-- | Run an 'Assertion'. If the assertion fails, the test will continue to
-- run until it finishes (or until an 'assert' fails).
--
-- 'expect' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $expect :: 'IsAssertion' assertion => assertion -> 'Assertions'
-- @
expect :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions
expect = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc False |]

data Option
	= OptionHelp
	| OptionVerbose
	| OptionXmlReport FilePath
	| OptionJsonReport FilePath
	| OptionLog FilePath
	| OptionSeed Int
	deriving (Show, Eq)

optionInfo :: [GetOpt.OptDescr Option]
optionInfo =
	[ GetOpt.Option ['h'] ["help"]
	  (GetOpt.NoArg OptionHelp)
	  "show this help"
	
	, GetOpt.Option ['v'] ["verbose"]
	  (GetOpt.NoArg OptionVerbose)
	  "print more output"
	
	, GetOpt.Option [] ["xml-report"]
	  (GetOpt.ReqArg OptionXmlReport "PATH")
	  "write a parsable report to a file, in XML"
	
	, GetOpt.Option [] ["json-report"]
	  (GetOpt.ReqArg OptionJsonReport "PATH")
	  "write a parsable report to a file, in JSON"
	
	, GetOpt.Option [] ["log"]
	  (GetOpt.ReqArg OptionLog "PATH")
	  "write a full log (always max verbosity) to a file path"
	
	, GetOpt.Option [] ["seed"]
	  (GetOpt.ReqArg (\s -> case parseInt s of
	   	Just x -> OptionSeed x
	   	Nothing -> error ("Failed to parse --seed value " ++ show s)) "SEED")
	  "the seed used for random numbers in (for example) quickcheck"
	
	]

parseInt :: String -> Maybe Int
parseInt s = case [x | (x, "") <- reads s] of
	[x] -> Just x
	_ -> Nothing

getSeedOpt :: [Option] -> Maybe Int
getSeedOpt [] = Nothing
getSeedOpt ((OptionSeed s) : _) = Just s
getSeedOpt (_:os) = getSeedOpt os

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

-- | A simple default main function, which runs a list of tests and logs
-- statistics to stderr.
defaultMain :: [Suite] -> IO ()
defaultMain suites = do
	args <- getArgs
	let (options, filters, optionErrors) = GetOpt.getOpt GetOpt.Permute optionInfo args
	unless (null optionErrors) $ do
		name <- getProgName
		hPutStrLn stderr (concat optionErrors)
		hPutStrLn stderr (GetOpt.usageInfo (usage name) optionInfo)
		exitFailure
	
	when (OptionHelp `elem` options) $ do
		name <- getProgName
		putStrLn (GetOpt.usageInfo (usage name) optionInfo)
		exitSuccess
	
	let allTests = concatMap suiteTests suites
	let tests = if null filters
		then allTests
		else filter (matchesFilter filters) allTests
	
	seed <- case getSeedOpt options of
		Just s -> return s
		Nothing -> randomIO
	
	let testOptions = TestOptions
		{ testOptionSeed = seed
		}
	
	allPassed <- withReports options $ do
		ReportsM (mapM_ reportStart)
		allPassed <- foldM (\good t -> do
			thisGood <- reportTest testOptions t
			return (good && thisGood)) True tests
		ReportsM (mapM_ reportFinish)
		return allPassed
	
	if allPassed
		then exitSuccess
		else exitFailure

matchesFilter :: [String] -> Test -> Bool
matchesFilter strFilters = check where
	filters = map Data.Text.pack strFilters
	check t = any (matchName (testName t)) filters
	matchName name f = f == name || Data.Text.isPrefixOf (Data.Text.append f ".") name

data Report = Report
	{ reportStart :: IO ()
	, reportStartTest :: Text -> IO ()
	, reportFinishTest :: Text -> TestResult -> IO ()
	, reportFinish :: IO ()
	}

jsonReport :: Handle -> IO Report
jsonReport h = do
	commaRef <- newIORef False
	let comma = do
		needComma <- atomicModifyIORef commaRef (\c -> (True, c))
		if needComma
			then hPutStr h ", "
			else hPutStr h "  "
	let putNotes notes = do
		hPutStr h ", \"notes\": [\n"
		hPutStr h (intercalate "\n, " (do
			(key, value) <- notes
			return (concat
				[ "{\"key\": \""
				, escapeJSON key
				, "\", \"value\": \""
				, escapeJSON value
				, "\"}\n"
				])))
		hPutStrLn h "]"
	return (Report
		{ reportStart = do
			hPutStrLn h "{\"test-runs\": [ "
		, reportStartTest = \name -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStr h "\", \"result\": \""
		, reportFinishTest = \_ result -> case result of
			TestPassed notes -> do
				hPutStr h "passed\""
				putNotes notes
				hPutStrLn h "}"
			TestSkipped -> do
				hPutStrLn h "skipped\"}"
			TestFailed notes fs -> do
				hPutStrLn h "failed\", \"failures\": ["
				hPutStrLn h (intercalate "\n, " (do
					Failure loc msg <- fs
					let locString = case loc of
						Just loc' -> concat
							[ ", \"location\": {\"module\": \""
							, escapeJSON (locationModule loc')
							, "\", \"file\": \""
							, escapeJSON (locationFile loc')
							, "\", \"line\": "
							, show (locationLine loc')
							, "}"
							]
						Nothing -> ""
					return ("{\"message\": \"" ++ escapeJSON msg ++ "\"" ++ locString ++ "}")))
				hPutStr h "]"
				putNotes notes
				hPutStrLn h "}"
			TestAborted notes msg -> do
				hPutStr h "aborted\", \"abortion\": {\"message\": \""
				hPutStr h (escapeJSON msg)
				hPutStr h "\"}"
				putNotes notes
				hPutStrLn h "}"
		, reportFinish = do
			hPutStrLn h "]}"
		})

escapeJSON :: Text -> String
escapeJSON = concatMap (\c -> case c of
	'"' -> "\\\""
	'\\' -> "\\\\"
	_ | ord c <= 0x1F -> printf "\\u%04X" (ord c)
	_ -> [c]) . Data.Text.unpack

xmlReport :: Handle -> Report
xmlReport h = Report
	{ reportStart = do
		hPutStrLn h "<?xml version=\"1.0\" encoding=\"utf8\"?>"
		hPutStrLn h "<report xmlns='urn:john-millikin:chell:report:1'>"
	, reportStartTest = \name -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStr h "' result='"
	, reportFinishTest = \_ result -> case result of
		TestPassed notes -> do
			hPutStrLn h "passed'>"
			putNotes notes
			hPutStrLn h "\t</test-run>"
		TestSkipped -> do
			hPutStrLn h "skipped'/>"
		TestFailed notes fs -> do
			hPutStrLn h "failed'>"
			forM_ fs $ \(Failure loc msg) -> do
				hPutStr h "\t\t<failure message='"
				hPutStr h (escapeXML msg)
				case loc of
					Just loc' -> do
						hPutStrLn h "'>"
						hPutStr h "\t\t\t<location module='"
						hPutStr h (escapeXML (locationModule loc'))
						hPutStr h "' file='"
						hPutStr h (escapeXML (locationFile loc'))
						hPutStr h "' line='"
						hPutStr h (show (locationLine loc'))
						hPutStrLn h "'/>"
						hPutStrLn h "\t\t</failure>"
					Nothing -> hPutStrLn h "'/>"
			putNotes notes
			hPutStrLn h "\t</test-run>"
		TestAborted notes msg -> do
			hPutStrLn h "aborted'>"
			hPutStr h "\t\t<abortion message='"
			hPutStr h (escapeXML msg)
			hPutStrLn h "'/>"
			putNotes notes
			hPutStrLn h "\t</test-run>"
	, reportFinish = do
		hPutStrLn h "</report>"
	} where
		putNotes notes = forM_ notes $ \(key, value) -> do
			hPutStr h "\t\t<note key=\""
			hPutStr h (escapeXML key)
			hPutStr h "\" value=\""
			hPutStr h (escapeXML value)
			hPutStrLn h "\"/>"

escapeXML :: Text -> String
escapeXML = concatMap (\c -> case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	'"' -> "&quot;"
	'\'' -> "&apos;"
	_ -> [c]) . Data.Text.unpack

textReport :: Bool -> Handle -> IO Report
textReport verbose h = do
	countPassed <- newIORef (0 :: Integer)
	countSkipped <- newIORef (0 :: Integer)
	countFailed <- newIORef (0 :: Integer)
	countAborted <- newIORef (0 :: Integer)
	
	let incRef ref = atomicModifyIORef ref (\a -> (a + 1, ()))
	
	let putNotes notes = forM_ notes $ \(key, value) -> do
		Data.Text.IO.hPutStr h key
		hPutStr h "="
		Data.Text.IO.hPutStrLn h value
	
	return (Report
		{ reportStart = return ()
		, reportStartTest = \_ -> return ()
		, reportFinishTest = \name result -> case result of
			TestPassed notes -> do
				when verbose $ do
					hPutStrLn h (replicate 70 '=')
					hPutStr h "PASSED: "
					Data.Text.IO.hPutStrLn h name
					putNotes notes
					hPutStr h "\n"
				incRef countPassed
			TestSkipped -> do
				when verbose $ do
					hPutStrLn h (replicate 70 '=')
					hPutStr h "SKIPPED: "
					Data.Text.IO.hPutStrLn h name
					hPutStr h "\n"
				incRef countSkipped
			TestFailed notes fs -> do
				hPutStrLn h (replicate 70 '=')
				hPutStr h "FAILED: "
				Data.Text.IO.hPutStrLn h name
				putNotes notes
				hPutStrLn h (replicate 70 '-')
				forM_ fs $ \(Failure loc msg) -> do
					case loc of
						Just loc' -> do
							Data.Text.IO.hPutStr h (locationFile loc')
							hPutStr h ":"
							hPutStrLn h (show (locationLine loc'))
						Nothing -> return ()
					Data.Text.IO.hPutStrLn h msg
					hPutStr h "\n"
				incRef countFailed
			TestAborted notes msg -> do
				hPutStrLn h (replicate 70 '=')
				hPutStr h "ABORTED: "
				Data.Text.IO.hPutStrLn h name
				putNotes notes
				hPutStrLn h (replicate 70 '-')
				Data.Text.IO.hPutStrLn h msg
				hPutStr h "\n"
				incRef countAborted
		, reportFinish = do
			n_passed <- readIORef countPassed
			n_skipped <- readIORef countSkipped
			n_failed <- readIORef countFailed
			n_aborted <- readIORef countAborted
			if n_failed == 0 && n_aborted == 0
				then hPutStr h "PASS: "
				else hPutStr h "FAIL: "
			let putNum comma n what = hPutStr h $ if n == 1
				then comma ++ "1 test " ++ what
				else comma ++ show n ++ " tests " ++ what
			
			let total = sum [n_passed, n_skipped, n_failed, n_aborted]
			putNum "" total "run"
			when (n_passed > 0) (putNum ", " n_passed "passed")
			when (n_skipped > 0) (putNum ", " n_skipped "skipped")
			when (n_failed > 0) (putNum ", " n_failed "failed")
			when (n_aborted > 0) (putNum ", " n_aborted "aborted")
			hPutStr h "\n"
		})

withReports :: [Option] -> ReportsM a -> IO a
withReports opts reportsm = do
	let loop [] reports = runReportsM reportsm (reverse reports)
	    loop (o:os) reports = case o of
	    	OptionXmlReport path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> loop os (xmlReport h : reports))
	    	OptionJsonReport path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> jsonReport h >>= \r -> loop os (r : reports))
	    	OptionLog path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> textReport True h >>= \r -> loop os (r : reports))
	    	_ -> loop os reports
	
	console <- textReport (OptionVerbose `elem` opts) stderr
	loop opts [console]

newtype ReportsM a = ReportsM { runReportsM :: [Report] -> IO a }

instance Monad ReportsM where
	return x = ReportsM (\_ -> return x)
	m >>= f = ReportsM (\reports -> do
		x <- runReportsM m reports
		runReportsM (f x) reports)

instance MonadIO ReportsM where
	liftIO io = ReportsM (\_ -> io)

reportTest :: TestOptions -> Test -> ReportsM Bool
reportTest options t = do
	let name = testName t
	let notify io = ReportsM (mapM_ io)
	
	notify (\r -> reportStartTest r name)
	result <- liftIO (runTest t options)
	notify (\r -> reportFinishTest r name result)
	return $ case result of
		TestPassed{} -> True
		TestSkipped{} -> True
		TestFailed{} -> False
		TestAborted{} -> False

pure :: Bool -> String -> Assertion
pure True _ = Assertion (return AssertionPassed)
pure False err = Assertion (return (AssertionFailed (Data.Text.pack err)))

-- | Assert that two values are equal.
equal :: (Show a, Eq a) => a -> a -> Assertion
equal x y = pure (x == y) ("equal: " ++ show x ++ " is not equal to " ++ show y)

-- | Assert that two values are not equal.
notEqual :: (Eq a, Show a) => a -> a -> Assertion
notEqual x y = pure (x /= y) ("notEqual: " ++ show x ++ " is equal to " ++ show y)

-- | Assert that two values are within some delta of each other.
equalWithin :: (Real a, Show a) => a -> a
                                -> a -- ^ delta
                                -> Assertion
equalWithin x y delta = pure
	((x - delta <= y) && (x + delta >= y))
	("equalWithin: " ++ show x ++ " is not within " ++ show delta ++ " of " ++ show y)

-- | Assert that some value is @Just@.
just :: Maybe a -> Assertion
just x = pure (isJust x) ("just: received Nothing")

-- | Assert that some value is @Nothing@.
nothing :: Maybe a -> Assertion
nothing x = pure (isNothing x) ("nothing: received Just")

-- | Assert that some computation throws an exception matching the provided
-- predicate. This is mostly useful for exception types which do not have an
-- instance for @Eq@, such as @'Control.Exception.ErrorCall'@.
throws :: Exception err => (err -> Bool) -> IO a -> Assertion
throws p io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if p exc
			then AssertionPassed
			else AssertionFailed (Data.Text.pack ("throws: exception " ++ show exc ++ " did not match predicate"))
		Right _ -> AssertionFailed (Data.Text.pack ("throws: no exception thrown"))))

-- | Assert that some computation throws an exception equal to the given
-- exception. This is better than just checking that the correct type was
-- thrown, because the test can also verify the exception contains the correct
-- information.
throwsEq :: (Eq err, Exception err, Show err) => err -> IO a -> Assertion
throwsEq expected io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if exc == expected
			then AssertionPassed
			else AssertionFailed (Data.Text.pack ("throwsEq: exception " ++ show exc ++ " is not equal to " ++ show expected))
		Right _ -> AssertionFailed (Data.Text.pack ("throwsEq: no exception thrown"))))

-- | Assert a value is greater than another.
greater :: (Ord a, Show a) => a -> a -> Assertion
greater x y = pure (x > y) ("greater: " ++ show x ++ " is not greater than " ++ show y)

-- | Assert a value is greater than or equal to another.
greaterEqual :: (Ord a, Show a) => a -> a -> Assertion
greaterEqual x y = pure (x > y) ("greaterEqual: " ++ show x ++ " is not greater than or equal to " ++ show y)

-- | Assert a value is less than another.
lesser :: (Ord a, Show a) => a -> a -> Assertion
lesser x y = pure (x < y) ("lesser: " ++ show x ++ " is not less than " ++ show y)

-- | Assert a value is less than or equal to another.
lesserEqual :: (Ord a, Show a) => a -> a -> Assertion
lesserEqual x y = pure (x <= y) ("lesserEqual: " ++ show x ++ " is not less than or equal to " ++ show y)
