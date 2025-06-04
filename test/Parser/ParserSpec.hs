module Parser.ParserSpec (tests) where

import Control.Monad (liftM)
import Parser
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Text.Parsec (ParseError, parse)

tests =
  [ testGroup "Unit tests" unitTests,
    testGroup "Property-based tests" propertyTests
  ]

parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
  case parser input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right result -> assertEqual ("Parsing: " ++ input) expected result

-- Unit tests

test_INIT :: Assertion
test_INIT = parseTest parseProgram "INIT 402" (Program [InitQubit 402])

test_invalidINIT :: Assertion
test_invalidINIT =
  case parseProgram "INIT " of
    Left _ -> return ()
    Right _ -> assertFailure "Should fail to initialize without specifying the number of qubits"

test_PRINT :: Assertion
test_PRINT = parseTest parseProgram "PRINT \"\"" (Program [Print ""])

test_MEASURE :: Assertion
test_MEASURE = parseTest parseProgram "MEASURE 1 -> result_1" (Program [Measure 1 "result_1"])

test_REPEAT :: Assertion
test_REPEAT =
  parseTest
    parseProgram
    "REPEAT 2 {PRINT \"w 1 31 adsa w    ,.! a\"\nINIT 2\nHADAMARD 3}"
    (Program [Repeat 2 [Print "w 1 31 adsa w    ,.! a", InitQubit 2, Hadamard 3]])

test_IF :: Assertion
test_IF =
  parseTest
    parseProgram
    "IF result {PRINT \"pointless comment\"\nPAULIX 5\nPAULIZ -1}"
    (Program [If "result" [Print "pointless comment", PauliX 5, PauliZ (-1)]])

test_CNOT :: Assertion
test_CNOT = parseTest parseProgram "CNOT 0   - 3" (Program [CNOT 0 (-3)])

test_PAULIX :: Assertion
test_PAULIX = parseTest parseProgram "PAULIX 01" (Program [PauliX 1])

test_PHASE :: Assertion
test_PHASE = parseTest parseProgram "PHASE 0.0201 2" (Program [Phase 0.0201 2])

-- Property test

genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a' .. 'z']
  rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
  return (first : take 10 rest)

genStringLit :: Gen String
genStringLit =
  resize 20 $
    listOf $
      elements $
        ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ', '!', '?', '.', ',']

genSmallInt :: Gen Int
genSmallInt = choose (-100, 100)

genSmallPositiveInt :: Gen Int
genSmallPositiveInt = choose (1, 100)

genSmallDouble :: Gen Double
genSmallDouble = choose (-100.00, 100.00)

genPauliGate :: Gen (String, Statement)
genPauliGate = do
  n <- genSmallInt
  oneof
    [ return ("PAULIX " ++ show n, PauliX n),
      return ("PAULIY " ++ show n, PauliY n),
      return ("PAULIZ " ++ show n, PauliZ n)
    ]

prop_INIT :: Property
prop_INIT = forAll genSmallInt $ \n ->
  let val = n
      prog = Program [InitQubit val]
      src = "INIT " ++ show n
   in parseProgram src === Right prog

prop_HADAMARD :: Property
prop_HADAMARD = forAll genSmallInt $ \n ->
  let val = n
      prog = Program [Hadamard val]
      src = "HADAMARD " ++ show n
   in parseProgram src === Right prog

prop_PAULI :: Property
prop_PAULI = forAll genPauliGate $ \(src, stmt) ->
  parseProgram src === Right (Program [stmt])

prop_CNOT = forAll genSmallInt $ \n ->
  forAll genSmallInt $ \m ->
    let val1 = n
        val2 = m
        prog = Program [CNOT val1 val2]
        src = "CNOT " ++ show val1 ++ " " ++ show val2
     in parseProgram src === Right prog

prop_PHASE :: Property
prop_PHASE = forAll genSmallDouble $ \n ->
  forAll genSmallInt $ \m ->
    let doubleVal = n
        intVal = m
        prog = Program [Phase doubleVal intVal]
        src = "PHASE " ++ show doubleVal ++ " " ++ show intVal
     in parseProgram src === Right prog

prop_PRINT :: Property
prop_PRINT = forAll genStringLit $ \msg ->
  let prog = Program [Print msg]
      src = "PRINT \"" ++ msg ++ "\""
   in parseProgram src === Right prog

prop_MEASURE :: Property
prop_MEASURE = forAll genIdentifier $ \var ->
  forAll genSmallPositiveInt $ \val ->
    let prog = Program [Measure val var]
        src = "MEASURE " ++ show val ++ " -> " ++ var
     in parseProgram src == Right prog

prop_Whitespace :: Property
prop_Whitespace = forAll genSmallInt $ \n ->
  let compact = "INIT " ++ show n
      spaced = " INIT       " ++ show n
   in parseProgram compact === parseProgram spaced

prop_Comment :: Property
prop_Comment = forAll genSmallInt $ \n ->
  forAll genStringLit $ \msg ->
    let prog = Program [Hadamard n]
        src = "HADAMARD " ++ show n ++ " //" ++ msg
     in parseProgram src === Right prog

-- Test groups

unitTests =
  [ testCase "Qubit initialization" test_INIT,
    testCase "Invalid Qubit initialization" test_invalidINIT,
    testCase "Empty print" test_PRINT,
    testCase "Measure variable name" test_MEASURE,
    testCase "Complex repeat test" test_REPEAT,
    testCase "Complex if test" test_IF,
    testCase "Whitespaces between values" test_CNOT,
    testCase "Unnecessary zeros in front of a value" test_PAULIX,
    testCase "Phase gate negative angle test" test_PHASE
  ]

propertyTests =
  [ testProperty "Integers initialize correctly" prop_INIT,
    testProperty "Hadamard gate test" prop_HADAMARD,
    testProperty "Pauli gate test" prop_PAULI,
    testProperty "CNot gate test" prop_CNOT,
    testProperty "Doubles parsing in phase gate" prop_PHASE,
    testProperty "String parsing in print" prop_PRINT,
    testProperty "Identifier parsing in measure" prop_MEASURE,
    testProperty "Whitespace is insignificant" prop_Whitespace,
    testProperty "Comments are ignored" prop_Comment
  ]
