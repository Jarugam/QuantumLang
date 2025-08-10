{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Semantics where

import Control.Monad
import Control.Monad.State
import Data.Complex
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix
import qualified Data.Vector as V
import Parser
import System.Random

-- Runtime values
data Value
  = VInt Int
  | VString String
  | VQubit [Complex Double]
  deriving (Eq)

instance Show Value where
  show (VInt n) = show n
  show (VString s) = s
  show (VQubit vector) = show vector

data Environment = Env
  { vars :: Map String Int,
    qubits :: [[Complex Double]],
    entangledId :: [[Int]]
  }

instance Show Environment where
  show :: Environment -> String
  show Env {vars = vars', qubits = qubits', entangledId = entangledId'} =
    "=== Environment ===\nvars: "
      ++ show vars'
      ++ "\nquibits: "
      ++ show qubits'
      ++ "\nlist of entangled quibits"
      ++ show entangledId'

type EvalM a = StateT Environment IO a

identityMatrix :: [[Complex Double]]
identityMatrix =
  [ [1 :+ 0, 0 :+ 0],
    [1 :+ 0, 0 :+ 0]
  ]

runEval :: Environment -> EvalM a -> IO (a, Environment)
runEval env eval = runStateT eval env

execStmt :: Statement -> EvalM ()
execStmt stmt = do
  case stmt of
    InitQubit n -> do
      if n < 1
        then liftIO $ putStrLn "Invalid ammount of initialized qubits"
        else do
          env <- get
          let newQubits = replicate n initQubit
              lastId = length $ entangledId env
              updatedQubits = qubits env ++ newQubits
              newEntanglement = map (: []) $ take n $ drop lastId [0 ..]
              updatedEntanglement = entangledId env ++ newEntanglement
          put env {qubits = updatedQubits, entangledId = updatedEntanglement}
          liftIO $ putStrLn $ "Initialized " ++ show n ++ " qubits"
    Hadamard qubitId -> do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let hadamard =
                [ [1 / sqrt 2 :+ 0, 1 / sqrt 2 :+ 0],
                  [1 / sqrt 2 :+ 0, (-(1 / sqrt 2)) :+ 0]
                ]
          if length (entangledId env !! qubitId) == 1
            then do
              applySingleGate "Hadamard" (fromLists hadamard) qubitId
            else do
              let entanglementList = entangledId env !! qubitId
                  oldQubit = qubits env !! qubitId
                  largeHadamard = scaleGateMatrix hadamard qubitId entanglementList
                  newQubit = applyGate (fromLists largeHadamard) oldQubit
                  newQubitList = updateQubitList entanglementList newQubit (qubits env)
              put env {qubits = newQubitList}
              liftIO $ putStrLn $ "Hadamard gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"
    PauliX qubitId -> do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let pauliX =
                [ [0 :+ 0, 1 :+ 0],
                  [1 :+ 0, 0 :+ 0]
                ]
          if length (entangledId env !! qubitId) == 1
            then do
              applySingleGate "Pauli X" (fromLists pauliX) qubitId
            else do
              let entanglementList = entangledId env !! qubitId
                  oldQubit = qubits env !! qubitId
                  largePauliX = scaleGateMatrix pauliX qubitId entanglementList
                  newQubit = applyGate (fromLists largePauliX) oldQubit
                  newQubitList = updateQubitList entanglementList newQubit (qubits env)
              put env {qubits = newQubitList}
              liftIO $ putStrLn $ "Pauli X gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"
    PauliY qubitId -> do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let pauliY =
                [ [0 :+ 0, 0 :+ 1],
                  [0 :+ (-1), 0 :+ 0]
                ]
          if length (entangledId env !! qubitId) == 1
            then do
              applySingleGate "Pauli Y" (fromLists pauliY) qubitId
            else do
              let entanglementList = entangledId env !! qubitId
                  oldQubit = qubits env !! qubitId
                  largePauliY = scaleGateMatrix pauliY qubitId entanglementList
                  newQubit = applyGate (fromLists largePauliY) oldQubit
                  newQubitList = updateQubitList entanglementList newQubit (qubits env)
              put env {qubits = newQubitList}
              liftIO $ putStrLn $ "Pauli Y gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"
    PauliZ qubitId -> do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let pauliZ =
                [ [1 :+ 0, 0 :+ 0],
                  [0 :+ 0, (-1) :+ 0]
                ]
          if length (entangledId env !! qubitId) == 1
            then do
              applySingleGate "Pauli Z" (fromLists pauliZ) qubitId
            else do
              let entanglementList = entangledId env !! qubitId
                  oldQubit = qubits env !! qubitId
                  largePauliZ = scaleGateMatrix pauliZ qubitId entanglementList
                  newQubit = applyGate (fromLists largePauliZ) oldQubit
                  newQubitList = updateQubitList entanglementList newQubit (qubits env)
              put env {qubits = newQubitList}
              liftIO $ putStrLn $ "Pauli Z gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"
    Phase angle qubitId -> do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let rotationVal = cis angle
              phaseMat =
                [ [1 :+ 0, 0 :+ 0],
                  [0 :+ 0, rotationVal]
                ]
          if length (entangledId env !! qubitId) == 1
            then do
              applySingleGate "Phase" (fromLists phaseMat) qubitId
            else do
              let entanglementList = entangledId env !! qubitId
                  oldQubit = qubits env !! qubitId
                  largePhase = scaleGateMatrix phaseMat qubitId entanglementList
                  newQubit = applyGate (fromLists largePhase) oldQubit
                  newQubitList = updateQubitList entanglementList newQubit (qubits env)
              put env {qubits = newQubitList}
              liftIO $ putStrLn $ "Phase gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"
    CNOT controlQubitId targetQubitId -> do
      env <- get
      if (length (qubits env) <= controlQubitId || controlQubitId < 0)
        || (length (qubits env) <= targetQubitId || targetQubitId < 0)
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show controlQubitId ++ " , or ID = " ++ show targetQubitId
        else do
          if targetQubitId `elem` entangledId env !! controlQubitId
            then
              liftIO $ putStrLn "Error: CNOT gate cannot be used with control and target set as the same qubit"
            else do
              let qubitList = qubits env
                  controlQubit = qubitList !! controlQubitId
                  targetQubit = qubitList !! targetQubitId
              if isBasisState controlQubit && isBasisState targetQubit
                then do
                  case magnitude $ last controlQubit of
                    1 -> do
                      let xGate = fromLists [[0 :+ 0, 1 :+ 0], [1 :+ 0, 0 :+ 0]]
                          newTarget = applyGate xGate targetQubit
                          newQubits = replace targetQubitId newTarget qubitList
                      put env {qubits = newQubits}
                      liftIO $ putStrLn $ "Cnot gate applied with cotrol = " ++ show controlQubitId ++ " and target = " ++ show targetQubitId
                    _ -> liftIO $ putStrLn "Qubits left unchanged (ket 0 as control case)"
                else do
                  let productVector = tensorProduct controlQubit targetQubit
                      newVector = applyCnot productVector
                      oldEntanglement = entangledId env
                      controlEntanglement = oldEntanglement !! controlQubitId
                      targetEntanglement = oldEntanglement !! targetQubitId
                      newEntanglement = updateEntanglementList (controlEntanglement ++ targetEntanglement) oldEntanglement
                      oldQubits = qubits env
                      newQubits = updateQubitList (controlEntanglement ++ targetEntanglement) newVector oldQubits
                  put env {qubits = newQubits, entangledId = newEntanglement}
                  liftIO $ putStrLn $ "Cnot gate applied with cotrol = " ++ show controlQubitId ++ " and target = " ++ show targetQubitId
    Measure qubitId varName -> do
      env <- get
      if qubitId < 0 || qubitId >= length (qubits env)
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          if length (entangledId env !! qubitId) == 1
            then do
              let qubit = qubits env !! qubitId
                  alpha = head qubit
                  pAlpha = magnitude alpha ^ 2
              r <- liftIO $ randomRIO (0.0, 1.0)
              let (result, measuredState) =
                    if r < pAlpha
                      then (0, [1 :+ 0, 0 :+ 0])
                      else (1, [0 :+ 0, 1 :+ 0])
                  oldQubits = qubits env
                  oldVariables = vars env
                  newQubits = replace qubitId measuredState oldQubits
                  newVars = Map.insert varName result oldVariables
              put env {qubits = newQubits, vars = newVars}
              liftIO $ putStrLn $ "Measured qubit " ++ show qubitId ++ ", as base state: " ++ show result ++ "\nValue saved in variable: " ++ varName
            else do
              let qubitList = qubits env !! qubitId
                  qubitProbabilityList = map (\x -> magnitude x ^ 2) qubitList
              r <- liftIO $ randomRIO (0.0, 1.0)
              let stateIndex = findNewStateIndex qubitProbabilityList r
                  binList = toBinary stateIndex (length (entangledId env !! qubitId))
                  result = findResult binList (entangledId env !! qubitId) qubitId
                  oldVariables = vars env
                  newVars = Map.insert varName result oldVariables
                  oldQubits = qubits env
                  newQubitsEntangled = map (\x -> if x == 0 then [1 :+ 0, 0 :+ 0] else [0 :+ 0, 1 :+ 0]) binList
                  newQubits = disentangleQubits newQubitsEntangled oldQubits (entangledId env !! qubitId)
              put env {qubits = newQubits, vars = newVars}
              liftIO $ putStrLn $ "Measured qubit " ++ show qubitId ++ ", as base state: " ++ show result ++ "\nValue saved in variable: " ++ varName ++ " (entanglement updated)"
    Print varName -> do
      env <- get
      let allVars = vars env
      case Map.lookup varName allVars of
        Nothing -> liftIO $ putStrLn $ "No variable found with the name: " ++ varName
        Just value -> liftIO $ putStrLn $ "Printing variable: " ++ show varName ++ "\nvalue of variable: " ++ show value
    If varName instruction -> do
      env <- get
      let allVars = vars env
      case Map.lookup varName allVars of
        Nothing -> liftIO $ putStrLn $ "No variable found with the name: " ++ varName
        Just 0 -> liftIO $ putStrLn "If condintion was not fulfilled"
        Just _ -> do
          liftIO $ putStrLn "If condintion fulfilled, executing instructions...\n\n\t---=== If block start ===---"
          execProgram instruction
          liftIO $ putStrLn "\t---=== If block end ===---"
    Repeat count instruction -> do
      if count < 0
        then liftIO $ putStrLn $ "A set of instructions cannot be repeated " ++ show count ++ " times"
        else do
          liftIO $ putStrLn $ "Repating instructions " ++ show count ++ " times\n\n\t---=== Repeat block start ===---"
          replicateM_ count $ execProgram instruction
          liftIO $ putStrLn "\t---=== Repeat block end ===---"

  currentState <- get
  liftIO $ putStrLn "\n=== Environment Status === \n--------------------------------------------"
  liftIO $ putStrLn $ "Current state vector: " ++ show (qubits currentState) ++ "\n--------------------------------------------"
  liftIO $ putStrLn $ "Current entanglements: " ++ show (entangledId currentState) ++ "\n--------------------------------------------"
  liftIO $ putStrLn $ "Current variables: " ++ show (vars currentState) ++ "\n--------------------------------------------\n"
  where
    initQubit :: [Complex Double]
    initQubit = [1 :+ 0, 0 :+ 0]

    replace :: Int -> c -> [c] -> [c]
    replace index' newelem = zipWith (\index elem' -> (if index == index' then newelem else elem')) [0 ..]

    applyGate :: Matrix (Complex Double) -> [Complex Double] -> [Complex Double]
    applyGate gateMatrix qubitVector = toList $ multStd gateMatrix (colVector (V.fromList qubitVector))

    applySingleGate :: String -> Matrix (Complex Double) -> Int -> EvalM ()
    applySingleGate gateName gateMatrix qubitId = do
      env <- get
      if length (qubits env) <= qubitId || qubitId < 0
        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
        else do
          let qubit = qubits env !! qubitId
              transformedQubit = applyGate gateMatrix qubit
          put env {qubits = replace qubitId transformedQubit (qubits env)}
          liftIO $ putStrLn $ gateName ++ " gate used on " ++ show qubitId ++ "-th qubit"

    isBasisState :: [Complex Double] -> Bool
    isBasisState [a, b] = magnitude a == 1 && magnitude b == 0 || magnitude a == 0 && magnitude b == 1
    isBasisState _ = False

    tensorProduct :: [Complex Double] -> [Complex Double] -> [Complex Double]
    tensorProduct [] _ = []
    tensorProduct _ [] = []
    tensorProduct (x : xs) vector2 =
      let y = map (x *) vector2
          ys = tensorProduct xs vector2
       in y ++ ys

    matrixTensorProduct :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
    matrixTensorProduct matrix1 matrix2 = goMatrix matrix1 matrix2 []

    goMatrix :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
    goMatrix [] _ prod = prod
    goMatrix (x : xs) matrix2' prod =
      let y = map (tensorProduct x) matrix2'
       in goMatrix xs matrix2' (prod ++ y)

    scaleGateMatrix :: [[Complex Double]] -> Int -> [Int] -> [[Complex Double]]
    scaleGateMatrix _ _ [] = []
    scaleGateMatrix gateMatrix qId [x] =
      if qId == x
        then gateMatrix
        else identityMatrix
    scaleGateMatrix gateMatrix qId (x : xs) =
      if qId == x
        then matrixTensorProduct gateMatrix (scaleGateMatrix gateMatrix qId xs)
        else matrixTensorProduct identityMatrix (scaleGateMatrix gateMatrix qId xs)

    updateEntanglementList :: [Int] -> [[Int]] -> [[Int]]
    updateEntanglementList newEntangledId entangledList =
      [ if i `elem` newEntangledId then newEntangledId else oldEntanglement
        | (i, oldEntanglement) <- zip [0 ..] entangledList
      ]

    updateQubitList :: [Int] -> [Complex Double] -> [[Complex Double]] -> [[Complex Double]]
    updateQubitList newEntangledId newEntangledQubit qubitList =
      [ if i `elem` newEntangledId then newEntangledQubit else oldQubitList
        | (i, oldQubitList) <- zip [0 ..] qubitList
      ]

    applyCnot :: [Complex Double] -> [Complex Double]
    applyCnot qubit = goCnot qubit [] []

    goCnot :: [Complex Double] -> [Complex Double] -> [Complex Double] -> [Complex Double]
    goCnot [] firstHalf secondHalf = firstHalf ++ secondHalf
    goCnot (x : xs) firstHalf secondHalf =
      if length (x : xs) > length firstHalf
        then goCnot xs (firstHalf ++ [x]) secondHalf
        else goCnot (tail xs) firstHalf (secondHalf ++ [head xs, x])

    findNewStateIndex :: [Double] -> Double -> Int
    findNewStateIndex probList prob = goFind probList prob 0

    goFind :: [Double] -> Double -> Int -> Int
    goFind [] _ _ = undefined
    goFind (x : xs) p elId =
      if p <= x
        then elId
        else goFind xs (p - x) (elId + 1)

    toBinary :: Int -> Int -> [Int]
    toBinary value size = goBinary value size []

    goBinary :: Int -> Int -> [Int] -> [Int]
    goBinary _ 0 acc = acc
    goBinary 0 s acc = goBinary 0 (s - 1) [0] ++ acc
    goBinary val s acc = goBinary (val `div` 2) (s - 1) [val `mod` 2] ++ acc

    findResult :: [Int] -> [Int] -> Int -> Int
    findResult [] _ _ = undefined
    findResult _ [] _ = undefined
    findResult (x : xs) (y : ys) qId =
      if qId == y
        then x
        else findResult xs ys qId

    disentangleQubits :: [[Complex Double]] -> [[Complex Double]] -> [Int] -> [[Complex Double]]
    disentangleQubits [] oldQubitList _ = oldQubitList
    disentangleQubits _ oldQubitList [] = oldQubitList
    disentangleQubits (x : xs) oldQubitList (y : ys) = disentangleQubits xs (replace y x oldQubitList) ys

execProgram :: [Statement] -> EvalM ()
execProgram = mapM_ execStmt

runProgram :: [Statement] -> IO ((), Environment)
runProgram list = do
  putStrLn "Running the program...\n--------------------------------------------"
  runStateT (execProgram list) initialEnv
  where
    initialEnv = Env {vars = Map.empty, qubits = [], entangledId = []}

evalAndPrint :: String -> [Statement] -> IO ()
evalAndPrint description instruction = do
  result <- runStateT (execProgram instruction) initialEnv
  putStrLn $ description ++ ": \n" ++ show result
  where
    initialEnv = Env {vars = Map.empty, qubits = [], entangledId = []}

testEvaluator :: IO ((), Environment)
testEvaluator = do
  liftIO $ putStrLn "=== Testing Expression Evaluator ==="

  -- Test basic expressions
  liftIO $ evalAndPrint "Quibit initialization" [InitQubit 2]
  liftIO $ evalAndPrint "Hadamard gate" [InitQubit 2, Hadamard 1]
  liftIO $ evalAndPrint "Pauli X gate" [InitQubit 2, PauliX 1]
  liftIO $ evalAndPrint "Pauli Y gate" [InitQubit 2, PauliY 1]
  liftIO $ evalAndPrint "Pauli Z gate" [InitQubit 2, PauliZ 1]
  liftIO $ evalAndPrint "Phase gate" [InitQubit 2, Phase 1 0]

  -- Test simple program
  liftIO $ putStrLn "\n\t=== Running a Simple Program ==="
  let simpleProgram =
        [ InitQubit 2,
          Hadamard 0,
          PauliX 0,
          PauliY 1,
          PauliZ 1,
          Phase 0.4 1,
          CNOT 0 1,
          Measure 0 "var1",
          Measure 1 "var2",
          If "var1" [Print "var2"],
          Repeat 2 [Print "var1"]
        ]

  runProgram simpleProgram

example1 :: [Statement]
example1 = [InitQubit (-1)]
