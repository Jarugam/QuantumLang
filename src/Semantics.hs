{-# LANGUAGE ScopedTypeVariables #-}

module Semantics where

import Parser
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import Data.Map (Map)
import Data.Complex
import Data.Matrix
import qualified Data.Vector as V
import qualified Data.Map as Map
import System.Random
import Text.Read (readMaybe)


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
                    { vars :: Map String Value
                    , qubits :: [[Complex Double]]
                    , entangledId :: [[Int]]}

type EvalM a = StateT Environment IO a

identityMatrix :: [[Complex Double]]
identityMatrix = 
    [ [1 :+ 0, 0 :+ 0]
    , [1 :+ 0, 0 :+ 0]
    ]

runEval :: Environment -> EvalM a -> IO(a, Environment)
runEval env eval = runStateT eval env

execStmt :: Statement -> EvalM ()
execStmt stmt = do
    case stmt of
        InitQubit n -> do
            when (n < 1) $ liftIO $ putStrLn "Invalid ammount of initialized qubits"
            env <- get
            let newQubits = replicate n initQubit
                lastId = length $ entangledId env
                updatedQubits = qubits env ++ newQubits
                newEntanglement = map (: []) $ take n $ drop lastId [0..]
                updatedEntanglement = entangledId env ++ newEntanglement
            put env {qubits = updatedQubits, entangledId = updatedEntanglement}
            liftIO $ putStrLn $ "Initialized " ++ show n ++ " qubits"

        Hadamard qubitId -> do
            env <- get
            let hadamard =
                        [ [1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0]
                        , [1/sqrt 2 :+ 0, (- (1 / sqrt 2)) :+ 0]
                        ]
            if length (entangledId env) == 1 then do
                applySingleGate "Hadamard" (fromLists hadamard) qubitId
            else do
                if length (qubits env) <= qubitId || qubitId < 0 
                    then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
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
            let pauliX =
                        [ [0 :+ 0, 1 :+ 0]
                        , [1 :+ 0, 0 :+ 0]
                        ]
            if length (entangledId env) == 1 then do
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
            let pauliY =
                        [ [0 :+ 0, 0 :+ 1]
                        , [0 :+ (-1), 0 :+ 0]
                        ]
            if length (entangledId env) == 1 then do
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
            let pauliZ =
                        [ [1 :+ 0, 0 :+ 0]
                        , [0 :+ 0, (-1) :+ 0]
                        ]
            if length (entangledId env) == 1 then do
                applySingleGate "Pauli Z" (fromLists pauliZ) qubitId
            else do
                let entanglementList = entangledId env !! qubitId
                    oldQubit = qubits env !! qubitId
                    largePauliZ = scaleGateMatrix pauliZ qubitId entanglementList
                    newQubit = applyGate (fromLists largePauliZ) oldQubit
                    newQubitList = updateQubitList entanglementList newQubit (qubits env)
                put env {qubits = newQubitList}   
                liftIO $ putStrLn $ "Pauli Z gate used on " ++ show qubitId ++ "-th qubit (entanglement updated)"

        CNOT controlQubitId targetQubitId -> do
            env <- get
            if targetQubitId `elem` entangledId env !! controlQubitId then
                liftIO $ putStrLn "Error: CNOT gate cannot be used with control and target set as the same qubit"
                else do
                    if length (qubits env) <= controlQubitId || controlQubitId < 0 ||
                        length (qubits env) <= targetQubitId || targetQubitId < 0
                        then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show controlQubitId ++ " , or ID = " ++ show targetQubitId
                    else do
                        let qubitList = qubits env
                            controlQubit = qubitList !! controlQubitId
                            targetQubit = qubitList !! targetQubitId
                        if isBasisState controlQubit && isBasisState targetQubit then do
                            case magnitude $ last controlQubit of
                                1 -> do
                                    let xGate = fromLists [ [0 :+ 0, 1 :+ 0], [1 :+ 0, 0 :+ 0]]
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

        _ -> undefined


    currentState <- get
    liftIO $ putStrLn $ "Current state vector: " ++ show (qubits currentState) ++ "\n--------------------------------------------"
    liftIO $ putStrLn $ "Current entanglements: " ++ show (entangledId currentState) ++ "\n--------------------------------------------"




    where
        initQubit :: [Complex Double]
        initQubit = [1 :+ 0, 0 :+ 0]

        replace :: Int -> c -> [c] -> [c]
        replace index' newelem = zipWith (\ index elem' -> (if index == index' then newelem else elem')) [0..]

        applyGate :: Matrix (Complex Double) -> [Complex Double] -> [Complex Double]
        applyGate gateMatrix qubitVector = toList $ multStd gateMatrix (colVector (V.fromList qubitVector))

        applySingleGate :: String -> Matrix (Complex Double) -> Int -> EvalM ()
        applySingleGate gateName gateMatrix qubitId = do
            env <- get
            if length (qubits env) <= qubitId || qubitId < 0 then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show qubitId
            else do
                let qubit = qubits env !! qubitId
                    transformedQubit = applyGate gateMatrix qubit
                put env {qubits = replace qubitId transformedQubit (qubits env)}
                liftIO $ putStrLn $ gateName ++ " gate used on " ++ show qubitId ++ "-th qubit"

        isBasisState :: [Complex Double] -> Bool
        isBasisState [a, b] = magnitude a == 1 && magnitude b == 0 || magnitude a == 0 && magnitude b == 1
        isBasisState _ = False

        -- isEntangled :: Int -> EvalM Bool
        -- isEntangled qubitId' = do
        --     env <- get
        --     let entagledList = entangledId env
        --     return $ length (entagledList !! qubitId') > 1

        tensorProduct :: [Complex Double] -> [Complex Double] -> [Complex Double]
        tensorProduct [] _ = []
        tensorProduct _ [] = []
        tensorProduct (x:xs) vector2 =
            let y = map (x *) vector2
                ys = tensorProduct xs vector2
            in y ++ ys

        matrixTensorProduct :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
        matrixTensorProduct matrix1 matrix2 = goMatrix matrix1 matrix2 []
         
        goMatrix :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
        goMatrix [] _ prod = prod
        goMatrix (x:xs) matrix2' prod = 
            let y = map (tensorProduct x) matrix2'
            in goMatrix xs matrix2' (prod ++ y) 

        scaleGateMatrix :: [[Complex Double]] -> Int -> [Int] -> [[Complex Double]]
        scaleGateMatrix _ _ [] = []
        scaleGateMatrix gateMatrix qId [x] = if qId == x
            then gateMatrix
            else identityMatrix
        scaleGateMatrix gateMatrix qId (x:xs) = if qId == x 
            then matrixTensorProduct gateMatrix (scaleGateMatrix gateMatrix qId xs)
            else matrixTensorProduct identityMatrix (scaleGateMatrix gateMatrix qId xs)



        updateEntanglementList :: [Int] -> [[Int]] -> [[Int]]
        updateEntanglementList newEntangledId entangledList = [ if i `elem` newEntangledId then newEntangledId else oldEntanglement
                    | (i, oldEntanglement) <- zip [0..] entangledList ]

        updateQubitList :: [Int] -> [Complex Double] -> [[Complex Double]] -> [[Complex Double]]
        updateQubitList newEntangledId newEntangledQubit qubitList = [ if i `elem` newEntangledId then newEntangledQubit else oldQubitList
                    | (i, oldQubitList) <- zip [0..] qubitList ]

        applyCnot :: [Complex Double] -> [Complex Double]
        applyCnot qubit = goCnot qubit [] []

        goCnot :: [Complex Double] -> [Complex Double] -> [Complex Double] -> [Complex Double]
        goCnot [] firstHalf secondHalf = firstHalf ++ secondHalf
        goCnot (x:xs) firstHalf secondHalf = if length (x:xs) > length firstHalf 
            then goCnot xs (firstHalf ++ [x]) secondHalf 
            else goCnot (tail xs) firstHalf (secondHalf ++ [head xs, x])


execProgram :: [Statement] -> EvalM ()
execProgram = mapM_ execStmt

runProgram :: [Statement] -> IO ((), Environment)
runProgram list = do
    putStrLn "Running the program...\n--------------------------------------------"
    runStateT (execProgram list) initialEnv
    where
        initialEnv = Env {vars = Map.empty ,qubits = [], entangledId = []}

example1 :: [Statement]
example1 = [InitQubit 2, Hadamard 0]