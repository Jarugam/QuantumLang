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

runEval :: Environment -> EvalM a -> IO(a, Environment)
runEval env eval = runStateT eval env

execStmt :: Statement -> EvalM ()
execStmt stmt = do
    case stmt of
        InitQubit n -> do
            when (n < 1) $ liftIO $ putStrLn "Invalid ammount of initialized qubits"
            env <- get
            let newQubits = replicate n initQubit
                updatedQubits = qubits env ++ newQubits
            put Env { qubits = updatedQubits }
            liftIO $ putStrLn $ "Initialized " ++ show n ++ " qubits"

        Hadamard qubitId -> do
            let hadamard = fromLists
                    [ [1/sqrt (2) :+ 0, 1/sqrt (2) :+ 0]
                    , [1/sqrt (2) :+ 0, (- (1 / sqrt (2))) :+ 0]
                    ]
            applySingleGate "Hadamard" hadamard qubitId

        PauliX qubitId -> do
            let pauliX = fromLists
                        [ [0 :+ 0, 1 :+ 0]
                        , [1 :+ 0, 0 :+ 0]
                        ]
            applySingleGate "Pauli X" pauliX qubitId

        PauliY qubitId -> do
            let pauliY = fromLists
                        [ [0 :+ 0, 0 :+ 1]
                        , [0 :+ (-1), 0 :+ 0]
                        ]
            applySingleGate "Pauli Y" pauliY qubitId

        PauliZ qubitId -> do
            let pauliZ = fromLists
                        [ [1 :+ 0, 0 :+ 0]
                        , [0 :+ 0, (-1) :+ 0]
                        ]
            applySingleGate "Pauli Z" pauliZ qubitId

        CNOT controlQubitId targetQubitId -> do
            env <- get
            if length (qubits env) <= controlQubitId || controlQubitId < 0 ||
                length (qubits env) <= targetQubitId || targetQubitId < 0
                then liftIO $ putStrLn $ "No valid qubit with ID = " ++ show controlQubitId ++ " , or ID = " ++ show targetQubitId
            else do
                let cnot = fromLists
                            [ [1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
                            , [0 :+ 0, 1 :+ 0, 0 :+ 0, 0 :+ 0]
                            , [0 :+ 0, 0 :+ 0, 0 :+ 0, 1 :+ 0]
                            , [0 :+ 0, 0 :+ 0, 1 :+ 0, 0 :+ 0]
                            ]
                    qubitList = qubits env
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
                        _ -> do
                            liftIO $ putStrLn "Qubits left unchanged (ket 0 as control case)"
                else do
                    liftIO $ putStrLn "Cnot gate for entangled qubits remains to be implemented"




    currentState <- get
    liftIO $ putStrLn $ "Current state vector: " ++ show (qubits currentState) ++ "\n--------------------------------------------"




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
        isBasisState [a, b] = (magnitude a == 1 && magnitude b == 0) || (magnitude a == 0 && magnitude b == 1)
        isBasisState _ = False

        isEntangled :: Int -> EvalM Bool
        isEntangled qubitId' = do
            env <- get
            let entagledList = entangledId env
            return $ any (qubitId' `elem`) entagledList


execProgram :: [Statement] -> EvalM ()
execProgram = mapM_ execStmt

runProgram :: [Statement] -> IO ((), Environment)
runProgram list = do
    putStrLn "Running the program...\n--------------------------------------------"
    runStateT (execProgram list) initialEnv
    where
        initialEnv = Env {vars = Map.empty ,qubits = [], entangledId = []}

example1 :: [Statement]
example1 = [InitQubit 2, PauliX 1, CNOT 0 1]
