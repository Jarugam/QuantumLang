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
                    , qubits :: [[Complex Double]] }

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


execProgram :: [Statement] -> EvalM ()
execProgram = mapM_ execStmt

runProgram :: [Statement] -> IO ((), Environment)
runProgram list = do
    putStrLn "Running the program...\n--------------------------------------------"
    runStateT (execProgram list) initialEnv
    where
        initialEnv = Env {vars = Map.empty ,qubits = []}

example1 :: [Statement]
example1 = [InitQubit 2, Hadamard 1, PauliY 0]
