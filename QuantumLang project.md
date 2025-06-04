# Quantum Computing Language
 
Taken (MJ,JP,AD)

## Project Overview
This project implements a simplified domain-specific language for expressing quantum computing algorithms. The language provides abstractions for common quantum operations such as qubit initialization, gate applications, measurements, and classical control flow based on measurement results.

## Key Goals
1. **Parser Implementation**: Convert quantum program text into a structured AST
2. **Quantum Simulator**: Execute quantum operations using linear algebra
3. **Test Suite**: Ensure correctness through unit and property-based testing

## Simplified Syntax Definition

```haskell
-- The Program is a sequence of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Statements represent quantum operations
data Statement
  = InitQubit Int                     -- Initialize n qubits
  | Hadamard Int                      -- Apply Hadamard gate to qubit n
  | PauliX Int                        -- Apply NOT (X) gate to qubit n 
  | PauliY Int                        -- Apply Y gate to qubit n
  | PauliZ Int                        -- Apply Z gate to qubit n
  | CNOT Int Int                      -- Controlled-NOT with control and target qubits
  | Phase Double Int                  -- Phase rotation by angle
  | Measure Int String                -- Measure qubit n, store in variable
  | If String [Statement]             -- Conditional execution based on measurement
  | Repeat Int [Statement]            -- Repeat statements n times
  | Print String                      -- Output a variable or message
  deriving (Show, Eq)
```

## Example Program: Quantum Bell State

```
// Bell State Creation and Measurement
INIT 2              // Initialize 2 qubits
HADAMARD 0          // Put qubit 0 in superposition
CNOT 0 1            // Entangle qubits 0 and 1

// Measure both qubits
MEASURE 0 -> m0
MEASURE 1 -> m1

// Print results
PRINT m0
PRINT m1

// Results should be perfectly correlated:
// Either both 0 or both 1
```

The Bell State example demonstrates key quantum phenomena:
1. Creation of quantum superposition with Hadamard gate
2. Entanglement of two qubits using CNOT
3. Measurement resulting in correlated outcomes
4. The non-classical behavior where measuring one qubit instantly affects the other

## Implementation Components

### 1. Parser
- Process quantum program text into an AST
- Handle quantum operations and gates
- Support variable declarations for measurement results
- Generate meaningful error messages for syntax issues
- Support comments and basic error recovery

### 2. Quantum Simulator
- Implement state vector representation for quantum systems
- Provide matrix implementations of standard quantum gates
- Calculate tensor products for multi-qubit operations
- Handle probabilistic measurement according to quantum mechanics
- Track classical variables for measurement results
- Support conditional operations based on measurement outcomes
- Provide visualization of quantum states (optional)

### 3. Test Suite
- **Unit Tests**:
  - Parser correctness for various inputs
  - Individual gate operations
  - Measurement operations
  
- **Property-Based Testing**:
  - Generate random valid quantum programs
  - Ensure simulator correctly implements quantum mechanical principles
