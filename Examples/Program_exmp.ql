
INIT 0
INIT 1

HADAMARD 0
PHASE 0.5 0
PAULIX 1
CNOT 0 1


MEASURE 1 -> result

IF result {
  PRINT "Warunek spelniony"
  PAULIZ 0
}


REPEAT 2 {
  HADAMARD 0
  CNOT 0 1
}


PRINT "Koniec programu"
