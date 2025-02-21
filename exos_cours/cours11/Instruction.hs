module Instruction where

data Instruction
    = Push Int
    | Pop
    | Display -- Display la dernière valeur de la pile (pop)
    | Store String -- Mémoire qui stocke la valeur des variables (map) prend un nom et pop la dernière valeur de la pile
    | Load String
    | Plus
    | Minus
    | JMPC Int -- Saut conditionnel avec la valeur du sommet de la pile
    | JMPI Int -- Saut inconditionnel
    deriving Show
