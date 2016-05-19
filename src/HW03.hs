module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend getState varToSet val varToGet
  | varToGet == varToSet = val
  | otherwise            = getState varToGet

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var var)         = state var
evalE state (Val val)         = val
evalE state (Op e1 Plus e2)   = evalE state e1 + evalE state e2
evalE state (Op e1 Minus e2)  = evalE state e1 - evalE state e2
evalE state (Op e1 Times e2)  = evalE state e1 * evalE state e2
evalE state (Op e1 Divide e2) = evalE state e1 `div` evalE state e2
evalE state (Op e1 Gt e2)     = if evalE state e1 > evalE state e2 then 1 else 0
evalE state (Op e1 Ge e2)     = if evalE state e1 >= evalE state e2 then 1 else 0
evalE state (Op e1 Lt e2)     = if evalE state e1 < evalE state e2 then 1 else 0
evalE state (Op e1 Le e2)     = if evalE state e1 <= evalE state e2 then 1 else 0
evalE state (Op e1 Eql e2)    = if evalE state e1 == evalE state e2 then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var expr)           = DAssign var expr
desugar (Incr var)                  = DAssign var (Op (Var var) Plus (Val 1))
desugar (If cond s1 s2)             = DIf cond (desugar s1) (desugar s2)
desugar (While cond s)              = DWhile cond (desugar s)
desugar (For init cond update body) = DSequence (desugar init)
                                                (DWhile cond (DSequence (desugar body)
                                                                        (desugar update)))
desugar (Sequence s1 s2)            = DSequence (desugar s1) (desugar s2)
desugar Skip                        = DSkip


-- Exercise 4 -----------------------------------------

isTrue :: State -> Expression -> Bool
isTrue state cond = evalE state cond > 0

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var expr) = extend state var (evalE state expr)
evalSimple state (DIf cond s1 s2)   = evalSimple state (if isTrue state cond then s1 else s2)
evalSimple state (DWhile cond s)    = if isTrue state cond
                                      then evalSimple state (DSequence s (DWhile cond s))
                                      else state
evalSimple state (DSequence s1 s2)  = evalSimple (evalSimple state s1) s2
evalSimple state DSkip              = state

run :: State -> Statement -> State
run state s = evalSimple state (desugar s)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
