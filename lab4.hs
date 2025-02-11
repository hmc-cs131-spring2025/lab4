data BoolExpr
  = T
  | F
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Not BoolExpr
  | Implies BoolExpr BoolExpr
  deriving (Show)

-- expr0 represents (T & F)
expr0 :: BoolExpr
expr0 = And T F

-- expr1 represents (T & F) | ~F
expr1 :: BoolExpr
expr1 = Or (And T F) (Not F)

-- TODO: expr2 should represent ~ (T & F) | (T -> F)
expr2 :: BoolExpr
expr2 = undefined

-- TODO: expr3 should be your own expression
-- that uses each operator (&, |, ~, ->) at least once
expr3 :: BoolExpr
expr3 = undefined

-- evaluate a BoolExpr to result in a Haskell Bool
-- TODO: implement eval for Or, Not, and Implies
eval :: BoolExpr -> Bool
eval T = True
eval F = False
eval (And a b) = eval a && eval b
eval (Or a b) = undefined
eval (Not a) = undefined
eval (Implies a b) = undefined

-- TODO: make sure these evaluate to the correct value
val0 = eval expr0

val1 = eval expr1

val2 = eval expr2

val3 = eval expr3

-- pretty print a bool expression
-- TODO: implement pretty printing for Or, Not, Implies
prettyPrint :: BoolExpr -> String
prettyPrint T = "T"
prettyPrint F = "F"
prettyPrint (And a b) = "(" ++ prettyPrint a ++ " & " ++ prettyPrint b ++ ")"
prettyPrint (Or a b) = undefined
prettyPrint (Not a) = undefined
prettyPrint (Implies a b) = undefined

-- make sure these convert to the correct strings
str0 = prettyPrint expr0

str1 = prettyPrint expr1

str2 = prettyPrint expr2

str3 = prettyPrint expr3
