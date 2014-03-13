import program

private
va : AType Nat
va = Var 0

private
vb : AType Nat
vb = Var 1

private
vc : AType Nat
vc = Var 2

private
vd : AType Nat
vd = Var 3

private
ve : AType Nat
ve = Var 4

private
vx : AType Nat
vx = Var 5

private
vy : AType Nat
vy = Var 6

private
vz : AType Nat
vz = Var 7

private
vx' : AType Nat
vx' = Var 8

-- Look at how effects uses type level sets for inspiration on dealing with constraints.
parseOp : Char -> Either String (a ** (b ** Op {v=Nat} a b))
parseOp 'l' = Right (va .* vb .* vc       ** (_ ** AssocL))
parseOp 'r' = Right ((va .* vb) .* vc     ** (_ ** AssocR))
parseOp 'w' = Right (va .* vb .* vc       ** (_ ** Swap))
parseOp 'z' = Right (va .* vb .* vc .* vd ** (_ ** SwapD))
parseOp 'v' = Right (va                   ** (_ ** Intro1))
parseOp 'c' = Right (va .* Unit           ** (_ ** Elim1))
--parseOp '%' = Right (vx .* ve             ** (_ ** Drop))
--parseOp '^' = Right (vx .* ve             ** (_ ** Copy))

--parseOp '$' = Right ((vx -> vx') .* vx .* ve ** (_ ** Apply))
parseOp op = Left $ "Unrecognised operation: " ++ show op

--parse : List Char -> Either String (a ** (b ** Program a b))
--parse (x :: xs) =
--  let (q ** (w ** op)) = parseOp x
--      (s ** (r ** prog)) = parse xs
--  in Right (s ** (r ** prog))
--parse [] = []
--  in case decEq w e of
--          Yes refl => Right ((q, r) ** op :: prog)
--          No  _    => Left $ "Could not match " ++ show w ++ " and " ++ show e
