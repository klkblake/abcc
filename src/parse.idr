import program

{-
private
va : AType a
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
-}

-- Look at how effects uses type level sets for inspiration on dealing with constraints.
parseOp : Char -> Either String (ty : QType ** Op ty)
parseOp 'l' = Right (_ ** AssocL)
parseOp 'r' = Right (_ ** AssocR)
--parseOp 'w' = Right (va .* vb .* vc       ** (_ ** Swap))
--parseOp 'z' = Right (va .* vb .* vc .* vd ** (_ ** SwapD))
--parseOp 'v' = Right (va                   ** (_ ** Intro1))
--parseOp 'c' = Right (va .* Unit           ** (_ ** Elim1))
--parseOp '%' = Right (vx .* ve             ** (_ ** Drop))
--parseOp '^' = Right (vx .* ve             ** (_ ** Copy))
parseOp op = Left $ "Unrecognised operation: " ++ show op

cons : (ty ** Op ty) -> (ty' ** Program ty') -> Either String (ty'' ** Program ty'')
cons (Forall {n=n} vs (a ~~> b) ** op) (Forall {n=n'} vs' (b' ~~> c) ** prog) with (decEq n n')
  cons (Forall {n=n} vs (a ~~> b) ** op) (Forall {n=n} vs' (b' ~~> c) ** prog) | Yes refl with (decEq vs vs')
    cons (Forall vs (a ~~> b) ** op) (Forall vs  (b' ~~> c) ** prog) | Yes refl | Yes refl with (decEq b b')
      cons (Forall vs (a ~~> b) ** op) (Forall vs (b  ~~> c) ** prog) | Yes refl | Yes refl | Yes refl = Right (Forall vs (a ~~> c) ** op :: prog)
      cons (Forall vs (a ~~> b) ** op) (Forall vs (b' ~~> c) ** prog) | Yes refl | Yes refl | No  _    = Left $ "Cannot unify " ++ show b ++ " and " ++ show b'
    cons (Forall vs (a ~~> b) ** op) (Forall vs' (b' ~~> c) ** prog) | Yes refl | No _ = Left $ "Variables don't match\n\tLeft: " ++ show vs ++ "\n\tRight: " ++ show vs'
  cons (Forall {n=n} vs _ ** _) (Forall {n=n'} vs' _ ** _) | No _ = Left $ "Number of variables don't match.\n\tLeft: " ++ show vs ++ "\n\tRight: " ++ show vs'

parse : List Char -> Either String (ty ** Program ty)
parse (x :: xs) = do
  (ty ** op) <- parseOp x
  (ty' ** prog) <- parse xs
  cons (ty ** op) (ty' ** prog)
parse [] = Right (Forall ["a"] $ Var "a" ~~> Var "a" ** [])
