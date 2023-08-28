infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand (Val x) = Val x
expand ((e1 :+: e2) :*: e) = let exp = expand e1 :*: expand e :+: expand e2 :*: expand e in if expand exp == exp then exp else expand exp
expand (e :*: (e1 :+: e2)) = let exp = expand e :*: expand e1 :+: expand e :*: expand e2 in if expand exp == exp then exp else expand exp
expand (e1 :+: e2) = let exp = expand e1 :+: expand e2 in if expand exp == exp then exp else expand exp
expand (e1 :*: e2) = let exp = expand e1 :*: expand e2 in if expand exp == exp then exp else expand exp
expand e = e