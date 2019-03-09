{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module UntypedLambda.Pretty (
  ppexpr
) where

import UntypedLambda.Syntax

import Prelude hiding ((<>))
import Text.PrettyPrint

class Pretty p where
  ppr :: Bool -> p -> Doc

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ = text

instance Pretty Expr where
  ppr _ (Var x) = text x
  ppr _ (Lit (LInt a)) = text (show a)
  ppr _ (Lit (LBool a)) = text (show a)
  ppr p e@(App f x) = parensIf p (ppr p f <+> sep (map (ppr True) xs))
    where (f, xs) = viewApp e
  ppr p e@(Lam _ _) = parensIf p $ char '\\' <> hsep vars <+> text "." <+> body
    where
      vars = map (ppr False) (viewVars e)
      body = ppr True $ viewBody e

viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ b) = viewBody b
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
  where
      go (App e1 e2) xs = go e1 (e2 : xs)
      go e xs = (e, xs)
viewApp _ = error "not application"

ppexpr :: Expr -> String
ppexpr = render . ppr False
