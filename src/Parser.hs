module Parser where

import Common

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char


qlcTerm :: GenParser Char st QTerm
qlcTerm = do
    { char '<'
    ; qlcTPair
    }
    <|> do
    { char '('
    ; qlcTApp
    }
    <|> do
    { string "let"
    ; qlcTLet
    }
    <|> do 
    { val <- qlcVal
    ; return $ QTeValue val
    }

qlcTPair :: GenParser Char st QTerm
qlcTPair = do
    { spaces
    ; left <- qlcTerm
    ; spaces
    ; char ','
    ; spaces
    ; right <- qlcTerm
    ; spaces
    ; char '>'
    ; return $ QTePair left right
    }

qlcTApp :: GenParser Char st QTerm
qlcTApp = do
    { spaces
    ; t1 <- qlcTerm
    ; spaces
    ; t2 <- qlcTerm
    ; spaces
    ; char ')'
    ; return $ QTeApp t1 t2
    }

qlcTLet :: GenParser Char st QTerm
qlcTLet = do
    { spaces
    ; char '<'
    ; v1 <- many letter
    ; spaces
    ; char ','
    ; spaces
    ; v2 <- many letter
    ; spaces
    ; char '>'
    ; spaces
    ; char '='
    ; spaces
    ; assignee <- qlcTerm
    ; spaces
    ; string "in"
    ; spaces
    ; body <- qlcTerm
    ; return $ QTeLet v1 v2 assignee body
    }

qlcVal :: GenParser Char st QValue
qlcVal = do
    { string "new"
    ; return $ QVConst QCNew
    } <|> do
    { string "meas"
    ; return $ QVConst QCNew
    } <|> do
    { char '*'
    ; return QVStar
    }
    <|> do
    { char '\\'
    ; spaces
    ; var <- many letter
    ; char '.'
    ; spaces
    ; body <- qlcTerm
    ; return $ QVLam var body
    }
    <|> do
    { char '<'
    ; spaces
    ; val1 <- qlcVal
    ; spaces
    ; char ','
    ; spaces
    ; val2 <- qlcVal
    ; char '>'
    ; return $ QVPair val1 val2
    }
    <|> do
    { var <- many letter
    ; return $ QVVar var
    }

{-
lcP :: GenParser Char st LCExp
lcP = do
    { lam <- lcLam
    ; char ')'
    ; return lam 
    }
    <|>
      do
    { pBody <- lcSS
    ; char ')'
    ; return pBody
    }
-}