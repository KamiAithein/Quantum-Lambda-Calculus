module Common where

type QVarType = String

data QConst = QCNew
            | QCMeas
            | QCHadamard
            deriving (Show)

qConstMap :: [(String, QConst)]
qConstMap = [ ("new", QCNew)
            , ("meas", QCMeas)
            , ("H", QCHadamard) ]

data QValue = QVVar     QVarType
            | QVConst   QConst
            | QVStar
            | QVLam     QVarType    QTerm
            | QVPair    QValue      QValue
            deriving (Show)

data QTerm = QTeValue    QValue
           | QTePair     QTerm       QTerm
           | QTeApp      QTerm       QTerm
           | QTeLet      QVarType    QVarType    QTerm   QTerm
           deriving (Show)

data QType = None -- remove, replace w alpha
           | QTyBang    QType
           | QTyFunc    QType   QType
           | QTyAlways 
           | QTyTens    QType   QType
           deriving (Show)