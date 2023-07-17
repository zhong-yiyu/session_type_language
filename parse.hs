import Text.Parsec
import Text.Parsec.ByteString
-- typeCheck:: Term -> Type -> TypeEnv

data Expr =
    ExprLet Expr Expr
    | ExprVar String
    -- | ExprApp Expr Expr
    | ExprInt Int
    -- | ExprBool Bool
    -- | ExprIf Expr Expr Expr
    | ExprPlus Expr Expr
    deriving (Show)

data Type =
    TypeInt
    -- | TypeBool
    -- | TypeFun Type Type
    | TypeVar String
    -- | TypeScheme [String] Type
    deriving (Show)


-- parseExpr is an function that takes a string and parses it into an Expr
-- Inside the parseExpr function, we use the parse function from Text.Parsec
-- the "let" keyword will be parsed into the ExprLet constructor
-- the numbers will be parsed into the ExprInt constructor
-- the "in" keyword will be parsed into the ExprIn constructor
-- the "=" keyword is used to bind a variable to an expression, it will be parsed into the ExprVar constructor
-- the "+" keyword will be parsed into the ExprPlus constructor
-- all none keyword strings will be parsed into the ExprVar constructor
-- The function's return result should be printable, so we derive the Show typeclass