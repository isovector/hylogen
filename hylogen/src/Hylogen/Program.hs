{- |
Internal shader program representation.
-}

module Hylogen.Program where


import Data.Reify
import Data.Monoid
import System.IO.Unsafe

import Hylogen.Expr
import Hylogen.Types.Vec (Vec4)

newtype Id = Id Int
instance Show Id where
  show (Id h) = "_" <> show h

-- | Statement internal representation
--
-- We tag a Statement with a Unique ID and its corresponding untyped expression
data Statement = NewAssign (Unique, ExprMonoF Unique)
               -- MutAssign (Unique, ExprMonoF Unique)

getExpr :: Statement -> ExprMonoF Unique
getExpr (NewAssign (_, expr)) = expr



instance Show Statement where
  show (NewAssign (i, expr@(TreeF (_, ty, _, _) _)))
    = mconcat [ show ty, " ", show . Id $ i, " = ", show . (Id<$>) $  expr, ";"]

-- | GLSL Function internal representation
--
-- A Function is composed of Statements.
newtype Function = Function [Statement]
instance Show Function where
  show (Function xs) = unlines [ "vec3 hash( vec3 p ){"
                               , "    return fract(sin(vec3( dot(p,vec3(1.0,57.0,113.0)),"
                               , "                           dot(p,vec3(57.0,113.0,1.0)),"
                               , "                           dot(p,vec3(113.0,1.0,57.0))))*43758.5453);"
                               , "                   }"

                               ,  "vec3 voronoi( const in vec3 x ) {"
                              , "    vec3 p = floor( x );"
                              , "    vec3 f = fract( x );"
                              , ""
                              , "    float id = 0.0;"
                              , "    vec2 res = vec2( 100.0 );"
                              , "    for( int k=-1; k<=1; k++ ) {"
                              , "        for( int j=-1; j<=1; j++ ) {"
                              , "            for( int i=-1; i<=1; i++ ) {"
                              , "                vec3 b = vec3( float(i), float(j), float(k) );"
                              , "                vec3 r = vec3( b ) - f + hash( p + b );"
                              , "                float d = dot( r, r );"
                              , ""
                              , "                if( d < res.x ) {"
                              , "                    id = dot( p+b, vec3(1.0,57.0,113.0 ) );"
                              , "                    res = vec2( d, res.x );"
                              , "                }"
                              , "                else if( d < res.y ) {"
                              , "                    res.y = d;"
                              , "                }"
                              , "            }"
                              , "        }"
                              , "    }"
                              , ""
                              , "    return vec3( sqrt( res ), abs(id) );"
                              , "}"
                              , ""
                              , "void main() {"
                              , assignments
                              , "    gl_FragColor = _1;"
                              , "}"
                              , ""
                              ]
    where
      assignments = mconcat $  (<> "\n") . ("    "<>) . show <$> reverse xs


-- | Returns a program given an expression in closed untyped form
monoToProgram :: ExprMono -> Function
monoToProgram v = unsafePerformIO $ do
  Graph nodes _ <- reifyGraph v
  return . Function $ NewAssign <$> nodes

-- | A GLSL program. Currently synonym for Function.
type Program = Function

-- | Helper function from a Vec4 to A GLSL Program, with sharing.
toProgram :: Vec4 -> Program
toProgram = monoToProgram . toMono
