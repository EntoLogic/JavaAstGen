
import System.IO

import Language.Java.Parser

import qualified Data.ByteString.Lazy as LBS

import Entologic.Convert
import Entologic.Ast
import Entologic.Ast.Json

import Data.Aeson

main = do
    code <- hGetContents stdin
    let jcu = parser compilationUnit code
    case jcu of
      Left err -> putStrLn $ "Error: " ++ show err
      Right jcu' -> do
--        putStrLn $ "Before conversion: " ++ show jcu'
        let cu = convert jcu' :: Program
        LBS.hPut stdout $ encode (UAst AstMeta cu)
    
