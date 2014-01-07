
import System.IO

import Language.Java.Parser

import qualified Data.ByteString.Lazy as LBS

import Entologic.Convert

main = do
    code <- hGetContents stdin
    let jcu = parser compilationUnit code
    case jcu of
      Left err -> putStrLn $ "Error: " ++ show err
      Right jcu' -> do
        let cu = convert jcu
        return ()
--    LBS.hPut $ encode cu 
    
