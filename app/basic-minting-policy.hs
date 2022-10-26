{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)

import Data.String          (IsString (..))
import System.Environment   (getArgs)
import BasicMintingPolicy (policy)
import Utils         (writeMintingPolicy)
import qualified Ledger
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, pubkeyhash'] <- getArgs
    let pubkeyhash    = pubkeyhash'
        policyOwner = Ledger.PaymentPubKeyHash $ fromString pubkeyhash
        typeOfPolicyOwner = typeOf policyOwner
    e <- writeMintingPolicy file $ policy policyOwner
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Policy saved to file       : " ++ file
                                   , " PubKeyHash of owner wallet : " ++ pubkeyhash
                                   , " policyOwner    (obj type)  : " ++ show typeOfPolicyOwner
                                   , "_______________________________________________"]
