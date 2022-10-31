{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import IfLovelacePaidMintingPolicy (policy, ContractParam(..))
import Utils         (writeMintingPolicy)
import qualified Ledger
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, address', amount'] <- getArgs
    let address   = address'
        amount    = amount' 
        contParam = ContractParam { addressToPay = Ledger.PaymentPubKeyHash $ fromString address
                                  , minLovelaceAmount = read amount :: Integer  
                                  }
        typeOfAddress = typeOf (addressToPay contParam)
        typeOfAmount  = typeOf (minLovelaceAmount contParam) 
    e <- writeMintingPolicy file $ policy contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Policy saved to file          : " ++ file
                                   , " addressToPay                  : " ++ address
                                   , " Minimum lovelace required     : " ++ amount
                                   , " Parameter to contract         : " ++ show contParam
                                   , " addressToPay    (obj type)    : " ++ show typeOfAddress
                                   , " minLovelaceAmount (obj type)  : " ++ show typeOfAmount
                                   , "_______________________________________________"]
