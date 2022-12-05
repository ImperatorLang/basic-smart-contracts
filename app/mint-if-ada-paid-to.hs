{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import IfLovelacePaidMintingPolicy (policy, IfLovelaceContractParam(..))
import Utils         (writeMintingPolicy)
import qualified Ledger
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, address', amount', allowedTokenToMint'] <- getArgs
    let address   = address'
        amount    = amount'
        allowedTokenToMint = allowedTokenToMint' 
        contParam = IfLovelaceContractParam { addressToReceivePayment = Ledger.PaymentPubKeyHash $ fromString address
                                            , lovelacePerToken        = read amount :: Integer
                                            , acceptedTokenNameToMint = fromString allowedTokenToMint   
                                            }
        typeOfAddress = typeOf (addressToReceivePayment contParam)
        typeOfAmount  = typeOf (lovelacePerToken contParam)
        typeOfTokenToMint = typeOf (acceptedTokenNameToMint contParam) 
    e <- writeMintingPolicy file $ policy contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Policy saved to file          : " ++ file
                                   , " addressToReceivePayment       : " ++ address
                                   , " lovelace per token            : " ++ amount
                                   , " Parameter to contract         : " ++ show contParam
                                   , " address (obj type)            : " ++ show typeOfAddress
                                   , " lovelace per token (obj type) : " ++ show typeOfAmount
                                   , " token to mint (obj type)      : " ++ show typeOfTokenToMint
                                   , "_______________________________________________"]
