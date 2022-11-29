{-# LANGUAGE OverloadedStrings          #-}

module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import IfTokenPaidMintingPolicy (policy, ContractParam(..))
import Utils         (writeMintingPolicy)
import qualified Ledger
-- utility library to be able to get object types
import Data.Typeable

main :: IO ()
main = do
    [file, address', amount', policyid', tokenname'] <- getArgs
    let address   = address'
        amount    = amount'
        policyid  = policyid'
        tokenname = tokenname'
        contParam = ContractParam { addressToPay        = Ledger.PaymentPubKeyHash $ fromString address
                                  , acceptedTokenPolicy = fromString policyid
                                  , acceptedTokenName   = fromString tokenname
                                  , minTokenAmount      = read amount :: Integer  
                                  }
        typeOfAddress   = typeOf (addressToPay contParam)
        typeOfAmount    = typeOf (minTokenAmount contParam) 
        typeOfPolicy    = typeOf (acceptedTokenPolicy contParam)
        typeOfTokenName = typeOf (acceptedTokenName contParam) 
    e <- writeMintingPolicy file $ policy contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Policy saved to file          : " ++ file
                                   , " addressToPay                  : " ++ address
                                   , " Minimum token amount required : " ++ amount
                                   , " Accepted token policy         : " ++ policyid
                                   , " Accepted token name           : " ++ tokenname 
                                   , " Parameter to contract         : " ++ show contParam
                                   , " addressToPay    (obj type)    : " ++ show typeOfAddress
                                   , " minTokenAmount (obj type)     : " ++ show typeOfAmount
                                   , " tokenPolicy  (obj type)       : " ++ show typeOfPolicy
                                   , " tokenName  (obj type)         : " ++ show typeOfTokenName
                                   , "_______________________________________________"]
