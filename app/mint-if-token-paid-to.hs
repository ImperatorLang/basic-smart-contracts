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
    [file, address', amount', policyid', tokenname', allowedTokenToMint'] <- getArgs
    let address            = address'
        amount             = amount'
        policyid           = policyid'
        tokenname          = tokenname'
        allowedTokenToMint = allowedTokenToMint'
        contParam = ContractParam { addressToReceivePayment              = Ledger.PaymentPubKeyHash $ fromString address
                                  , acceptedTokenPolicyAsPayment         = fromString policyid
                                  , acceptedTokenNameAsPayment           = fromString tokenname
                                  , numTokensAsPaymentForEachMintedToken = read amount :: Integer
                                  , acceptedTokenNameToMint              = fromString allowedTokenToMint  
                                  }
        typeOfAddress     = typeOf (addressToReceivePayment contParam)
        typeOfAmount      = typeOf (numTokensAsPaymentForEachMintedToken contParam) 
        typeOfPolicy      = typeOf (acceptedTokenPolicyAsPayment contParam)
        typeOfTokenName   = typeOf (acceptedTokenNameAsPayment contParam) 
        typeOfTokenToMint = typeOf (acceptedTokenNameToMint contParam)
    e <- writeMintingPolicy file $ policy contParam
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> mapM_ putStrLn [ "_______________________________________________"
                                   , " Policy saved to file          : " ++ file
                                   , " addressToReceivePayment       : " ++ address
                                   , " Minimum token amount required : " ++ amount
                                   , " Accepted token policy paid    : " ++ policyid
                                   , " Accepted token name paid      : " ++ tokenname
                                   , " Accepted token name to mint   : " ++ allowedTokenToMint 
                                   , " Parameter to contract         : " ++ show contParam
                                   , " addressToPay    (obj type)    : " ++ show typeOfAddress
                                   , " minTokenAmount (obj type)     : " ++ show typeOfAmount
                                   , " tokenPolicy paid (obj type)   : " ++ show typeOfPolicy
                                   , " token paid (obj type)         : " ++ show typeOfTokenName
                                   , " token to mint (obj type)      : " ++ show typeOfTokenToMint
                                   , "_______________________________________________"]
