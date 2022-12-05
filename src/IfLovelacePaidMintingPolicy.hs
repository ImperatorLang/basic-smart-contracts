{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module IfLovelacePaidMintingPolicy where

import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import           Prelude                (Show)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Contract parameter object
data IfLovelaceContractParam = IfLovelaceContractParam
    { addressToReceivePayment :: PaymentPubKeyHash
    , lovelacePerToken :: Integer
    , acceptedTokenNameToMint :: TokenName
    } deriving Show

-- Tell compiler the IfLovelaceContractParam is liftable
PlutusTx.makeLift ''IfLovelaceContractParam

-- a pragma for the mkPolicy to make it possible to use it as inlinable parameter in the compile function
{-# INLINABLE mkPolicy #-}

--          Parameter                  Redeemer   Context         Result
mkPolicy :: IfLovelaceContractParam -> ()      -> ScriptContext-> Bool
mkPolicy contractParam () ctx = traceIfFalse "At least one token with the correct name must be minted" minOneCorrectTokenMinted &&
                                traceIfFalse "Payment too low to mint requested number of tokens" checkMintedTypeAndAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx 

        payment :: Value
        payment = valuePaidTo info (unPaymentPubKeyHash $ addressToReceivePayment contractParam)

        lovelacePaid :: Integer
        lovelacePaid = valueOf payment adaSymbol adaToken

        minOneCorrectTokenMinted :: Bool
        minOneCorrectTokenMinted = case flattenValue (txInfoMint info) of
          [(_, tn', amt)]     -> tn' == (acceptedTokenNameToMint contractParam) && amt >= 1
          _                   -> False

        checkMintedTypeAndAmount :: Bool
        checkMintedTypeAndAmount = case flattenValue (txInfoMint info) of
          [(_, _, amt)] -> lovelacePaid >= amt * (lovelacePerToken contractParam)
          _             -> False


-- compile policy into Plutus script
policy :: IfLovelaceContractParam -> Scripts.MintingPolicy
policy contractParam = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractParam

-- get currency symbol for policy
curSymbol :: IfLovelaceContractParam -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
