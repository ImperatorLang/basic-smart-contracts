# If Lovelace Paid Minting Policy
The `IfLovelacePaidMintingPolicy` checks if a certain amount of lovelace is paid to a particular wallet before allowing to mint any tokens. 
The contract also checks that the amount is enough according to the number of tokens minted. The provided fee is the cost for each token to mint, so if the transaction mints 3 tokens, 
the cost is also 3 times the fee.

## Prerequisites
- Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md)
- Local copy of this repository, built according to [building instructions](building-the-basic-smart-contracts-repo.md)
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)

## Brief
The following instructions are for deploying the built IfLovelacePaidMintingPolicy to Cardano blockchain.
Instructions are the same for all of the Cardano Blockchains, but you need to change the
"Network Magic" parameter according to your environment.
Use one of the following Network Magics according to which network you want to work
| Network | Magic | Command |
| --- | --- | --- |
| Preview | `--testnet-magic 2` | `export MAGIC="--testnet-magic 2"` |
| Pre-Production | `--testnet-magic 1` | `export MAGIC="--testnet-magic 1"` |
| Mainnet | `--mainnet` | `export MAGIC="--mainnet"` |

The Cardano CLI commands were run using version 1.35.3
```
~  : cardano-cli --version
cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
git rev 950c4e222086fed5ca53564e642434ce9307b0b9
```

## Configure minting policy
Usage of the minting policy will require a fee to be paid to a specific wallet for each of the tokens minted. We will now configure the minting policy so it knows which wallet to check and the amount required for minting. 

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file mp-pay-to-wallet.vkey --signing-key-file mp-pay-to-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file mp-pay-to-wallet.vkey $MAGIC --out-file mp-pay-to-wallet.addr
```

### Fetch Payment pub key hash for address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file mp-pay-to-wallet.vkey --out-file mp-pay-to-wallet.pkh
```

The contents of your mp-pay-to-wallet.pkh should now be a 56 byte hex, similar to but not identical to `df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c`. 
All wallets have their own unique public key hash

## Serialize minting policy script
Time has come to build your unique minting policy. This is accomplished with the following command.
This works in the way that the `mint-if-ada-paid-to` executable compiles your minting policy using four parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-paid-to-0-2.plutus` |
| 2 | wallet pub key hash | `df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c` | 
| 3 | amount of lovelaces per token minted | `2000000` |
| 4 | token name allowed to mint | `Membership` |
```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-ada-paid-to -- plutus-scripts/mint-if-ada-paid-to-address-0-3.plutus 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2 2000000 Membership
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-ada-paid-to-address-0-3.plutus
 addressToReceivePayment       : 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2
 lovelace per token            : 2000000
 Parameter to contract         : IfLovelaceContractParam {addressToReceivePayment = 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2, lovelacePerToken = 2000000, acceptedTokenNameToMint = "Membership"}
 address (obj type)            : PaymentPubKeyHash
 lovelace per token (obj type) : Integer
 token to mint (obj type)      : TokenName
_______________________________________________

[nix-shell:~/basic-smart-contracts]$ 
```
The contents of your minting policy plutus script file should now look similar to
```
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "590b89590b860100<shortened for readability>bd0048848cc00400c0088005"
}
```

## Determining minting policy id
To be able to mint the tokens, you will need to calculate the policy id of the finished minting policy. This is done with the following command. This id will be used when interacting with the contract later.
```
~/ : cardano-cli transaction policyid --script-file smart-contracts/mint-if-ada-paid-to-address-0-3.plutus
92b11c9351cfe054284b3757921f157745b82b602dc6805434b31d30
~/ : 
```
