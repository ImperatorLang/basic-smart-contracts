# If Token Paid Minting Policy
The `IfTokenPaidMintingPolicy` only accepts minting of tokens for transactions that pay an amount of specific tokens to a defined wallet.
Use case for this minting policy, is to require a payment in stable coins like **Djed** or **USDA** to mint the requested amount of tokens

Warning: A future version will also check that the amount is enough according to the number of tokens minted. Today the amount is a simple “minimum fee” to allow minting. There is no limitation on the number of tokens minted, so users can request as many as they want.
A near future update to this contract will enforce the minumum amount as the fee for a specific amount of tokens minted. 

Examples: 
- 1 Djed = 1 minted token, 10 Djed = 10 minted tokens
- 10 USDA = 2 minted tokens, 20 USDA = 4 minted tokens

## Prerequisites
- Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md)
- Local copy of this repository, built according to [building instructions](building-the-basic-smart-contracts-repo.md)
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)

## Brief
The following instructions are for deploying the built IfTokenPaidMintingPolicy to Cardano blockchain.
Instructions are the same for all of the Cardano Blockchains, but you need to change the
"Network Magic" parameter according to your environment
Use one of the following Network Magics according to which network you want to work
| Network | Magic | Command |
| --- | --- | --- |
| Preview | `--testnet-magic 2` | `export MAGIC="--testnet-magic 2"` |
| Pre-Production | `--testnet-magic 1` | `export MAGIC="--testnet-magic 1"` |
| Mainnet | `--mainnet` | `export MAGIC="--mainnet"` |

The Cardano CLI commands are compatible with version 1.35.3
```
~  : cardano-cli --version
cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
git rev 950c4e222086fed5ca53564e642434ce9307b0b9
```

## Configure minting policy
Usage of the minting policy will require a minimum amount of tokens to be paid to a specific wallet. We will now configure the minting policy so it knows which wallet to check and the minimum amount or the token required for minting. 

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file mp-pay-to-wallet.vkey --signing-key-file mp-pay-to-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file mp-pay-to-wallet.vkey $MAGIC --out-file mp-pay-to-wallet.addr
```

### Fetch Payment pub key hash for address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file mp-pay-to-wallet.vkey --out-file mp-pay-to-wallet.pkh
```

The contents of your mp-pay-to-wallet.pkh should now be a 56 byte hex, similar to but not identical to `7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2`. 
All wallets have their own unique key hash

## Serialize minting policy script
Time has come to build your unique minting policy. This is accomplished with the following command.
This works in the way that the `mint-if-token-paid-to` executable compiles your minting policy using three parameters (following --)

NOTE: It is important that you verify the policy id (currency symbol) of the token you require as payment. The policy id `c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f` **IS NOT** the real policy id of Djed.
To get hold of your preferred tokens policy id, please refer to the website of the token creators.
| Token | Stable coin provider | Web site | Status |
| --- | --- | --- | --- |
| iUSD | Indigo protocol | https://app.indigoprotocol.io/ | Mainnet |
| USDA | Emurgo | https://www.anzens.com/ | Announced |
| Djed | IOG and COTI | https://djed.xyz/ | Testnet, Mainnet January 2023 |


When you have determined the policy id to use, you can use this as a parameter when building your contract as follows
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-nok-paid-to-0-1.plutus` |
| 2 | wallet pub key hash | `7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2` | 
| 3 | amount of desired tokens required to mint | `10` |
| 4 | policy id of required token | `c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f` |
| 5 | token name of required token | `Djed` |
```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-token-paid-to -- plutus-scripts/mint-if-djed-paid-to-address-1-0.plutus 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2 10 c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f Djed
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-djed-paid-to-address-1-0.plutus
 addressToPay                  : 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2
 Minimum token amount required : 10
 Accepted token policy         : c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f
 Accepted token name           : Djed
 Parameter to contract         : ContractParam {addressToPay = 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2, acceptedTokenPolicy = c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f, acceptedTokenName = "Djed", minTokenAmount = 10}
 addressToPay    (obj type)    : PaymentPubKeyHash
 minTokenAmount (obj type)     : Integer
 tokenPolicy  (obj type)       : CurrencySymbol
 tokenName  (obj type)         : TokenName
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
