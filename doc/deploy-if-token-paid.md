# If Token Paid Minting Policy
The `IfTokenPaidMintingPolicy` only accepts minting of tokens for transactions that pay an amount of specific tokens to a defined wallet.

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
This works in the way that the `mint-if-paid-to` executable compiles your minting policy using three parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-nok-paid-to-0-1.plutus` |
| 2 | wallet pub key hash | `7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2` | 
| 3 | amount of desired tokens required to mint | `10` |
| 4 | policy id of required token | `c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f` |
| 5 | token name of required token | `4e4f4b` |
```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-token-paid-to -- plutus-scripts/mint-if-nok-paid-to-0-1.plutus 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2 10 c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f 4e4f4b
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-nok-paid-to-0-1.plutus
 addressToPay                  : 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2
 Minimum token amount required : 10
 Accepted token policy         : c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f
 Accepted token name           : 4e4f4b
 Parameter to contract         : ContractParam {addressToPay = 7b45192a44984917462c498270f8ba855f9689a50d923d7fa00aeef2, acceptedTokenPolicy = c5825916b76ea72083f51b28faa07931d93677e6be92fab76d97269f, acceptedTokenName = "4e4f4b", minTokenAmount = 10}
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

## Generating minting policy script address
The final step is to generate the minting policy script address. This address is used when you interact with it

```
~/ : cardano-cli address build --payment-script-file smart-contracts/mint-if-nok-paid-to-0-1.plutus $MAGIC --out-file smart-contracts/mint-if-nok-paid-to-0-1.addr
~/ : cat smart-contracts/mint-if-nok-paid-to-0-1.addr 
addr_test1wqthpaynm5qjkr74nfyn6sauae5meuu486n4uttk73w3stg0j5a2f 
```
Once again, the address `addr_test1wqthpaynm5qjkr74nfyn6sauae5meuu486n4uttk73w3stg0j5a2f` is only an example of how your address should look.
You are now ready for interacting with your minting policy
