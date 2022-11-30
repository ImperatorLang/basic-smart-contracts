# If Lovelace Paid Minting Policy
The `IfLovelacePaidMintingPolicy` only accepts minting of tokens for transactions that pay an amount of lovelaces to a defined wallet.

Warning: A future version will also check that the amount is enough according to the number of tokens minted. Today the amount is a simple “minimum fee” to allow minting. There is no limitation on the number of tokens minted, so users can request as many as they want.
A near future update to this contract will enforce the minumum amount as the fee for a specific amount of tokens minted. 

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
Usage of the minting policy will require a minimum amount / fee to be paid to a specific wallet. We will now configure the minting policy so it knows which wallet to check and the minimum amount required for minting. 

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
This works in the way that the `mint-if-ada-paid-to` executable compiles your minting policy using three parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-paid-to-0-2.plutus` |
| 2 | wallet pub key hash | `df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c` | 
| 3 | required amount of lovelaces required to mint | `2000000` |

```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-ada-paid-to -- plutus-scripts/mint-if-paid-to-0-2.plutus df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c 2000000
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-paid-to-0-2.plutus
 addressToPay                  : df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c
 Minimum lovelace required     : 2000000
 Parameter to contract         : ContractParam {addressToPay = df0bf673765ccce01f7cb46da22c39be0bc51433abf8e142da21cb8c, minLovelaceAmount = 2000000}
 addressToPay    (obj type)    : PaymentPubKeyHash
 minLovelaceAmount (obj type)  : Integer
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
~/ : cardano-cli address build --payment-script-file smart-contracts/mint-if-paid-to-0-2.plutus $MAGIC --out-file smart-contracts/mint-if-paid-to-0-2.addr
~/ : cat smart-contracts/mint-if-paid-to-0-2.addr 
addr_test1wq0hmhjc928djzkzu6nlzrkld2a0p3xge9g5358cl9yjstsph7f3f 
```
As with the public key hash, the address `addr_test1wq0hmhjc928djzkzu6nlzrkld2a0p3xge9g5358cl9yjstsph7f3f` is only an example of how your address should look.
You are now ready for interacting with your minting policy
