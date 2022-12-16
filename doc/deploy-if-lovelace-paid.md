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

The Cardano CLI commands were run using version 1.35.4
```
~  : cardano-cli --version
cardano-cli 1.35.4 - linux-x86_64 - ghc-8.10
git rev ebc7be471b30e5931b35f9bbc236d21c375b91bb
```

## Configure minting policy
Usage of the minting policy will require a fee to be paid to a specific wallet for each of the tokens minted. We will now configure the minting policy so it knows which wallet to check and the amount required for minting. 

### Generate payment address for owner
If you already have an address, you can skip this section
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file owner-wallet.vkey --signing-key-file owner-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file owner-wallet.vkey $MAGIC --out-file owner-wallet.addr
```

### Fetch Payment pub key hash for address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file owner-wallet.vkey --out-file owner-wallet.pkh
```

The contents of your owner-wallet.pkh should now be a 56 byte hex, similar to but not identical to `3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c`. 
All wallets have their own unique public key hash

## Serialize minting policy script
Time has come to build your unique minting policy. This is accomplished with the following command.
This works in the way that the `mint-if-ada-paid-to` executable compiles your minting policy using four parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-paid-to-1-0.plutus` |
| 2 | wallet pub key hash | `3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c` | 
| 3 | amount of lovelaces per token minted | `4000000` |
| 4 | token name allowed to mint | `Membership` |
```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-ada-paid-to -- plutus-scripts/mint-if-ada-paid-to-address-1-0.plutus 3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c 4000000 Membership
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-ada-paid-to-address-1-0.plutus
 addressToReceivePayment       : 3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c
 lovelace per token            : 4000000
 Parameter to contract         : IfLovelaceContractParam {addressToReceivePayment = 3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c, lovelacePerToken = 4000000, acceptedTokenNameToMint = "Membership"}
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
    "cborHex": "590ccd590cca010<shortened for readability>1004003002200101"
}
```
