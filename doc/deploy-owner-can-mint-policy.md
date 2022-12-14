# Owner Can Mint Policy
The `OwnerCanMintPolicy` contract only accepts minting of tokens for transactions signed by a specific owner wallet. The minting / burning is free and as many tokens as one wants can be minted

## Prerequisites
- Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md)
- Local copy of this repository, built according to [building instructions](building-the-basic-smart-contracts-repo.md)
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)

## Brief
The following instructions are for deploying the built OwnerCanMintPolicy to Cardano blockchain.
Instructions are the same for all of the Cardano Blockchains, but you need to change the
"Network Magic" parameter according to your environment.
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
One wallet owns the minting policy. The owner is the only wallet allowed to do minting using the policy. This is how you create the wallet and getting hold of the payment pub key hash needed for configuring the minting policy

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file owner-wallet.vkey --signing-key-file owner-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file owner-wallet.vkey $MAGIC --out-file owner-wallet.addr
```

### Fetch Payment pub key hash for owner address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file owner-wallet.vkey --out-file owner-wallet.pkh
```

The contents of your owner-wallet.pkh should now be a 56 byte hex, similar to but not identical to `3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c`. 
All wallets have their own unique public key hash

## Serialize minting policy script
Time has come to build your unique minting policy. This is accomplished with the following command.
This works in the way that the `basic-minting-policy` executable compiles your minting policy using two parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/owner-can-mint-policy-1-0.plutus` |
| 2 | owner wallet payment pub key hash | `3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c` | 

```
[nix-shell:~/basic-smart-contracts]$ cabal exec owner-can-mint-policy -- plutus-scripts/owner-can-mint-policy-1-0.plutus 3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c
_______________________________________________
 Policy saved to file       : plutus-scripts/owner-can-mint-policy-1-0.plutus
 PubKeyHash of owner wallet : 3723e650b82e52a915cae8504362d4cb16dda3ddb9311879a28dac5c
 policyOwner    (obj type)  : PaymentPubKeyHash
_______________________________________________

[nix-shell:~/basic-smart-contracts]$ 
```
The contents of your minting policy plutus script file should now look similar to
```
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "5908a259089f010<shortened for readability>311879a28dac5c0001"
}
```

## Generating minting policy script address
The final step is to generate the minting policy script address. This address is used when you interact with it

```
~/smart-contracts  : cardano-cli address build --payment-script-file owner-can-mint-policy-1-0.plutus $MAGIC --out-file owner-can-mint-policy-1-0.addr
~/smart-contracts  : cat owner-can-mint-policy-1-0.addr 
addr_test1wqpag056z5ag3mkmge3nmxkgmwt38llpz27w7dn3gc04yvq2c9a70
~/smart-contracts  : 
```
As with the public key hash, the address `addr_test1wqpag056z5ag3mkmge3nmxkgmwt38llpz27w7dn3gc04yvq2c9a70` is only an example of how your address should look.
You are now ready for interacting with your minting policy
