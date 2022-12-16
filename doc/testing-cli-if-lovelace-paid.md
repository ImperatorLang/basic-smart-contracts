# Testing the If Lovelace Paid Minting Policy with Cardano CLI
The `IfLovelacePaidMintingPolicy` contract checks if a certain amount of lovelace is paid to a particular wallet before allowing to mint any tokens and that the correct amount is paid compared to the number of tokens requested.
These instructions show how to test and interact with the contract using the Cardano CLI.

## Prerequisites
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)
- Built and configured `IfLovelacePaidMintingPolicy` contract according to the [instructions](deploy-if-lovelace-paid.md) 

## Brief
The following instructions are for interacting with the built IfLovelacePaidMintingPolicy on the Cardano blockchain.
Instructions are the same for all of the Cardano Blockchains, but you need to change the
"Network Magic" parameter according to your environment.
Use one of the following Network Magics according to which network you want to work
| Network | Magic | Command |
| --- | --- | --- |
| Preview | `--testnet-magic 2` | `export MAGIC="--testnet-magic 2"` |
| Pre-Production | `--testnet-magic 1` | `export MAGIC="--testnet-magic 1"` |
| Mainnet | `--mainnet` | `export MAGIC="--mainnet"` |

The Cardano CLI commands are compatible with version 1.35.4
```
~  : cardano-cli --version
cardano-cli 1.35.4 - linux-x86_64 - ghc-8.10
git rev ebc7be471b30e5931b35f9bbc236d21c375b91bb
```

## Determine the Policy ID of your minting policy
To be able to mint tokens with your new shining minting policy, you will need to know its policy id. Use the following command to determine this
```
~/ : cardano-cli transaction policyid --script-file smart-contracts/mint-if-ada-paid-to-address-1-0.plutus 
4649824e7b26b43ececdc6b6eacae59b1f38f4ca4001bec4c5ef8b37
~/ : 
```
The policy id of the contract in this example is `4649824e7b26b43ececdc6b6eacae59b1f38f4ca4001bec4c5ef8b37`. Your contract will have a similar 56 bit hex, but not the same, as every contract is unique.

## Generate Hex name of your token
Cardano CLI needs the token name in hexadecimal form. We have provided a tool for determining this. In this example we will determine the hex for a token called `Membership` as this is the only token allowed to mint with this policy
The command is as follows
```
[nix-shell:~/basic-smart-contracts]$ cabal exec token-name -- Membership       
4d656d62657273686970

[nix-shell:~/basic-smart-contracts]$ 
```

## Minting Membership tokens using the minting policy with the owner wallet
The CLI is very low level and requires all the correct values in exactly the correct place to work.
The policy id and token name generated above will come in handy now. We also need to specify the transaction inputs and outputs to tell the node what tokens go to which address.

First step is to query the owner address for UTxOs to spend.

```
~/ : cardano-cli query utxo --address $(cat wallets/owner-wallet.addr) $MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
34bdbfefb111e436aefe81f123199fb7784dbdf330273a34a79fc5efbd287f34     1        9998175243 lovelace + TxOutDatumNone
~/ : 
```

In this example, we make use of the same UTxO both for the fees and collateral, but your are free to use different if you prefer.
Using this UTxO, we can then construct the CLI build command to mint 100 Membership tokens
```
cardano-cli transaction build \
    --babbage-era \
    $MAGIC \
    --tx-in 34bdbfefb111e436aefe81f123199fb7784dbdf330273a34a79fc5efbd287f34#1 \
    --tx-in-collateral 34bdbfefb111e436aefe81f123199fb7784dbdf330273a34a79fc5efbd287f34#1 \
    --tx-out $(cat wallets/owner-wallet.addr)+4000000 \
    --tx-out $(cat wallets/owner-wallet.addr)+1500000+" 1 4649824e7b26b43ececdc6b6eacae59b1f38f4ca4001bec4c5ef8b37.4d656d62657273686970" \
    --mint "1 4649824e7b26b43ececdc6b6eacae59b1f38f4ca4001bec4c5ef8b37.4d656d62657273686970" \
    --mint-script-file smart-contracts/mint-if-ada-paid-to-address-1-0.plutus \
    --mint-redeemer-file smart-contracts/unit.json   \
    --change-address $(cat wallets/owner-wallet.addr) \
    --protocol-params-file babbage-params.json \
    --out-file tmp-txs/tx.body \
    --required-signer wallets/owner-wallet.skey
```
Make sure to replace the values above with the correct ones according to your wallet, contract and token name. When the transaction is submitted, you should see a message similar to `Estimated transaction fee: Lovelace 446292
`. If not, something is not set up correctly. Make changes to the transaction until you see this message. 
**Note that the transaction will fail already at this point if you try to mint token with different name, or pay too little or to the wrong address.**

If you see the estimated transaction fee, it means you have created a valid transaction and are ready to submit to the blockchain.
The remaining commands are
```
cardano-cli transaction sign \
--tx-body-file tmp-txs/tx.body \
--signing-key-file wallets/owner-wallet.skey \
$MAGIC \
--out-file tmp-txs/tx.signed

cardano-cli transaction submit \
$MAGIC \
--tx-file tmp-txs/tx.signed
``` 
If it is successful, you should see the message `Transaction successfully submitted.`

When you now query the owner wallet, you will see your newly minted token(s). Since we also did this using the wallet that was to receive the minting fee, we can see that 4000000 (4 ADA) has appeared in its own UTxO as payment for the token.
```
~/ : cardano-cli query utxo --address $(cat wallets/owner-wallet.addr) $MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb     0        4000000 lovelace + TxOutDatumNone
2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb     1        1500000 lovelace + 1 4649824e7b26b43ececdc6b6eacae59b1f38f4ca4001bec4c5ef8b37.4d656d62657273686970 + TxOutDatumNone
2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb     2        9992228951 lovelace + TxOutDatumNone
~/ : 
```
