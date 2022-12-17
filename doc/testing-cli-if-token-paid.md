# Testing the If Token Paid Minting Policy with Cardano CLI
The `IfTokenPaidMintingPolicy` checks if a certain amount of a specific token is paid to a particular wallet before allowing to mint any tokens. The contract also checks that the amount is enough according to the number of tokens minted. The provided fee is the cost for each token minted, so if the transaction mints 3 tokens, the fee is also 3 times the fee
These instructions show how to test and interact with the contract using the Cardano CLI.

## Prerequisites
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)
- Built and configured `IfTokenPaidMintingPolicy` contract according to the [instructions](deploy-if-token-paid.md) 

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
To be able to mint the tokens, you will need to calculate the policy id of the finished minting policy. This is done with the following command.
```
~/ : cardano-cli transaction policyid --script-file smart-contracts/mint-if-testnet-djed-paid-to-address-1-0.plutus
80aaee7ea07a40b480ac0a88b4d75ca104885d7799daa9d25dc1b168
~/ : 
```

The policy id of the contract in this example is `80aaee7ea07a40b480ac0a88b4d75ca104885d7799daa9d25dc1b168`. Your contract will have a similar 56 bit hex, but not the same, as every contract is unique.

## Generate Hex of the token names
Cardano CLI needs the token name in hexadecimal form. We have provided a tool for determining this. 
In this contract, we need two token names, the payment token Djed_testMicroUSD and the token to mint Membership. 
The commands are as follows
```
[nix-shell:~/basic-smart-contracts]$ cabal exec token-name -- Membership       
4d656d62657273686970

[nix-shell:~/basic-smart-contracts]$ cabal exec token-name -- Djed_testMicroUSD
446a65645f746573744d6963726f555344

[nix-shell:~/basic-smart-contracts]$ 
```

## Minting Membership tokens paid with testnet Djed
The CLI is very low level and requires all the correct values in exactly the correct place to work.
The policy id and token name generated above will come in handy now. We also need to specify the transaction inputs and outputs to tell the node what tokens go to which address.

First step is to query the paying address for UTxOs to spend. We need UTxO with the Djed token and a lovelace only UTxO to be used for collateral
```
~/ : cardano-cli query utxo --address $(cat wallets/owner-wallet.addr) $MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb     2        9992228951 lovelace + TxOutDatumNone
6ec87b91ddb4150519948c52c8432566cd09c352e159d55294a66722e0d95576     0        200000000 lovelace + 100000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe.446a65645f746573744d6963726f555344 + TxOutDatumNone
~/ : 
```

In this example, we make use of the same UTxO both for the fees and collateral, but your are free to use different if you prefer. The second UTxO is used for paying for the mint with Djed.
Using these UTxOs, we can then construct the CLI build command to mint 20 Membership tokens, that cost 10 Djed each

```
cardano-cli transaction build \
    --babbage-era \
    $MAGIC \
    --tx-in 2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb#2 \
    --tx-in 6ec87b91ddb4150519948c52c8432566cd09c352e159d55294a66722e0d95576#0 \
    --tx-in-collateral 2b591f9d8e00d3e302ca4962ce2b6d230e46b9f1dced0169d2324a32183cbecb#2 \
    --tx-out $(cat ~/testnets/preprod/wallets/contract-token-treasury.addr)+1500000+"10000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe.446a65645f746573744d6963726f555344" \
    --tx-out $(cat ~/testnets/preprod/wallets/owner-wallet.addr)+1500000+"90000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe.446a65645f746573744d6963726f555344" \
    --tx-out $(cat ~/testnets/preprod/wallets/owner-wallet.addr)+1500000+"1 80aaee7ea07a40b480ac0a88b4d75ca104885d7799daa9d25dc1b168.4d656d62657273686970" \
    --mint "1 80aaee7ea07a40b480ac0a88b4d75ca104885d7799daa9d25dc1b168.4d656d62657273686970" \
    --mint-script-file ~/testnets/preprod/smart-contracts/mint-if-testnet-djed-paid-to-address-1-0.plutus \
    --mint-redeemer-file ~/testnets/preprod/smart-contracts/unit.json   \
    --change-address $(cat ~/testnets/preprod/wallets/owner-wallet.addr) \
    --protocol-params-file ~/testnets/preprod/babbage-params.json \
    --out-file ~/testnets/preprod/tmp-txs/tx.body \
    --required-signer ~/testnets/preprod/wallets/owner-wallet.skey
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

When you now query the owner wallet, you will see your newly minted Membership token and the remaining 90 Djed.
```
~/ : cardano-cli query utxo --address $(cat wallets/owner-wallet.addr) $MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7dbaea6b3d9e989cb788d71a5baede02d3705883ca3006b84a9e12aa001c54d2     1        1500000 lovelace + 90000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe.446a65645f746573744d6963726f555344 + TxOutDatumNone
7dbaea6b3d9e989cb788d71a5baede02d3705883ca3006b84a9e12aa001c54d2     2        1500000 lovelace + 1 80aaee7ea07a40b480ac0a88b4d75ca104885d7799daa9d25dc1b168.4d656d62657273686970 + TxOutDatumNone
7dbaea6b3d9e989cb788d71a5baede02d3705883ca3006b84a9e12aa001c54d2     3        10187287991 lovelace + TxOutDatumNone
~/ : 
```

When querying the contract treasury wallet, you can see that the 10 Djed were transferred as payment for the Membership token 
```
~/ : cardano-cli query utxo --address $(cat wallets/contract-token-treasury.addr) $MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7dbaea6b3d9e989cb788d71a5baede02d3705883ca3006b84a9e12aa001c54d2     0        1500000 lovelace + 10000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe.446a65645f746573744d6963726f555344 + TxOutDatumNone
a0a43a9eb6fdae610075358a763872aa7a42d16249fc481fa2e99126b1a73a81     1        8691978614 lovelace + TxOutDatumNone
~/ : 
```
