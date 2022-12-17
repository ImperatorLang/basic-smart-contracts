# If Token Paid Minting Policy
The `IfTokenPaidMintingPolicy` checks if a certain amount of a specific token is paid to a particular wallet before allowing to mint any tokens. 
The contract also checks that the amount is enough according to the number of tokens minted. The provided fee is the cost for each token to mint, so if the transaction mints 3 tokens, the cost is also 3 times the fee.
A use case for this minting policy, is to require a payment in stable coins like **Djed** or **USDA** to mint the requested amount of tokens

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

The Cardano CLI commands are compatible with version 1.35.4
```
~  : cardano-cli --version
cardano-cli 1.35.4 - linux-x86_64 - ghc-8.10
git rev ebc7be471b30e5931b35f9bbc236d21c375b91bb
```

## Configure minting policy
Usage of the minting policy will require an amount of tokens to be paid to a specific wallet. We will now configure the minting policy so it knows which wallet to verify correct amount of tokens paying for the minting. 

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file mint-treasury-wallet.vkey --signing-key-file mint-treasury-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file mint-treasury-wallet.vkey $MAGIC --out-file mint-treasury-wallet.addr
```

### Fetch Payment pub key hash for address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file mint-treasury-wallet.vkey --out-file mint-treasury-wallet.pkh
```

The contents of your mint-treasury-wallet.pkh should now be a 56 byte hex, similar to but not identical to `ce7e716960ebcea4e07b62cf8a2962ba9b0a6f852ee452ef108f977d`. 
All wallets have their own unique key hash

## Serialize minting policy script
Time has come to build your unique minting policy. This is accomplished with the following command.
This works in the way that the `mint-if-token-paid-to` executable compiles your minting policy using six parameters (following --)

**NOTE: It is important that you verify the policy id (currency symbol) of the token you require as payment.** In this example, we require testnet Djed as payment for our tokens.
Policy id of Testnet Djed (Preprod) is `9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe` and the token name is **Djed_testMicroUSD**
To get hold of your preferred tokens policy id, please refer to the website of the token creators.

Example stable coins:
| Token | Stable coin provider | Web site | Status |
| --- | --- | --- | --- |
| iUSD | Indigo protocol | https://app.indigoprotocol.io/ | Mainnet |
| USDA | Emurgo | https://www.anzens.com/ | Announced |
| Djed | IOG and COTI | https://djed.xyz/ | Testnet, Mainnet January 2023 |

When you have determined the policy id to use, you can use this as a parameter when building your contract as follows
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/mint-if-testnet-djed-paid-to-address-1-0.plutus` |
| 2 | wallet pub key hash | `ce7e716960ebcea4e07b62cf8a2962ba9b0a6f852ee452ef108f977d` | 
| 3 | amount of desired tokens required to mint | `10000000` |
| 4 | policy id of required token | `9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe` |
| 5 | token name of required token | `Djed_testMicroUSD` |
| 6 | token name allowed to mint | `Membership` |

In this example, the minting policy is set to require 10000000 Djed_testMicroUSD (10 Djed) tokens to mint one Membership token and the Djed must be sent to the provided wallet in order to be allowed to mint the Membership token.

```
[nix-shell:~/basic-smart-contracts]$ cabal exec mint-if-token-paid-to -- plutus-scripts/mint-if-testnet-djed-paid-to-address-1-0.plutus ce7e716960ebcea4e07b62cf8a2962ba9b0a6f852ee452ef108f977d 10000000 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe Djed_testMicroUSD Membership
_______________________________________________
 Policy saved to file          : plutus-scripts/mint-if-testnet-djed-paid-to-address-1-0.plutus
 addressToReceivePayment       : ce7e716960ebcea4e07b62cf8a2962ba9b0a6f852ee452ef108f977d
 Minimum token amount required : 10000000
 Accepted token policy paid    : 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe
 Accepted token name paid      : Djed_testMicroUSD
 Accepted token name to mint   : Membership
 Parameter to contract         : ContractParam {addressToReceivePayment = ce7e716960ebcea4e07b62cf8a2962ba9b0a6f852ee452ef108f977d, acceptedTokenPolicyAsPayment = 9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe, acceptedTokenNameAsPayment = "Djed_testMicroUSD", numTokensAsPaymentForEachMintedToken = 10000000, acceptedTokenNameToMint = "Membership"}
 addressToPay    (obj type)    : PaymentPubKeyHash
 minTokenAmount (obj type)     : Integer
 tokenPolicy paid (obj type)   : CurrencySymbol
 token paid (obj type)         : TokenName
 token to mint (obj type)      : TokenName
_______________________________________________

[nix-shell:~/basic-smart-contracts]$ 

```
The contents of your minting policy plutus script file should now look similar to
```
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "590d16590d13010<shortened for readability>05004003002200101"
}
```
