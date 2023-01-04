# Basic smart contracts

## About this repository
This repository is [CENT Stake Pool's](https://cent.stakepoolcentral.com) way to contribute open source smart contract code to the Cardano community.
It contains a collection of smart contracts that are designed to be as small and to the point as possible. They are small because we want them to be easy to reuse and extend without the need to read through and understand 100's of lines of code, or even end up removing most of the code because it doesn't fit the use case you want. Our hope is that these contracts will fill the gap between the too simple contracts like [Plutus Pioneer Program - AlwaysSucceeds](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week02/src/Week02/Gift.hs) and the (in some cases too) advanced contracts you can find in IOG repositories like [Plutus Use Cases](https://github.com/input-output-hk/plutus-use-cases).

The contracts themselves are extracts we have picked from our own development and published here to be used as starting points for other smart contracts, but they can also be used for simple use cases as they are. This is possible because the contracts become uniquely linked to your wallet when you build them.

## Getting started
The supplied code can be used in your existing development / production environment if you already have one.
If you do not have a running environment you can use the following instructions to set one up
- [Install Plutus](doc/installing-plutus.md)
- [Clone and build this repository](doc/building-the-basic-smart-contracts-repo.md)

## The contracts
| Contract | Building instructions | Testing | Dapp sample |
| --- | --- | --- | --- |
| [Owner Can Mint Policy](src/OwnerCanMintPolicy.hs) | [Instructions](doc/deploy-owner-can-mint-policy.md) | [using CLI](doc/testing-cli-owner-can-mint.md) | [using lucid](doc/dapp-sample-lucid-owner-can-mint.md)
| [Mint if Lovelace Paid to Wallet](src/IfLovelacePaidMintingPolicy.hs) | [Instructions](doc/deploy-if-lovelace-paid.md) | [using CLI](doc/testing-cli-if-lovelace-paid.md)| |
| [Mint if Token Paid to Wallet](src/IfTokenPaidMintingPolicy.hs) | [Instructions](doc/deploy-if-token-paid.md) | [using CLI](doc/testing-cli-if-token-paid.md) | |

## How to contact CENT 
| Platform | <img width=50 height=50 src="https://cent.stakepoolcentral.com/resources/SPC.png"> |
| --- | --- |
| Telegram | @CentStakePool |
| Discord | CENT#6930 |
| Email | developer@cent.stakepoolcentral.com |
| Website | https://cent.stakepoolcentral.com |

## Disclaimer
Even though we have tested our smart contracts, make sure to do your own thorough testing on Cardano testnet before publishing your contract to the Cardano mainnet. The contracts are shared with you free of charge to be an inspiration for your own contracts. Be sure to read and understand the repository [LICENSE](LICENSE)




