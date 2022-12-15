# Basic smart contracts

## About this repository
This repository is one way for **CENT Stake Pool** to contribute open source smart contract code to the Cardano community.
It contains a collection of smart contracts that are designed to be as small and to the point as possible. They are small because we want them to be easy to reuse and extend without the need to read through and understand 100's of lines of code and end up removing most of them because they do not fit the use case you want. Our hope is that these contracts will fill the gap between the too simple contracts like [Plutus Pioneer Program - AlwaysSucceeds](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week02/src/Week02/Gift.hs) and the (in some cases too) advanced contracts you can find in IOG repositories like [Plutus Use Cases](https://github.com/input-output-hk/plutus-use-cases).

The contracts themselves are extracts we have picked from our own development and published here to be used as starting points for other smart contracts, but they can also be used for simple use cases as they are. This is possible because the contracts become uniquely linked to your wallet when you build them.

We plan to continuously publish new smart contracts, so make sure to watch this repository to be notified.

## Getting started
The supplied code can be used in your existing development setup if you already have one.
If you do not have a running environment you can use the following instructions to set it up
- [Install Plutus](doc/installing-plutus.md)
- [Clone and build this repository](doc/building-the-basic-smart-contracts-repo.md)

## The contracts
| Contract | Building instructions | Testing |
| --- | --- | --- |
| [Owner Can Mint Policy](src/OwnerCanMintPolicy.hs) | [Instructions](doc/deploy-owner-can-mint-policy.md) | [using CLI](doc/testing-cli-owner-can-mint.md) | 
| [Mint if Lovelace Paid to Wallet](src/IfLovelacePaidMintingPolicy.hs) | [Instructions](doc/deploy-if-lovelace-paid.md) | |
| [Mint if Token Paid to Wallet](src/IfTokenPaidMintingPolicy.hs) | [Instructions](doc/deploy-if-token-paid.md) | |

## How to contact CENT
We would love to hear from you. These are our socials.
| Platform | |
| --- | --- |
| Telegram | @CentStakePool |
| Twitter | [@CentStakePool](https://twitter.com/CentStakePool) |
| Discord | CENT#6930 |
| Email | developer@cent.stakepoolcentral.com |
| Website | https://cent.stakepoolcentral.com |
