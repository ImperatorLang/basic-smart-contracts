# Basic smart contracts

This repository is dedicated to all things regarding Cardano smart contract development.

It contains a collection of smart contract related resources created in our own development. The resources are targeted to be reusable components for you to customize to your own needs or to use as basis for your own smart contracts.

The objective of the site is not to be a course you follow from Lecture 1 until Lecture X. The components of this site is meant to be as self containing as possible. If a component depends on other components we list these dependencies in the topic brief.

## Getting started
The supplied code can be used in your existing development setup if you already have one.
If you do not have a running environment you can use the following instructions to set it up
- [Install Plutus](doc/installing-plutus.md)
- [Clone and build this repository](doc/building-the-basic-smart-contracts-repo.md)

## The contracts
| Contract | Building instructions |
| --- | --- |
| [Basic Minting Policy](src/BasicMintingPolicy.hs) | [Instructions](doc/deploy-basic-minting-policy.md) |
| [Mint if Lovelace Paid to Wallet](src/IfLovelacePaidMintingPolicy.hs) | [Instructions](doc/deploy-if-lovelace-paid.md) |
| [Mint if Token Paid to Wallet](src/IfTokenPaidMintingPolicy.hs) | [Instructions](doc/deploy-if-token-paid.md) |
