# Building the smart contracts repository

## Prerequisites
Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md) 

## Clone this repository
```bash
git clone https://github.com/cent-development/basic-smart-contracts.git
```

## Start NIX Shell
Change directory into the folder of your local clone of the plutus-apps repository
```bash
cd plutus-apps
nix-shell
```
## Compile the repository code
In the NIX Shell, change directory into the basic-smart-contracts folder
```bash
cd <directory of basic-smart-contracts>
cabal build
```
