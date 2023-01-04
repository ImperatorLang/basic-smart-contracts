# Owner can mint used from Dapp (sample)
Because there are so many different platforms for web applications, we simply provide sample off-chain javascript to interact with the smart contract instead of doing a complete dapp.
The library we use in this example is the great [lucid](https://github.com/spacebudz/lucid)

```js
async function ownerCanMint() {

  // connect to the wallet
  const lucid = await connectwallet();
  const utils = lucid.utils;

  // the smart contract and redeemer
  const unitRedeemer = () => Data.empty();
  const mintWhenOwnerScript = {
    type: "PlutusV1",
    script: "insert the plutus cbox hex here..."
  }
  
  // calculate policy id of minting policy and token to mint with that policy
  const mintingPolicyId = utils.mintingPolicyToId(mintWhenOwnerScript);
  const tokenToMint = mintingPolicyId + utf8ToHex("insert token name to mint") // make sure the token name is allowed by the minting policy

  // get payment pub key has of currently loaded wallet
  const bech32_addr = await lucid.wallet.address();
  const addrDetails = utils.getAddressDetails(bech32_addr);
  const paymentKeyHash = addrDetails.paymentCredential.hash; 
  
  // create transaction that mints the desired token
  const tx = await lucid
    .newTx()
    .mintAssets({[tokenToMint]: 1n }, unitRedeemer())
    .attachMintingPolicy(mintWhenOwnerScript)
    .addSignerKey(paymentKeyHash)
    .complete();

  // sign and submit transaction
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log(`transaction id ${txHash}`);  
}
```

If the above code is run from a dapp where the owner wallet of the minting policy is loaded in the wallet connector, the owner wallet will receive the minted token(s).
Since this minting policy is meant to be run only by the owner, no costs are required in addition to the regular transaction fees.

Running this using a non-owner wallet will fail as the smart contract will reject the minting request (as designed)
