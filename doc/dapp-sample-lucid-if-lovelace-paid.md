# If Lovelace Paid Minting Policy used from Dapp (sample)
Because there are so many different platforms for web applications, we simply provide sample off-chain javascript to interact with the smart contract instead of doing a complete dapp.
The library we use in this example is the great [lucid](https://github.com/spacebudz/lucid)

```js
async function payLovelaceToMint() {

  // connect to the wallet
  const lucid = await connectwallet();
  const utils = lucid.utils;

  // the smart contract and redeemer
  const unitRedeemer = () => Data.empty();
  const payLovelaceToMintScript = {
    type: "PlutusV1",
    script: "insert the plutus cbox hex here..."
  }
  
  // calculate policy id of minting policy and token to mint with that policy
  const mintingPolicyId = utils.mintingPolicyToId(payLovelaceToMintScript);
  const tokenToMint = mintingPolicyId + utf8ToHex("insert token name to mint"); // make sure the token name is allowed by the minting policy

  // get payment pub key has of currently loaded wallet
  const bech32_addr = await lucid.wallet.address();
  const addrDetails = utils.getAddressDetails(bech32_addr);
  const paymentKeyHash = addrDetails.paymentCredential.hash; 
  
  // create transaction that mints the desired token and pay the required lovelace per token
  const tx = await lucid
    .newTx()
    .payToAddress("addr...", { lovelace: 4000000n })
    .mintAssets({[tokenToMint]: 1n }, unitRedeemer())
    .attachMintingPolicy(payLovelaceToMintScript)
    .addSignerKey(paymentKeyHash)
    .complete();

  // sign and submit transaction
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log(`transaction id ${txHash}`);  
}
```

For the above code to be successful, the ```payToAddress()``` must pay the required number of lovelaces to the address the contract is monitoring. The paying wallet will receive the minted token(s).
