# If Token Paid Minting Policy used from Dapp (sample)
Because there are so many different platforms for web applications, we simply provide sample off-chain javascript to interact with the smart contract instead of doing a complete dapp.
The library we use in this example is the great [lucid](https://github.com/spacebudz/lucid)

To make this sample even more interesting, we are showing how to request an amount of stable coin Djed before accepting mints. Just think about the possibilities this gives you:)
**If you want to do the same on mainnet, make sure to provide the correct policy id for Djed stable coin, or any other stable coin or token.**

```js
async function payDjedToMint() {

  // connect to the wallet
  const lucid = await connectwallet();
  const utils = lucid.utils;

  // the smart contract and redeemer
  const unitRedeemer = () => Data.empty();
  const payDjedToMintScript = {
    type: "PlutusV1",
    script: "insert the plutus cbox hex here..."
  }
  
  // calculate policy id of minting policy and token to mint with that policy
  const mintingPolicyId = utils.mintingPolicyToId(payDjedToMintScript);
  const tokenToMint = mintingPolicyId + utf8ToHex("insert token name to mint"); // make sure the token name is allowed by the minting policy

  // provide information about the Djed **testnet** token accepted as payment
  const djedPolicyID = "9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe"
  const djedTokenName = utf8ToHex("Djed_testMicroUSD");
  const djedToken   = djedPolicyID + djedTokenName; 

  // get payment pub key has of currently loaded wallet
  const bech32_addr = await lucid.wallet.address();
  const addrDetails = utils.getAddressDetails(bech32_addr);
  const paymentKeyHash = addrDetails.paymentCredential.hash; 
  
  // create transaction that mints the desired token and pays the required Djed per token minted
  const tx = await lucid
    .newTx()
    .payToAddress("addr...", {[djedToken]: 10000000n }) // 10 (testnet) Djed
    .mintAssets({[tokenToMint]: 1n }, unitRedeemer())
    .attachMintingPolicy(payDjedToMintScript)
    .addSignerKey(paymentKeyHash)
    .complete();

  // sign and submit transaction
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log(`transaction id ${txHash}`);  
}
```

For the above code to be successful, the ```payToAddress()``` must pay the required number of Djed to the address the contract is monitoring. The paying wallet will receive the minted token(s).
