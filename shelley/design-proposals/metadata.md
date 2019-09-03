Cardano transaction metadata
============================

This is a draft specification to add support for metadata to Cardano Shelley transactions.

Details
-------

The transaction body format is extended to contain metadata. The structure of the metadata is a mapping from keys to values. The keys are unsigned integers limited in size up to 64 bits. The values are bytes limited in size up to 64 bytes. There is no limit on the number of key-value pairs, except that imposed by the overall transaction size limit.

Note that the metadata is within the transaction body, which is the signed portion of the transaction.

A key aspect of the design is that metadata included in transactions is *not* available for later retrieval from within the ledger validation rules, including scripts. The metadata is not entered into the ledger state, and general historical chain data is not otherwise available to the ledger validation rules.

The changes to the ledger validation rules are thus very limited: only the metadata syntax, metadata size limits and the effect of the metadata on the transaction size calculation and thus the transaction fees. No data is added to the ledger state. The metadata resides only on the chain.

There are no special fees for metadata. This is justified by the fact that the cost to operators is only the one-time processing cost and any long term storage of the blockchain. There is no long term random access state.

The metadata within a transaction will be made available to validation scripts, including Plutus Core scripts. Note again that this is only the immediate transaction being validated. No metadata from predecessor transactions is available.


Design motivation
-----------------

A goal is to add very little complexity to the on-chain part of the system but to get (or allow for) as much functionality as possible, in combination with other features or components. This helps keep implementation complexity lower. Importantly it keeps the size of the trusted base low, by having the complex functionality to use the metadata outside of the trusted base.

A design principle that we preserve is that the historical data on the chain is not needed to validate the next block or transaction. All data needed for later validation must be explicitly tracked in the ledger state. This means the old part of the chain does not need to be preserved locally at all, or at least not in random access storage. This avoids a problem that Ethereum ran into with disk I/O becoming a performance bottleneck. This is why the design does not include metadata into the ledger state, and does not make it accessible to later scripts.


Use in combination with other features
--------------------------------------

With an indexing service, like an explorer, it becomes possible off-chain to collect and query the metadata that is posted.

Using HD wallet schemes, the authenticity of the metadata can be ensured. Depending on the HD scheme -- using public or non-public key derivation -- the metadata could be publicly verifiable, or only privately verifiable.

For example, a simple scheme to track the issuance of physical items could involve the original owner posting metadata within transactions that spend from a designated wallet. An indexing server that knows the HD wallet structure (and either public or private keys depending on the HD scheme) can track the wallet and index all the metadata in transactions from that wallet.

Such schemes have a great deal of flexibility since there is a lot of flexibility in HD wallet schemes. With public HD derivation, the indexing server does not need any signing keys, just an appropriate verification key of a sub-tree in the HD wallet space. If that verification key is revealed then anyone can reliably run the indexing service, and anyone can verify that the metadata is authentic. If the verification key is not revealed then only the owner can run the indexing service, and be used to implement some lookup / verification service, or it can reveal the authenticity of a particular address without revealing all addresses.

It is even possible in principle to use multi-signature wallets, or wallets involving scripts. There just needs to be some wallet scheme that the indexing service can use to reliably track and authenticate the transactions using the wallet.

Obviously, to take advantage of these possibilities requires suitable wallet and indexing components. These are however independent components and their complexity does not impact the complexity of the on-chain rules, so does not add to the size of the trusted base of the overall system.


Relation to EUTxO data values
-----------------------------

This section needs more thought and feedback.

Data values are part of the transaction witness, not the transaction body.

Data value hashes are included in the output of a transaction, and only provided in full in the input of a consuming transaction.

By contrast, metadata values are included in the transaction body, not the witnesses.


CDDL CBOR binary schema
-----------------------

    metadata = {* uint => bstr . size (0..64) }

