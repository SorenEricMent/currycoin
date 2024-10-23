# CPSC 312 Project

# Currycoin

Currycoin is a simple blockchain implemented in Haskell. It provides a set of interfaces for its user to test blockchain related actions like creating a transaction or mint a new block on a simple, locally stored, Bitcoin-esque blockchain.

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

It is in such fulfillment as:
+ It is built on our Haskell learning from CPSC 312
+ Used useful new element of Haskell - the project relies on several cryptography libraries from Hackage, and several Haskell language extensions: as of now, FlexibleInstances, UndecidableInstances and GADTs
+ The meaningful problem solved: this project provided a way to "step" blockchain by simple function calls / commands, which is a nice way to visualize and learn how blockchain works.
## Team Members

Our team is:

+ Jessie Zhong (student #50544220)
+ Yunchuan Hu (student #90325788):  "Winslow Flandre"
+ Yuteng Liang (student #56100415): "Yuuta"

We call ourselves: __Knights Of the Lambda Calculus__

## Acknowledgment

We surely built on the work of others! Here are resources and people we got support from:

+ Our dependencies: cryptohash-sha256, ByteString, Crypto.Secp256k1, Haskeline
+ The Bitcoin Project, BIPs
+ ChatGPT, for making some helper function much easier (**All** code with help from this source has been labeled with comment about what help was given)

## Product Pitch

Currycoin is a lightweight, Bitcoin-esque **blockchain educational** project with the aim of helping familiarize people with blockchain fundamentals through the power of functional programming. 

Blockchains are a powerful decentralized technology but can often be seen **inaccessible** due to their **complexity** and **intricacy**. Currycoin aims to demystify the blockchain by offering a transparent implementation that is both feature complete and easily understandable. 

The final product would be an easily approachable blockchain implementation with a real-time, **intuitive visual interface** that would allow users to monitor data and view the effects of their actions such as contracts, block creation, and transactions. A low level interace would also be available for tech-savvy users. Users would also be able to simulate a simple P2P network, allowing for better understanding of the decentralized nature of the blockchain. 

The idea is to offer an approachable interface for anyone to get started with tinkering with the inner workings of a blockchain allowing users to grasp concepts through a **hands-on** learning experience.  Whether it's understanding the role of cryptographic hashes in securing data, how transactions are verified in a decentralized network, or the visual structure of a blockchain, users will be able to learn by doing.

## Minimal Viable Project

For our minimal viable project (MVP), we plan to implement the core components of a blockchain in Haskell:

* **Blocks**: A simple data type that represents a block containing transaction data and a cryptographic hash. Basic building block of the blockchain
    * **Merkle Tree**: Users will also be able to create and verify Merkle proofs through the construction and expansion of a Merkle Tree
* **Transactions**: Our MVP would also support the creation and verification of transactions within the blockchain
* **Bitcoin-style Address**: We plan to implement the Bitcoin addressing scheme (Without SegWit).


The end goal is to expand on the Merkle Tree from the PoC by adding a set of minimal core functions that allow a user to explore the fundamental operations that define a blockchain. Most of these functions would be performed through the terminal.

Our MVP will take advantage of Haskell's functional nature, type safety features and other useful tools to create a pure, minimal but functional, foundation for a basic blockchain that can easily be expanded on to encapsulate more features.

## Proof of Concept

We present the proof of concept of our project with a haskell implementation of [Merkle Tree](https://en.wikipedia.org/wiki/Merkle_tree) and data definitions for key blockchain related concepts (block and transaction).

The Merkle Tree's is one of the foundation of a blockchain - it is essentially how the hash of a block is generated, which is a key part for the proof-of-work consensus, and it allows for Merkle proofs - commitment that a transaction is included in the block.

Beside of this fundamental data structure of blockchain, we also provided data and type definition for key concepts - blocks and transactions.

This gives us the confindence of working towards the minimal viable projects as the rest works are essentially building generation functions that operates on those types and structures.

### How to test our PoC
<!-- Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

Also include instructions for us to test and run your code. (See our guidelines below.) -->

<!-- A good goal to aim for is the top rubric item from proposal grading: -->

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.

### How to test and run the code: Haskell


In our PoC, we have implemented the Merkle Tree and the following interfaces:
* Creating Merkle Tree from lists (In blockchain, a list of transaction. But we have implemented an abstract version and it only takes a Typeclass Hashable)
    * Function: [createMerkleTreeFromList](https://github.students.cs.ubc.ca/tokenjz3/cpsc312-project/blob/f88bf293dfcb21725cacd5e7097efc231d8013d8/haskell/app/Main.hs#L163C1-L163C25)
* Generating a Path inclusion proof from a Merkle Tree and one of its Leaf Node
    * Function: [generateInclusionProof](https://github.students.cs.ubc.ca/tokenjz3/cpsc312-project/blob/f88bf293dfcb21725cacd5e7097efc231d8013d8/haskell/app/Main.hs#L173C1-L173C23)
* Verify a Path Inclusion Proof on a root hash
    * Function: [proveHashableInclusion](https://github.students.cs.ubc.ca/tokenjz3/cpsc312-project/blob/f88bf293dfcb21725cacd5e7097efc231d8013d8/haskell/app/Main.hs#L191C1-L191C23)
    
We have also wrote an overall test function to test all the three interfaces above: [testMerkleTree](https://github.students.cs.ubc.ca/tokenjz3/cpsc312-project/blob/f88bf293dfcb21725cacd5e7097efc231d8013d8/haskell/app/Main.hs#L231), which takes an list and an element, create a MerkleTree out of it, try to construct the element's inclusion proof, and print all results out.

Our PoC is to be tested via `stack ghci` and run those function accordingly, we have also provided a few helper functions like converting ByteString and readable hex string (byteStringToHex), but the easiest way would still be `testMerkleTree`.
Examples of calling `testMerkleTree`
* testMerkleTree ["1", "2", "3", "4"] "1"
* testMerkleTree ["Delicious", "Curry"] "Curry"
* testMerkleTree ["Steve", "Wolfman", "Is", "Awesome"] "Haskell"


#### Running Currycoin Shell (Not for PoC!)
Simply execute `stack build`, if you want to inspect on our example functions, run `stack ghci`, if you want to enter the interactive terminal (Not yet in progress but will be in the actual phase of the project), run the executable from the build (`stack run`).

<!-- 
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try. -->

### How to test and run the code: Prolog
N/A
<!-- Replace this section with instructions to us for how to test and run your code.

We have set up a simple test file for you to extend using [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing. Included among your tests should be some that demonstrate the core functionality of your code. Please remove the example tests before you submit or you will lose marks. (We will be running `make prolog-eval` from the project root.)

In the `prolog` directory, you can run `make test` to run the unit tests. You can also load the test file into the swipl repl with `make test-repl` and in that repl you can run `run_tests.` to run those tests.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!
 -->
