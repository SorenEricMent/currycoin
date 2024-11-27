# CPSC 312 Project

# RSAndbox

RSAndbox is a Prolog-based interactive cryptography sandbox centered around helping students and developers understand RSA encrpytion through guided experimentation.

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

It is in such fulfillment as:
+ It is built on our Prolog learnt from CPSC 312
+ Used useful new element of Prolog - Failure-driven loop(repeat/cut/findall/forall), constraint logics, Prolog arithmetics.
+ Very cool for general audience as it is semantically sound - due to the nature of prolog.
+ This project aims to help users develop an intuition for abstract cryptographical concepts through logical deduction and interactive exploration.
## Team Members

Our team is:

+ Jessie Zhong (student #50544220)
+ Yunchuan Hu (student #90325788):  "Winslow Flandre"
+ Yuteng Liang (student #56100415): "Yuuta"

We call ourselves: __Knights Of the Lambda Calculus__

![Knights_of_the_Lambda_Calculus](https://i.ibb.co/9VTSJX1/Knights-of-the-Lambda-Calculus.png)

## Acknowledgment

We surely built on the work of others! Here are resources and people we got support from:

+ The Prolog crypto library - they made the ground work much easier with prime generation and multiplicative inverse (we can implement them but with a huge time complexity penalty)
+ ChatGPT, with all contribution marked with commment.
+ **R**ivest, **S**hamir, **A**dleman
## Product Pitch

RSAndbox is an **educational, RSA cryptography sandbox/toolikt** designed to help eager learners and encryption enthusiasts explore the principles of public-key cryptography in an interactive, sandbox-style environment.

While many implementations of RSA exist, they often treat the underlying **mathmatical operations and algorithm** (such as prime selection) behind key/message encryption as a "**black box**" of sorts. While learners may be able to understand **use** RSA, they miss out and the **how** and **why**; this is something RSAndbox trys to amend.

Our goal is is to demystify the core processes behind RSA by breaking down it's components into understandable pieces. By expressing RSA operations as **logical predicates**, RSAndbox makes these relationships **explicit and explorable** through Prolog's powerful constraint system and logical predicates. Users can intuitively test inputs and outputs and visualize the logical constraints that govern RSA.

On top of building upon the framework layed by our MVP and PoC, the final product would also likely include:

* The ability to  **encrypt** and **decrypt** messages to demonstrate how plain messages are transformed into cipher and back
* A **visual GUI component** for lower barrier to entry and step-by-step intuitive feedback
* Simulate common RSA **vulnerabilities** like low exponent or small prime factor attacks <!-- ChatGPT suggestion -->
* **Guided practice** through forms of quizzing and problem solving exercises

## Minimal Viable Project

For our MVP, we plan to implement **peer-to-peer message signing and verification** using RSA digital signatures. This (along with the key generation from the PoC) would provide a **minimal** but **fundamental implemntation** of RSA cryptosystem that could serve as a basis for further expansion. The interfaces implemented fufill core parts of the RSA public key framework without the complete suite of encryption features.

Key elements include:

* **RSA Key Generation**: Generating public and private key pairs in Prolog
* **Message Signing**: Allowing users to sign messages using their private key
* **Signature Verification**: Enabling verification of messages with a public key

By focusing solely on signing and verification, our MVP keeps the scope manageable while remaining highly instructive. Users can see firsthand how a signed message can be authenticated by verifying the sender’s signature with their public key and gain intuitive understanding of these core processes through Prolog's logical paradigm.

## Proof of Concept

Our PoC implements the fundamental components of encryption messaging: RSA key generation and distrubition system primarily through use of Prolog's logical predicates.

To generate a RSA key pair, the program has to:

1. Choose two random large prime numbers $p$ and $q$, having a large difference.
2. Calculate the **modulus**: $n = p \cdot q$
3. Compute $\lambda{(n)}$, the Carmichael's totient function.
4. Determine the **public exponent** $e$: It should be a coprime of $\lambda{(n)}$. It is commonly chosen to be $65537$.
5. Determine the **private exponent** $d \equiv e ^ {-1} (\mod{\lambda{(n)}})$.

Prolog is suitable for the implemention of such an algorithm. Because it is mostly pure mathematical relationships, we can just define the logic predicates in Prolog and let Prolog to find the numbers needed. And because we only define the relationships between those algebras the semantics of our algorithm is easy to proven sounds as per Prolog's nature.
The initial $p$ and $q$ can be randomly selected using the Prolog crypto library (`crypto_generate_prime()`). We used key length of 128 in the PoF, but 4096 is also tested to work.

This proof of concept gives us confidence moving forward as it implements the most fundamental and mathmatically complex portion of RSA encrpytion and lays the foundation for building towards the fully functional sandbox environment. Implementing the key generation algorithm is also the most important part of this project.

### How to test our PoC

To generate a key pair, call `generate_key()` with `N`, `E`, `D`, `P`, and `Q` variables.

```prolog
?- [code].
true.
?- generate_key(N, E, D, P, Q).
Your public exponent: 65537
Your public modulus: 99609037685710722854410064360530116406745820465652251039508611856927889584733
Your private exponent: 13750445762586400196283745857602819218637249763534429637524366563462267376796
N = 99609037685710722854410064360530116406745820465652251039508611856927889584733,
E = 65537,
D = 13750445762586400196283745857602819218637249763534429637524366563462267376796,
P = 326983594072741981738357465801814270231,
Q = 304630077751091473114353723443012380843.
```

The line:

```prolog
generate_prime(X) :- crypto_generate_prime(4096, X, []).
```

defines the key length to be 128bits. This value can be changed to other desirable key lengths (like 4096).

You can also run some substeps, like, coprime_list(List, N), which generate a list of all coprimes to N from 1. for Carmichael's totient function, we specially wrote two implementations: one is a translation of the equation to prolog (λ(Result, N)), one is the analytics implementation specifcally for RSA's case with a much better time complexity (the former is too high for anything past 7 bit), that is λ_lcm(P, Q, N).

### How to test and run the code: Prolog
N/A. The standard Prolog installation is sufficient. 
Simply `swipl code.pl` inside the `/prolog` folder. 
