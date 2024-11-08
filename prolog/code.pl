%% Key generation
% First, generate two prime number, P and Q
% Then compute N = PQ

generate_prime(X) :- crypto_generate_prime(16, X, []).

binded_mult(X, Y, Z) :- Z is X * Y.
binded_pow(X, Y, Z) := Z is X ** Y.
binded_mod(X, Y, Z) := Z is X mod Y.

coprime(X, Y) :-
    D is gcd(X, Y),
    D == 1.

% Credit: ChatGPT taught me to use findall, really stuck with forall in λ.
% wow failure driven loop actually
% find all coprime to N from 1 to N
coprime_list(List, N) :-
    findall(X, (between(1, N, X), coprime(X, N)), List).

pq_generation(P, Q) :-
    generate_prime(P),
    generate_prime(Q).


% Carmichael's totient function
% a^m congruent to 1 (mod n).
% a is forall int coprime to N

% https://stackoverflow.com/questions/75220820/prolog-unification-doesnt-evaluate-arithmetic-expression
% Prolog does not evaluate arithmetic during unification
% https://stackoverflow.com/questions/43672562/prolog-how-to-evaluate-math-expressions
% As the first post indicate, should use library(clpfd) for finite field over modular, but I'm too tired
% and as existence of m is guanranteed I won't bother with it.
evaluate(X,R):- R is X.

λ(R, 1) :- R is 1, !.
% Somehow evaluating the arithmetic does not halt on 1
% So for special case 1 just bind R with 1 and halt.

λ(R, N) :-
    λ_internal(1, N, M),
    evaluate(M, R). 

λ_internal(M, N, M) :-
    coprime_list(List, N),
    % binded_mod(1, N, B),
    % We are essentially saying a^m mod n = 1
    forall(member(A, List),
	   (
	       (A ** M) mod N =:= 1
	   )), !.
λ_internal(M, N, R) :- λ_internal(M + 1, N, R).
% ChatGPT hinted that a cut is needed so the smallest m is preserved.
% We don't do a halt as existence of M is guanranteed (I won't bother prove that.)
% Smallest since we begin the search from 1

%% Final generation clause
% N = modulus = P * Q
% E = public exponent
% D = private exponent
generate_key(N, E, D, P, Q) :-
    pq_generation(P, Q),
    binded_mult(P, Q, N).
