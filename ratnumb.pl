% 
% A program that can add, subtract, multiple and divide rational numbers.
% Additionally, an exponent can be applied to a rational number.
%
% Joshua Boyd <boyd_joshua@columbusstate.edu>
% CPSC5135G
%

% empty list, print 0
rnprint([]) :-
    writeln('0').

% numerator is 0, print 0
rnprint([X|R]) :-
    L = [X|R],
    numer(L, N),
    denom(L, D),
    N == 0,
    rnprint([]).

% not a mixed number, numerator less than denominator, print fraction
rnprint([X|R]) :-
    L = [X|R],
    numer(L, N),
    denom(L, D),
    abs(N) < D,
    format('~d/~d', [N, D]).

% numerator divides cleanly by denominator, print whole number 
rnprint([X|R]) :-
    L = [X|R],
    numer(L, N),
    denom(L, D),
    0 is mod(N, D),
    format('~d', [N/D]).

% mixed number, print
rnprint([X|R]) :-
    L = [X|R],
    numer(L, N),
    denom(L, D),
    makern(mod(abs(N), D), D, Simplify),
    numer(Simplify, N2),
    denom(Simplify, D2),
    format('~d and ~d/~d', [truncate(N/D), N2, D2]).

% empty list
numer([], Numer) :-
    Numer is 0.

% number only
numer([X|Y], Numer) :-
    number(X),
    not(compound(Y)),
    Numer is X.

% number and fraction
numer([X|Y], Numer) :-
    compound(Y),
    flatten(Y,Fraction),
    nth0(1, Fraction, Denom),
    nth0(0, Fraction, FractionalNumerator),
    numer(X, FractionalNumerator, Denom, Numer).

% fraction only
numer([X|Y], Numer) :-
    compound(X),
    nth0(0, X, Numer).
    
% calculate numerator for positive number
numer(Number, FractionNumer, Denom, Result) :-
    Number >= 0,
    Result is Number * Denom + FractionNumer. 

% calculate numerator for negative number
numer(Number, FractionNumer, Denom, Result) :-
    Number < 0,
    Result is -1 * (abs(Number) * Denom + FractionNumer).

% empty list
denom([], Denom) :-
    Denom is 1.

% number only
denom([X|Y], Denom) :-
    number(X),
    not(compound(Y)),
    Denom is 1.

% number and fraction
denom([X|Y], Denom) :-
    compound(Y),
    flatten(Y,Fraction),
    nth0(1, Fraction, Denom).

% fraction only
denom([X|Y], Denom) :-
    compound(X),
    nth0(1, X, Denom).

% takes a numerator and denominator and assigns a simplified fraction
makern(N,D,Result) :-
    Numerator is N / gcd(N,D),
    Denominator is D / gcd(N,D),
    Result = [[Numerator, Denominator]].

% takes two rational numbers as arguments and assigns their sum
rnadd(X,Y,Result) :-
    numer(X, NX),
    numer(Y, NY),
    denom(X, DX),
    denom(Y, DY),
    makern(
        NX * DY + NY * DX,
        DX * DY, Result).

% takes two rational numbers as arguments and assigns the result of
% subtracting the second argument from the first argument
rnsub(X,Y,Result) :-
    numer(X, NX),
    numer(Y, NY),
    denom(X, DX),
    denom(Y, DY),
    makern(
        NX * DY - NY * DX,
        DX * DY, Result).

% takes two rational numbers as arguments and assigns their product
rnprod(X,Y,Result) :-
    numer(X, NX),
    numer(Y, NY),
    denom(X, DX),
    denom(Y, DY),
    makern(
        NX * NY,
        DX * DY, Result).

% takes two rational numbers as arguments and assigns the result of
% dividing the first argument by the second argument
rndiv(X,Y,Result) :-
    numer(X, NX),
    numer(Y, NY),
    denom(X, DX),
    denom(Y, DY),
    makern(
        NX * DY,
        DX * NY, Result).

% takes a rational number and an integer as arguments and assigns 
% the rational number to the power of the second argument
% handles positive exponents
rnexp(X,Y,Result) :-
    sign(Y) >= 0,
    numer(X, NX),
    denom(X, DX),
    makern(
        NX^Y,
        DX^Y,
        Result).

% takes a rational number and an integer as arguments and assigns 
% the rational number to the power of the second argument
% handles negative exponents
rnexp(X,Y,Result) :-
    sign(Y) < 0,
    numer(X, NX),
    denom(X, DX),
    D is DX^(-1 * Y),
    N is NX^(-1 * Y),
    N2 is N * sign(N),
    D2 is D * sign(N),
    makern(
        D2,
        N2,
        Result).
