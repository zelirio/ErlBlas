ErlBlas
=====

A library allowing to perform fast numerical computations in Erlang using the power of CBLAS

# Installation

## rebar3

This library is used as a rebar3 dependency see [the installation](https://rebar3.readme.io/docs/getting-started) and [how to use deependencies](https://rebar3.readme.io/docs/dependencies).

In the rebar.config file add the following line

```erlang
    {deps, [{erlBlas, {git, "https://github.com/zelirio/ErlBlas.git", {branch, "main"}}}]}.
```

## CBLAS

Openblas and lapacke are mandatory to use this library. 

Ubuntu-like os'es:
```sh
    sudo apt-get install libopenblas-dev liblapacke-dev
```

macOS:   
lapacke is included through the accelerate framework; only openblas is required.
```sh
    brew install openblas
```

Windows:   
WSL is prefered when using windows



# Interface

## Matrix creation

Functions that creates new matrices in ErlBlas format

```erlang
% Creates a matrix of size NxM with all elements set to 0.
zeros(N, M)

% Creates a block matrix filled with a diagonal of 1 of size N
eye(N)

% Creates a block matrix from a matrix in Erlang representation
matrix(Mat)

% Creates a new block matrix with the same content as the given one
copy(M)

% Creates a new block matrix with the same dimensions as the given one but filled with 0
copy_shape(M) 

% return the number of lines L and number of columns C of matrix M as a tuple {L, C}
get_shape(M)
```

## Arithmetic operations

Operations that returns a new matrix in single assignment style

```erlang
% Performs the addition of matrices M1 and M2 and return the result in a new matrix
add(M1, M2)

% Performs the subtraction of matrices M1 and M2 and return the result in a new matrix
sub(M1, M2)

% Performs the matrix multiplication of matrices M1 and M2 and return the result in a new matrix
mult(M1, M2)

% returns a new matrix containing the inverse of matrix M1.
% Warning: it only works for blocks smaller than 17*17
inv(M1)

% Multiplies Matrix M by the constant Const and return the result in a new matrix
scal(Const, M)

% Returns the transpose of the given matrix
transpose(M)
```

## Blas interface

Blas operations that performs in place operations

```erlang
% Performs the dgemm operation from CBlas with the given arguments.
% This computes C := Alpha*op( A )*op( B ) + Beta*C where op( X ) = X or op( X ) = X**T depending on the value of ATransp and BTransp.
% The result is written inside the C matrix.
dgemm(ATransp, BTransp, Alpha, A, B, Beta, C)

% Performs the daxpy operation from CBlas with the given arguments.
% This function computes Y := Alpha*X + Y.
% The result is written inside the Y matrix.
daxpy(Alpha, X, Y)

% Performs the dscal operation from CBlas with the given arguments.
% This function multiplies Matrix X by constant Const.
% The result is written inside the X matrix.
dscal(Alpha, X)
```

## Utility functions

```erlang
% Returns the Erlang representation of block matrix M
toErl(M)

% Returns true if matrices M1 and M2 are equals with a 10e-6 precision, false otherwise
equals(M1, M2)
```

## Benchmark functions

```erlang
% Returns maximal size of a block
get_max_length()

% Sets the maximal size of a block to N.
% Doesn't change already existing matrices
set_max_length(N)
```
