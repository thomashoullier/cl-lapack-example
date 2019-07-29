# Calling LAPACK/BLAS in SBCL
We provide an example of calling the foreign LAPACK/BLAS library in SBCL.
All credit for this example goes to this SO poster [1] and the source he cites
(which I was not able to exactly pinpoint).
This example is about the matrix multiplication `dgemm()` [2].

## Pre-requisites
You need to install LAPACK of course. There is a `lapack` package in the Void
Linux repository:

```shell
sudo xbps-install -S lapack
```

There should be a similar package for the distribution you use.

## References
1. https://stackoverflow.com/questions/29822509/matrix-multiplication-using-blas-from-common-lisp
1. http://www.netlib.org/lapack/explore-html/d1/d54/group__double__blas__level3_gaeda3cbd99c8fb834a60a6412878226e1.html#gaeda3cbd99c8fb834a60a6412878226e1
