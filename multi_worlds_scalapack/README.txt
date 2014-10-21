
Usage:

mpiexec -np 12 ./mytest.x -n 9 -ng 2 -np 3

-n,  -ndim       dimension of the matrix to be diagonalized
-ng, -ngrid      dimension fo the suqare scalapack grid to be used ( 2 x 2 above)
-np, -npool      number of different pools, each of then building a scalapck grid

