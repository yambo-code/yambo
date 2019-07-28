
#if defined(_CUDA)
#  define __CUDA
#endif

#if defined(__CUDA) || defined(__OPENACC) || defined(__OPENMP5)
#  define __HAVE_DEVICE
#endif

