
#if defined(_CUDA)
#  ifndef __CUDA
#     define __CUDA
#  endif
#endif

#if defined(_OPENACC)
#  ifndef __OPENACC
#     define __OPENACC
#  endif
#endif

#if defined(_OPENMP5)
#  ifndef __OPENMP5
#     define __OPENMP5
#  endif
#endif

#if defined(__CUDA) || defined(__OPENACC) || defined(__OPENMP5)
#  define __HAVE_DEVICE
#  define  _HAVE_DEVICE
#endif

