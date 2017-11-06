#ifndef __IOTK_SPECIALS_H
#define __IOTK_SPECIALS_H

! Some definitions from QE
! to enhance the portability
!
! some compilers do not like the following
!    #define __IOTK_REAL1 selected_real_kind(6,30)
!    #define __IOTK_REAL2 selected_real_kind(14,200)
! so we use explicit kinds
#ifdef __IOTK_REAL1
#   undef __IOTK_REAL1
#endif
#ifdef __IOTK_REAL2
#   undef __IOTK_REAL2
#endif
!
#if defined(__NAG)
#   define __IOTK_REAL1 1
#   define __IOTK_REAL2 2
#elif defined(__SX6)
#   define __IOTK_REAL2 8
#else
#   define __IOTK_REAL1 4
#   define __IOTK_REAL2 8
#endif
! Machine-dependent options
! Only for compilers that require some special tricks

! End of QE definitions

#endif
