!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine X_ALLOC_elemental(WHAT,DIM)
 !
 use pars,          ONLY:rZERO,cZERO
 use X_m,           ONLY:Resp_ii,Resp_ij,Epsilon_ii,Joint_DOS,BS_E_sorted,BS_E_sorted_indx, &
&                        Epsilon_ij,X_magnons,X_dichroism,                  &
&                        X_fxc,EEL,Alpha,X_mat,X_drude_term,X_RLcomp_ig
 use PHOTOLUM,      ONLY:PL
#ifdef _CUDA
 use X_m,           ONLY:X_mat_d
#endif
 use stderr,        ONLY:STRING_match
 !
#include<memory.h>
 !
 character(*)      :: WHAT
 integer, optional :: DIM(:)
 !
 if (present(DIM)) then
   select case(what)
   case('RESP')
     if (allocated(Resp_ii)) return
     YAMBO_ALLOC(Resp_ii,(DIM(1),DIM(2))) !2(int)3(nonint)4(n-1 Haydock/inv shifted)
     Resp_ii   = cZERO
     YAMBO_ALLOC(X_drude_term,(DIM(1)))
     X_drude_term = cZERO
   case('EPS')
     if (allocated(Epsilon_ii)) return
     YAMBO_ALLOC(Epsilon_ii,(DIM(1),DIM(2))) !2(int)3(nonint)4(n-1 Haydock/inv shifted)
     Epsilon_ii   = cZERO
   case('OFF')
     if (allocated(Resp_ij)) return
     YAMBO_ALLOC(Resp_ij,(DIM(1),DIM(2)))
     Resp_ij=cZERO
     YAMBO_ALLOC(Epsilon_ij,(DIM(1),DIM(2)))
     Epsilon_ij=cZERO
   case('MAGNONS')
     YAMBO_ALLOC(X_magnons,(DIM(1),DIM(2),DIM(3)))
     X_magnons=cZERO
   case('DICH')
     YAMBO_ALLOC(X_dichroism,(DIM(1),DIM(2)))
     X_dichroism=cZERO
   case('EEL')
     YAMBO_ALLOC(EEL,(DIM(1),DIM(2)))
     EEL=cZERO
   case('ALPHA')
     if (allocated(Alpha)) return
     YAMBO_ALLOC(Alpha,(DIM(1),DIM(2)))
     Alpha=cZERO
   case('JDOS')
     YAMBO_ALLOC(Joint_DOS,(DIM(1),DIM(2)))
     Joint_DOS=cZERO
   case('BS_E_SORTED')
     YAMBO_ALLOC(BS_E_sorted,(DIM(1),DIM(2),DIM(3)))
     BS_E_sorted=cZERO
     YAMBO_ALLOC(BS_E_sorted_indx,(DIM(1),2*DIM(2),5))
     BS_E_sorted_indx=0
   case('FXC')
     YAMBO_ALLOC(X_fxc,(DIM(1)))
     X_fxc=cZERO
   case('PL')
     YAMBO_ALLOC(PL,(DIM(1),DIM(2))) !2(int)3(nonint)
     PL = rZERO
   case('X')
     if (allocated(X_mat)) return
     YAMBO_ALLOC(X_mat,(DIM(1),DIM(2),DIM(3)))
     X_mat=cZERO
#ifdef _CUDA
     allocate(X_mat_d, source=X_mat)
#endif
   end select
   return
 endif
 !
 if (STRING_match(WHAT,"RESP").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(Resp_ii)
 endif
 if (STRING_match(WHAT,"EPS").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(Epsilon_ii)
   YAMBO_FREE(X_drude_term)
 endif
 if (STRING_match(WHAT,"OFF").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(Resp_ij)
   YAMBO_FREE(Epsilon_ij)
 endif
 if (STRING_match(WHAT,"MAGNONS").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(X_magnons)
 endif
 if (STRING_match(WHAT,"DICH").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(X_dichroism)
 endif
 if (STRING_match(WHAT,"EEL").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(EEL)
 endif
 if (STRING_match(WHAT,"ALPHA").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(Alpha)
 endif
 if (STRING_match(WHAT,"JDOS").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(Joint_DOS)
 endif
 if (STRING_match(WHAT,"BS_E_SORTED").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(BS_E_sorted)
   YAMBO_FREE(BS_E_sorted_indx)
 endif
 if (STRING_match(WHAT,"FXC").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(X_fxc)
 endif
 if (STRING_match(WHAT,"PL").or.STRING_match(WHAT,"ALL")) then
   YAMBO_FREE(PL)
 endif
 if (STRING_match(WHAT,"X").or.STRING_match(WHAT,"ALL")) then
   !
   ! I clean the RL components mapping
   YAMBO_FREE(X_RLcomp_ig)
   !
   ! and the matrix itself
   YAMBO_FREE(X_mat)
#ifdef _CUDA
   YAMBO_FREE(X_mat_d)
#endif
   !
 endif
 !
end subroutine
