!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!                  
subroutine INTERPOLATION_nearest_E(Energy,qp,E_diff_tresh,i_qp_ref,i_sp_pol)
 !
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_t
 use vec_operate,    ONLY:sort
 use electrons,      ONLY:spin
 !
 implicit none
 !
 real(SP),           intent(in) :: Energy
 type(QP_t),         intent(in) :: qp
 integer,            intent(in) :: i_sp_pol
 real(SP),           intent(out):: E_diff_tresh
 integer,            intent(out):: i_qp_ref
 !
 ! Work Space
 !
 integer  :: E_indx(qp%n_states),i_qp
 real(SP) :: E_tmp(qp%n_states),E_diff
 !
 E_tmp=qp%E_bare
 !
 call sort(arrin=E_tmp,indx=E_indx)
 !
 E_diff_tresh=1000._SP
 !
 do i_qp=1,qp%n_states
   !
   if (spin(qp%table(E_indx(i_qp),:))/=i_sp_pol) cycle
   !
   E_diff=abs( Energy-E_tmp(i_qp) )
   !
   if (E_diff<E_diff_tresh) then
     i_qp_ref=E_indx(i_qp)
     E_diff_tresh=E_diff
   endif
   !
 enddo
 !
end subroutine INTERPOLATION_nearest_E
