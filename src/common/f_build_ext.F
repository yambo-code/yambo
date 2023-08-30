!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine f_build_ext(E,Ef)
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV
 use electrons,  ONLY:levels,spin_occ,n_sp_pol
 use D_lattice,  ONLY:T_elecs,T_holes
 use functions,  ONLY:Fermi_fnc
 !
 implicit none 
 !
 real(SP), intent(in)           :: Ef
 type(levels), intent(inout)    :: E
 !
 ! Work Space
 !
 integer :: i1,i2,is
 !
 !$omp parallel do default(shared), private(i1,i2,is), collapse(3)
 do i1=1,E%nb
   do i2=1,E%nk
     do is=1,n_sp_pol
       if((E%E(i1,i2,is)-Ef)<=0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_holes)
       if((E%E(i1,i2,is)-Ef)> 0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_elecs)
     enddo
   enddo 
 enddo
 !$omp end parallel do
 !
end subroutine
