!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
subroutine excitons_find_degeneracies(BS_E,BS_E_degs,n_eig)
 !
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use vec_operate,   ONLY:sort
 use YPPm,          ONLY:deg_energy
 !
 implicit none
 !
 integer, intent(in) :: n_eig
 complex(SP)   :: BS_E(n_eig)
 integer       :: BS_E_degs(n_eig)
 !
 ! Work space
 !
 integer :: j1,j2
 integer :: S_indx(n_eig)
 real(SP):: v2sort(n_eig)
 !
 call msg('s',' Sorting energies')
 !==================================
 !
 v2sort=real(BS_E)
 call sort(arrin=v2sort,indx=S_indx)
 !
 BS_E_degs=0
 !
 do j1=1,n_eig
   !
   if (BS_E_degs(S_indx(j1))>0) cycle
   !
   BS_E_degs(S_indx(j1))=S_indx(j1)
   !
   do j2=j1+1,n_eig
     if ( abs( real(BS_E(S_indx(j1)))-real(BS_E(S_indx(j2))) )>deg_energy) exit
     BS_E_degs(S_indx(j2))=S_indx(j1)
   enddo
   !
 enddo
 !
end subroutine excitons_find_degeneracies
