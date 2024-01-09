!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine OCCUPATIONS_Extend(Xe,E,k)
 !
 ! The occupation of E levels are decided using the Fermi "Levels" of Xe.
 !
 ! The Double Grid levels/occ.s are shifted/defined accordingly.
 ! To check if the Double Grid is present the Xe%FG%E pointer is checked. 
 !
 ! This s.r. takes care that E keeps the same electronic character if the Xe grid.
 ! If this is not possible a warning message is reported. 
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV
 use electrons,  ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,  ONLY:bz_samp
 use D_lattice,  ONLY:T_elecs,T_holes
 use functions,  ONLY:Fermi_fnc
 use interfaces, ONLY:OCCUPATIONS_Gaps
 !
#include<memory.h>
 !
 type(levels)    ::Xe,E
 type(bz_samp)   ::k
 !
 ! Work Space
 !
 real(SP)           :: Ef_test
 real(SP),parameter :: Ef_step=0.1/HA2EV
 !
 if (.not.allocated(E%f)) then
   YAMBO_ALLOC(E%f,(E%nb,E%nk,n_sp_pol))
 endif
 !
 E%E_Fermi=Xe%E_Fermi
 E%E_VBM =Xe%E_VBM
 E%E_CBm =Xe%E_CBm
 call f_build_ext(E,E%E_Fermi)
 call OCCUPATIONS_Gaps(E)
 !
 ! If Xe and E have same metallic character then
 ! simply shift E components (Xe have been already shifted
 ! in the Fermi routine)
 !
 if (all(E%nbf==Xe%nbf).and.all(E%nbm==Xe%nbm)) then
   E%E=E%E-E%E_Fermi
   E%E_VBM =E%E_VBM-E%E_Fermi
   E%E_CBm =E%E_CBm-E%E_Fermi
   !
   ! FineGd
   call OCCUPATIONS_Extend_Double_Grid("E",E,k)
   return
 endif
 !
 ! If Xe and E have not the same metallic character 
 ! find a range for the E_Fermi that keeps the
 ! two grids with the same character
 !
 Ef_test=maxval(Xe%E_VBM(:n_sp_pol))+Ef_step
 do while(Ef_test<minval(Xe%E_CBm(:n_sp_pol)))
   call f_build_ext(E,Ef_test)
   call OCCUPATIONS_Gaps(E)
   if (all(E%nbf==Xe%nbf).and.all(E%nbm==Xe%nbm)) then
     E%E_Fermi=Ef_test
     exit
   endif
   Ef_test=Ef_test+Ef_step
 enddo
 !
 E%E=E%E-E%E_Fermi
 E%E_VBM =E%E_VBM-E%E_Fermi
 E%E_CBm =E%E_CBm-E%E_Fermi
 !
 ! FineGd
 call OCCUPATIONS_Extend_Double_Grid("E",E,k)
 !
 if (any(E%nbf/=Xe%nbf).or.any(E%nbm/=Xe%nbm)) then
   call warning('Global and response function energy levels have different metallic character')
 endif
 !
end subroutine
