!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine Dipole_SC_rotate(H_rotation,Xen,X,rot_direction)
 !
 ! This routine update dipoles according to eq. 13
 ! of ref. PRB 59, 7486 (1999)
 ! The intra-band matrix elements of the Kohn-Sham eigenfunctions are
 ! considered zero
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:levels,n_sp_pol
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use DIPOLES,        ONLY:DIP_P,DIP_iR
 use X_m,            ONLY:X_t
 !
 implicit none
 !
 type(levels),  intent(in) :: Xen
 complex(SP),   intent(in) :: H_rotation(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
 type(X_t),  intent(inout) :: X
 !
 integer,       intent(in) :: rot_direction
 !
 ! Work Space
 !
 integer  :: ik,id
 !
 if(.not.l_sc_run) call warning(" Optical matrix elements updated with WFs rotation. Neglecting diagonal terms of DIP_iR")
 !
 do ik=1,Xen%nk
   !
   do id=1,3
     !
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call OBS_rotate(H_rotation(1:SC_bands(2),1:SC_bands(2),ik,1),&
&                  DIP_P (id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),rot_direction)
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     !
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call OBS_rotate(H_rotation(1:SC_bands(2),1:SC_bands(2),ik,1),&
&                  DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),rot_direction)
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     !
   enddo
   !
 enddo
 !
end subroutine DIPOLE_SC_rotate
