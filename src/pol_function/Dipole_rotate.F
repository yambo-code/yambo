!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine Dipole_rotate(SC_R,Xen,Xk,X)
 !
 ! This routine update dipoles according to eq. 13
 ! of ref. PRB 59, 7486 (1999)
 ! The intra-band matrix elements of the Kohn-Sham eigenfunctions are
 ! considered zero
 !
 use pars,           ONLY:SP
 use com,            ONLY:warning
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:levels,n_sp_pol
 use SC,             ONLY:SC_bands,H_rotate
 use QP_m,           ONLY:QP_nk
 use X_m,            ONLY:DIP_P,DIP_iR,X_t
 use R_lattice,      ONLY:bz_samp 
 !
 implicit none
 !
 type(levels),  intent(in) :: Xen
 type(bz_samp), intent(in) :: Xk
 complex(SP),   intent(in) :: SC_R(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer  :: ik,id
 !
 if(.not.l_sc_run) &
&   call warning(" Optical matrix elements updated according to the new WF")
 !
 do ik=1,Xen%nk
   !
   do id=1,3
#if defined _SC
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call H_rotate(SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),DIP_P (id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),1)
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
#endif
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call H_rotate(SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),1)
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
   enddo
   !
   call Dipole_dip_dot_field(Xen,Xk,X,X%q0)
   !
   do id=1,3
#if defined _SC
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call H_rotate(SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),DIP_P (id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),-1)
     DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1) =transpose(DIP_P(id,1:SC_bands(2),1:SC_bands(2),ik,1))
#endif
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
     call H_rotate(SC_R(1:SC_bands(2),1:SC_bands(2),ik,1),DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1),SC_bands(2),-1)
     DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1)=transpose(DIP_iR(id,1:SC_bands(2),1:SC_bands(2),ik,1))
   enddo
   !
 enddo
 !
end subroutine Dipole_rotate
