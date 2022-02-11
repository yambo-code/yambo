!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_polarization(E,k,FL_V_bands,FL_pol)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,bz_map
 use vec_operate,    ONLY:get_id_perpendicular
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_S_det
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k
 complex(SP),   intent(in)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 complex(SP),   intent(out) :: FL_pol(3)  ! In reduced coordinates
 !
 ! Work Space
 !
 complex(SP) :: FL_P_Berry_RED(3)  ! In reduced coordinates
 integer :: ik_bz,is
 integer       :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id
 complex(DP)   :: zeta
 real(DP)      :: imag_zeta
 !
 ! Update Floquet Overlaps and determinant
 ! ===================================================================
 !
 FL_S_minus=cZERO
 FL_S_plus =cZERO
 FL_S_det  =cZERO
 !
 do ik_bz=1,QP_nk
   !
   !if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
     !
     call FL_overlaps(E,k,is,ik_bz,FL_V_bands,FL_S_plus(:,:,:,:,ik_bz,is),&
                                           &  FL_S_minus(:,:,:,:,ik_bz,is),&
                                           &  FL_S_det(:,:,ik_bz,is))
     !
   enddo
   !
 enddo
 !
 FL_P_Berry_RED = cZERO
 !
!do id_in=1,3
!  !
!  call get_id_perpendicular(id_in,id2,id3)
!  !
!  Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
!  !
!  do i2=1,k_map%max_kdir(id2)
!    do i3=1,k_map%max_kdir(id3)
!      !
!      zeta=cONE
!      !
!      idx(id2)=i2
!      idx(id3)=i3
!      !
!      ! Loop on the string
!      !
!      do i1=1,k_map%max_kdir(id_in)
!        !
!        idx(id_in)=i1
!        ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
!        zeta=zeta*All_S_det(id_in,ikbz,1)
!        !
!      enddo
!      !
!      ! Map phases between -pi and pi for all k_perpendicular
!      ! Do we miss a factor 2?
!      !
!      imag_zeta=aimag(log(zeta))
!      imag_zeta=imag_zeta-1._DP*pi*nint(imag_zeta/(1._DP*pi))
!      !
!      FL_P_Berry_RED(id_in)=FL_P_Berry_RED(id_in)+imag_zeta
!      !
!    enddo
!  enddo
!  !
!  FL_P_Berry_RED(id_in)=-FL_P_Berry_RED(id_in)/real(Nperpend,SP)
!  !
!enddo
!!
!!
!do id=1,3
!  FL_pol(id)=sum(FL_P_Berry_RED(:)*a(:,id))/DL_vol
!enddo
 !
 !
end subroutine
