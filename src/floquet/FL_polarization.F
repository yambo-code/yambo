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
subroutine FL_polarization(E,k,FL_V_bands,FL_Chi_pol)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,bz_map
 use vec_operate,    ONLY:get_id_perpendicular
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_time_S_det,FL_Chi_order
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k
 complex(SP),   intent(in)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 complex(DP),   intent(inout) :: FL_Chi_pol(3,FL_Chi_order+1)  ! In cartesian coordinates
 !
 ! Work Space
 !
 complex(DP)  :: FL_time_P_Berry_RED(3,n_tot_fl_modes)  ! In reduced coordinates
 complex(DP)  :: FL_time_P_Berry_CART(3,n_tot_fl_modes) ! In cartesian coordinates
 complex(DP)  :: FL_Pi_pol(3,n_tot_fl_modes)
 integer      :: ik_bz,is,i_t,i_order,i1_fl
 integer      :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id
 complex(DP)  :: zeta
 real(DP)     :: imag_zeta
 real(SP)     :: time,w0_sm1
 !
 ! Update Floquet Overlaps and determinant
 ! ===================================================================
 !
 FL_S_minus=cZERO
 FL_S_plus =cZERO
 FL_time_S_det =cONE
 !
 do ik_bz=1,QP_nk
   !
   !if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
     !
     call FL_overlaps(E,k,is,ik_bz,FL_V_bands,FL_S_plus(:,:,:,:,ik_bz,is),&
                                           &  FL_S_minus(:,:,:,:,ik_bz,is),&
                                           &  FL_time_S_det(:,:,ik_bz,is),.true.)
     !
   enddo
   !
 enddo
 !
 FL_time_P_Berry_RED  = cZERO
 FL_time_P_Berry_CART = cZERO
 !
 do id_in=1,3
   !
   call get_id_perpendicular(id_in,id2,id3)
   !
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   !
   do i_t=1,n_tot_fl_modes
     !
     do i2=1,k_map%max_kdir(id2)
       do i3=1,k_map%max_kdir(id3)
         !
         zeta=cONE
         !
         idx(id2)=i2
         idx(id3)=i3
         !
         ! Loop on the string
         !
         do i1=1,k_map%max_kdir(id_in)
           !
           idx(id_in)=i1
           ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
           zeta=zeta*FL_time_S_det(i_t,id_in,ikbz,1)
           !
         enddo
         !
         ! Map phases between -pi and pi for all k_perpendicular
         ! Do we miss a factor 2?
         !
         imag_zeta=aimag(log(zeta))
         imag_zeta=imag_zeta-1._DP*pi*nint(imag_zeta/(1._DP*pi))
         !
         FL_time_P_Berry_RED(id_in,i_t)=FL_time_P_Berry_RED(id_in,i_t)+imag_zeta
         !
       enddo
     enddo
     !
     FL_time_P_Berry_RED(id_in,i_t)=-FL_time_P_Berry_RED(id_in,i_t)/real(Nperpend,SP)
     !
   enddo
   !
 enddo
 !
 !
 do id=1,3
   !
   do i_t=1,n_tot_fl_modes
     !
     FL_time_P_Berry_CART(id,i_t)=sum(FL_time_P_Berry_RED(:,i_t)*a(:,id))/DL_vol
     !
   enddo
   !
 enddo
 !
 !aim137 DEBUG
 !
 !print*, real(FL_time_P_Berry_CART(1,:))
 !aim137 DEBUG
 !
 FL_Chi_pol = cZERO
 FL_Pi_pol = cZERO
 !
 do id=1,3
   !
   call FL_FT_polarization(FL_time_P_Berry_CART(id,:),FL_Pi_pol(id,:))
   !
 enddo
 !
 do i_order=0,FL_Chi_order
   !
   i1_fl = i_order + (max_fl_mode+1)
   !
   FL_Chi_pol(:,i_order+1) = FL_Pi_pol(:,i1_fl) * (-1)*(cI*2/Efield(1)%amplitude)**i_order
   !
 enddo
 !
 !aim137 DEBUG
 !
 !
 !
 !aim137 DEBUG
end subroutine
!
!
subroutine FL_FT_polarization(FL_time_pol,FL_floquet_pol)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1
 use fields,         ONLY:Efield
 use wrapper,        ONLY:M_by_V
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_time_S_det
 !
 implicit none
 !
 complex(DP), intent(inout) :: FL_time_pol(n_tot_fl_modes)
 complex(DP), intent(inout) :: FL_floquet_pol(n_tot_fl_modes)
 !
 ! Work Space
 !
 complex(DP) :: FL_FT_Mat(n_tot_fl_modes,n_tot_fl_modes)
 complex(DP) :: tmp_P_t
 complex(SP) :: dummy_det
 integer     :: i_t,i1_fl,i1_shifted_fl
 real(SP)    :: time,w0_sm1
 !
 ! Build Matrix for Fourier Transform
 ! ===================================================================
 !
 FL_FT_Mat = cZERO
 FL_floquet_pol = cZERO
 !
 do i_t=1,n_tot_fl_modes
   !
   w0_sm1 = Efield(1)%frequency(1)*HA2FSm1
   time = 1/(w0_sm1) * (i_t-1)/n_tot_fl_modes ! when i_t=1, time = 0
   !
   do i1_fl=1,n_tot_fl_modes
     !
     i1_shifted_fl = i1_fl -(max_fl_mode+1)
     !
     FL_FT_Mat(i_t,i1_fl) =  exp(-1*cI*i1_shifted_fl*2*pi*(i_t-1)/n_tot_fl_modes)
     !
   enddo
   !
 enddo
 !
 ! Invert Matrix for Fourier Transform
 ! ===================================================================
 !
 call SERIAL_inversion(n_tot_fl_modes,FL_FT_Mat,dummy_det,.false.)
 !
 ! Calculate FL_floquet_pol
 ! ===================================================================
 !
 call M_by_V('N',n_tot_fl_modes,n_tot_fl_modes,cONE,FL_FT_Mat,&
        &   n_tot_fl_modes,FL_time_pol,1,cONE,FL_floquet_pol,1)
 !
 !
!!aim137 DEBUG
!do i_t=1,n_tot_fl_modes
!  tmp_P_t = cZERO
!  do i1_fl=1,n_tot_fl_modes
!    i1_shifted_fl = i1_fl -(max_fl_mode+1)
!    tmp_P_t = tmp_P_t + exp(-1*cI*i1_shifted_fl*2*pi*(i_t-1)/n_tot_fl_modes)&
!                    & * FL_floquet_pol(i1_fl)
!  enddo
!  write(*,'(2(1f22.17))') real(FL_time_pol(i_t)),aimag(FL_time_pol(i_t))
!  write(*,'(2(1f22.17))') real(FL_time_pol(i_t) - tmp_P_t),aimag(FL_time_pol(i_t) - tmp_P_t)
!enddo
!!aim137 DEBUG - works ok
end subroutine FL_FT_polarization
