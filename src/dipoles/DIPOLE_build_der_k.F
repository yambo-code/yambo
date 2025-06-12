!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA, DS
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
subroutine DIPOLE_build_der_k(Xen,Xk,Dip,direction_is_todo)
 !
 ! Build the dipole as k-derivative using
 ! the overlaps to construct local rotation matrices 
 ! see Phys. Rev. B 76, 035213 (2007) and  Phys. Rev. Research 2, 013357 (2020)
 !
 use pars,           ONLY:SP,cI,cZERO,pi,cONE
 use units,          ONLY:HA2EV
 use LIVE_t,         ONLY:live_timing
 use electrons,      ONLY:levels,n_sp_pol
 use DIPOLES,        ONLY:DIPOLE_t,DIP_S,DIP_iR,DIP_v,l_force_SndOrd,&
                          der_k_obs,x_space_obs
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx,degeneration_finder
 use matrix_operate, ONLY:hermitian
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                         PAR_DIPk_nbz,PAR_IND_CON_BANDS_DIP, PAR_IND_VAL_BANDS_DIP, &
&                         PAR_IND_DIPk_bz,PAR_DIPk_bz_index,PAR_IND_DIPk_bz_ID,PAR_COM_DIPk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use com,            ONLY:msg
 use D_lattice,      ONLY:a
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
 use stderr,         ONLY:STRING_match
 !
#include <y_memory.h>

 type(levels),           intent(in)     :: Xen
 type(bz_samp),          intent(inout)  :: Xk
 type(DIPOLE_t),         intent(inout)  :: Dip
 logical,                intent(out)    :: direction_is_todo(3)
 !
 ! Work Space
 !
 integer     :: ik,i_sp_pol,id_red,idx(3),idir,ic,ik_mem,ikbz,iv
 integer     :: istep,max_step,ikm1,ikp1
 complex(SP), allocatable :: U_l(:,:,:),U_r(:,:,:),Der_k_red(:,:,:),Der_k_cart(:,:,:),delta(:,:)
 !
 logical     :: deg_table_k(Dip%ib(2),Dip%ib(2)),deg_table_kp(Dip%ib(2),Dip%ib(2)),deg_table_km(Dip%ib(2),Dip%ib(2))
 integer     :: ib,ibp,i_grp,first_el_k(Dip%ib(2)),n_of_el_k(Dip%ib(2)),n_deg_grp_k
 real(SP)    :: Ev_m_Ec_KS,deg_thresh
 character(1)::str(3)=(/'x','y','z'/)
 !
 call timing("DIPOLE_buil_derk",OPR="start")
 !
 if(.not.Dip%force_v_g_space) Dip%Vnl_included=.true.
 !
 if(.not.allocated(k_map%k_map_dir)) call k_map_nearest_by_dir(Xk,k_map)
 if(.not.allocated(Xk%k_table))      call k_build_up_BZ_tables(Xk)
 !
 direction_is_todo(:)= (k_map%max_kdir(:)==1)
 !
 if(any(direction_is_todo)) call warning(' Der-k Dipoles not in 3d. Assuming non periodic system.')
 !
 if(any(k_map%max_kdir(:)>=6.and..not.l_force_SndOrd)) &
 &   call msg('rs','Using forth order approximation for der-k dipoles')
 !
 ! Notice that DIP_iR/DIP_v are defined only in the IBZ
 ! while the overlaps are in the BZ
 !
 call live_timing("Build Rotation Matrices:",PAR_IND_DIPk_bz%n_of_elements(PAR_IND_DIPk_bz_ID+1)*n_sp_pol)
 !
 YAMBO_ALLOC(U_l,(Dip%ib(2),Dip%ib(2),3))
 YAMBO_ALLOC(U_r,(Dip%ib(2),Dip%ib(2),3))
 YAMBO_ALLOC(Der_k_cart,(Dip%ib(2),Dip%ib(2),3))
 YAMBO_ALLOC(Der_k_red,(Dip%ib(2),Dip%ib(2),2))
 YAMBO_ALLOC(delta,(Dip%ib(2),Dip%ib(2)))
 !
 delta=cZERO
 do ib=1,Dip%ib(2)
   delta(ib,ib)=cONE
 enddo
 !
 Dip%Energy_threshold=max(Dip%Energy_threshold,0.001/HA2EV)
 deg_thresh=Dip%Energy_threshold
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz 
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     ikbz=Xk%k_table(ik,1)
     !
     call degeneration_finder(Xen%E(1:Dip%ib(2),ik,i_sp_pol),Dip%ib(2),&
     &                           first_el_k,n_of_el_k,n_deg_grp_k,deg_thresh,Include_single_values=.true.)
     !
     deg_table_k=.false.
     do i_grp=1,n_deg_grp_k
       do ib=first_el_k(i_grp),first_el_k(i_grp)+n_of_el_k(i_grp)-1
         do ibp=first_el_k(i_grp),first_el_k(i_grp)+n_of_el_k(i_grp)-1
           deg_table_k(ib,ibp)=.true.
           deg_table_k(ibp,ib)=.true.
         enddo
       enddo
     enddo
     !
     U_l       =cZERO
     U_r       =cZERO
     !
     Der_k_cart=cZERO
     Der_k_red =cZERO
     !
     do id_red=1,3  ! Loop on the three crystal directions in the BZ
       !
       if(k_map%max_kdir(id_red)==1) cycle ! Non-periodic direction, skip calculation
       !
       max_step=1 ! Only the first neighbor
       if(k_map%max_kdir(id_red)>=6.and..not.l_force_SndOrd) max_step=2
       !
       do istep=1,max_step
         ! 
         idx=k_map%k_map_inv(ikbz,:)
         idx(id_red)=idx(id_red)+istep
         idx=k_periodic_idx(idx,k_map)
         ikp1=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         idx=k_map%k_map_inv(ikbz,:)
         idx(id_red)=idx(id_red)-istep
         idx=k_periodic_idx(idx,k_map)
         ikm1=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         U_r(:,:,1)=          DIP_S(:,:,id_red+(istep-1)*3,ikbz,i_sp_pol)
         call zeroing_disconnected_blocks(Xen,Xk,(/1,Dip%ib(2)/),ikp1,i_sp_pol,&
         &                         deg_table_k,deg_thresh,U_r(:,:,1),U_r(:,:,2))
         U_r(:,:,3)=U_r(:,:,2)
         call SERIAL_SVD(Dip%ib(2),U_r(:,:,3),'uni',0)
         !
         U_l(:,:,1)=hermitian(DIP_S(:,:,id_red+(istep-1)*3,ikm1,i_sp_pol))
         call zeroing_disconnected_blocks(Xen,Xk,(/1,Dip%ib(2)/),ikm1,i_sp_pol,&
         &                         deg_table_k,deg_thresh,U_l(:,:,1),U_l(:,:,2))
         U_l(:,:,3)=U_l(:,:,2)
         call SERIAL_SVD(Dip%ib(2),U_l(:,:,3),'uni',0)
         !
	 ! Yambo version
         Der_k_red(:,:,istep)=+matmul(U_r(:,:,1),hermitian(U_r(:,:,3)) )  &
         &                    -matmul(U_l(:,:,1),hermitian(U_l(:,:,3)) )
	 ! Virk-Sipe version
         !Der_k_red(:,:,istep)=+matmul((U_r(:,:,1)-U_r(:,:,2)),hermitian(U_r(:,:,3)) )  &
         !&                    -matmul((U_l(:,:,1)-U_l(:,:,2)),hermitian(U_l(:,:,3)) )
         !
	 ! This contains the i/2q factor 
	 ! 1/q=k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
         Der_k_red(:,:,istep)=Der_k_red(:,:,istep)*cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))/2._SP
         !
       enddo
       !
       ! Transform in cartesian coordinates 
       !
       if(max_step==1) then
         !      
         ! First order formula D(dk)      
         !
         do idir=1,3
           Der_k_cart(:,:,idir)=Der_k_cart(:,:,idir)+transpose(Der_k_red(:,:,1))*a(id_red,idir)
         enddo
         !
       else
         !      
         ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
         !
         do idir=1,3
           Der_k_cart(:,:,idir)=Der_k_cart(:,:,idir)+transpose(4._SP*Der_k_red(:,:,1)-Der_k_red(:,:,2))/3._SP*a(id_red,idir)
         enddo
         !
       endif
       !
     enddo  ! id_red loop on crystal directions 
     !
     !======
     ! <iR>
     !======
     do iv=Dip%ib(1),Dip%ib_lim(1)
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         do idir=1,3
           DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)=(Der_k_cart(ic,iv,idir)+conjg(Der_k_cart(iv,ic,idir)))*cI
         enddo
       enddo
     enddo
     !
     !=====
     ! <v>
     !=====
     if(STRING_match(der_k_obs,"V")) then
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
         do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
           if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
           Ev_m_Ec_KS   =Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
           if (allocated(Xen%Eo)) Ev_m_Ec_KS   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
           do idir=1,3
             DIP_v(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec_KS
           enddo
           !
         enddo
       enddo
       !
     endif
     !
     call live_timing(steps=1)
     !
   enddo ! ik iiBZ loop
   !
 enddo ! i_sp_pol
 !
 call live_timing()
 !
 YAMBO_FREE(U_l)
 YAMBO_FREE(U_r)
 YAMBO_FREE(Der_k_red)
 YAMBO_FREE(Der_k_cart)
 YAMBO_FREE(delta)
 !
 ! Build non-periodic directions if necessary
 ! 
 if (any(direction_is_todo)) then
   do ic=1,3
     if (direction_is_todo(ic)) then
       x_space_obs=trim(x_space_obs)//" R_"//str(ic)
       if (STRING_match(der_k_obs,"V")) x_space_obs=trim(x_space_obs)//" V_"//str(ic)
     endif
   enddo 
   Dip%computed=trim(Dip%computed)//trim(x_space_obs)//" [X-space]"
 endif
 !
 call timing("DIPOLE_buil_derk",OPR="stop")
 return
 !
end subroutine DIPOLE_build_der_k
!
!
subroutine zeroing_disconnected_blocks(Xen,Xk,nb,ikpbz,i_sp_pol,deg_table_k,deg_thresh,U_in,U_out)
 !
 use pars,           ONLY:SP,cZERO
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:degeneration_finder
 !
 implicit none
 !
 type(levels),   intent(in) :: Xen
 type(bz_samp),  intent(in) :: Xk
 !
 integer, intent(in) :: nb(2),ikpbz,i_sp_pol
 logical, intent(in) :: deg_table_k(nb(1):nb(2),nb(1):nb(2))
 real(SP),intent(in) :: deg_thresh
 !
 complex(SP), intent(in)  :: U_in(nb(1):nb(2),nb(1):nb(2))
 complex(SP), intent(out) :: U_out(nb(1):nb(2),nb(1):nb(2))
 !
 ! Workspace
 !
 integer     :: ib,ibp,i_grp,first_el_k(nb(2)-nb(1)+1),n_of_el_k(nb(2)-nb(1)+1),n_deg_grp_k
 logical     :: deg_table_kp(nb(1):nb(2),nb(1):nb(2))
 !
 call degeneration_finder(Xen%E(nb(1):nb(2),Xk%sstar(ikpbz,1),i_sp_pol),nb(2)-nb(1)+1,&
 &                        first_el_k,n_of_el_k,n_deg_grp_k,deg_thresh,Include_single_values=.true.)
 !
 deg_table_kp=.false.
 do i_grp=1,n_deg_grp_k
   do ib=nb(1)+first_el_k(i_grp)-1,nb(1)+first_el_k(i_grp)+n_of_el_k(i_grp)-2
     do ibp=nb(1)+first_el_k(i_grp)-1,nb(1)+first_el_k(i_grp)+n_of_el_k(i_grp)-2
       deg_table_kp(ib,ibp)=.true.
       deg_table_kp(ibp,ib)=.true.
     enddo
   enddo
 enddo
 !
 U_out=U_in
 !
 do ib=nb(1),nb(2)
   do ibp=nb(1),nb(2)
     if(deg_table_k(ibp,ib).or.deg_table_kp(ibp,ib)) cycle
     U_out(ib,ibp)=cZERO
   enddo
 enddo
 !
end subroutine
