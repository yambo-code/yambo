!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
subroutine DIPOLE_overlaps(Xk,Dip)
 !    
 use pars,              ONLY:SP,cZERO,pi
 use LIVE_t,            ONLY:live_timing
 use R_lattice,         ONLY:k_map,bz_samp,WF_shifts,G_m_G
 use wave_func,         ONLY:WF
 use vec_operate,       ONLY:k_periodic_idx
 use DIPOLES,           ONLY:DIPOLE_t,DIP_S
 use electrons,         ONLY:n_spinor,n_sp_pol
 use wrapper,           ONLY:Vstar_dot_V
 use wave_func,         ONLY:wf_ng_1st_BZ,wf_ng_overlaps,wf_ng
 use parallel_m,        ONLY:PAR_IND_DIPk_bz,PAR_IND_DIPk_bz_ID,PAR_COM_DIPk_ibz_A2A, &
&                            PAR_IND_CON_BANDS_OVLP,PAR_IND_VAL_BANDS_OVLP,           &
&                            PAR_IND_OVLPk_ibz,PAR_COM_DIPk_ibz_INDEX
 use parallel_int,      ONLY:PP_redux_wait,PARALLEL_WF_index,PARALLEL_WF_distribute
 use interfaces,        ONLY:WF_shift_kpoint,WF_symm_kpoint,eval_G_minus_G,WF_load,WF_free
 use timing_m,          ONLY:timing
 !
#include<dev_defs.h>
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk 
 type(DIPOLE_t),intent(in)    :: Dip
 !
 ! Work Space
 !
 integer           :: ikbz,ikbzp,is,isp,ib,ibp,i_sp_pol,ik,ikp,istep,max_step
 integer           :: iG0,ng_tmp,wf_ng_save,wf_ng_1st_BZ_save
 integer           :: id,idx_kp(3),idx_k(3),shift(3),g0_idx(3,2)
 real(SP)          :: g0_length(3)
 complex(SP), allocatable :: WF_symm(:,:,:,:),WF_shifted(:,:,:,:),WF_ik(:,:,:,:),WF_ikp(:,:,:,:)
#ifdef _CUDA
 attributes(device):: WF_symm,WF_shifted,WF_ik,WF_ikp
#endif
 logical           :: USE_shifted_wf
 !
 call timing("DIPOLE_overlaps",OPR="start")
 !
 call k_find_smallest_g0(g0_idx,g0_length)
 iG0=max(maxval(g0_idx(:,1)),maxval(g0_idx(:,2)))
 !
 ! Generate G_m_G of the needed size
 !
 YAMBO_FREE(G_m_G)
 ng_tmp=eval_G_minus_G(wf_ng_1st_BZ,iG0)
 !
 wf_ng_save       =wf_ng
 wf_ng_1st_BZ_save=wf_ng_1st_BZ
 !
 if (maxval(G_m_G(:,1)) < wf_ng_1st_BZ) then
   !
   call warning(" Forced lowering on cutoff on WFs")
   wf_ng_1st_BZ  =maxval(G_m_G(:,1))
   wf_ng         =maxval(G_m_G(:,1))
   !
 endif
 !
 wf_ng_overlaps=maxval(G_m_G)
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_OVLPk_ibz,CLEAN_UP=.TRUE.)
 !
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,0,1,(/1,Dip%ib(2)/),(/1,Xk%nibz/),space='G',title='-Oscillators/G space/Overlaps')
 !
 YAMBO_ALLOC(WF_symm,(wf_ng_1st_BZ,n_spinor,Dip%ib(2),n_sp_pol))
 YAMBO_ALLOC(WF_shifted,(wf_ng_overlaps,n_spinor,Dip%ib(2),n_sp_pol))
 YAMBO_ALLOC(WF_ik,     (wf_ng_overlaps,n_spinor,Dip%ib(2),n_sp_pol))
 YAMBO_ALLOC(WF_ikp,    (wf_ng_overlaps,n_spinor,Dip%ib(2),n_sp_pol))
 !
 DIP_S=cZERO
 !
 call live_timing("Overlaps:",PAR_IND_DIPk_bz%n_of_elements(PAR_IND_DIPk_bz_ID+1)*n_sp_pol)
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ikbz=1,Xk%nbz
     !
     if(.not.PAR_IND_DIPk_bz%element_1D(ikbz)) cycle
     !
     idx_k=k_map%k_map_inv(ikbz,:)
     !
     ik = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
     !
     call WF_symm_kpoint((/1,Dip%ib(2)/),ikbz,Xk,WF_symm)
     !  
     call WF_shift_kpoint((/1,Dip%ib(2)/),ikbz,WF_shifts(ikbz,:),Xk,WF_symm,WF_ik)
     !
     do id=1,3  ! Loop on the three crystal directions in the BZ
       !
       if(k_map%max_kdir(id)==1) cycle ! Non-periodic direction, skip calculation
       !
       max_step=1 ! Only the first neighbor
       if(k_map%max_kdir(id)>=6) max_step=2  ! First and second neighbors
       !
       do istep=1,max_step
         !
         idx_kp=idx_k
         idx_kp(id)=idx_kp(id)+istep   ! Neighbor on the right
         !
         USE_shifted_wf=.false.
         if(idx_kp(id)>k_map%max_kdir(id)) USE_shifted_wf=.true. ! Check if the neighbor is out of the BZ
         !
         idx_kp=k_periodic_idx(idx_kp,k_map)  ! Bring it back by periodicity
         ikbzp=k_map%k_map_dir(idx_kp(1),idx_kp(2),idx_kp(3))  ! Find the corresponding index in the BZ
         !
         ! ! Find the index in the IBZ and the symmetry
         ikp = Xk%sstar(ikbzp,1)
         isp = Xk%sstar(ikbzp,2)
         !
         call WF_symm_kpoint((/1,Dip%ib(2)/),ikbzp,Xk,WF_symm)
         !
         ! Shift the wave-function by a G-vector if the neighbor is out of the BZ (USE_shifed_wf=.true.)
         shift=WF_shifts(ikbzp,:)
         if(USE_shifted_wf) then
           if(WF_shifts(ikbzp,id)==1) shift(id)=1
           if(WF_shifts(ikbzp,id)==0) shift(id)=1
           if(WF_shifts(ikbzp,id)==2) shift(id)=0
         endif
         call WF_shift_kpoint((/1,Dip%ib(2)/),ikbzp,shift,Xk,WF_symm,WF_ikp)
         !
         do ib=1,Dip%ib(2)
           if(.not.PAR_IND_VAL_BANDS_OVLP%element_1D(ib)) cycle
           do ibp=1,Dip%ib(2)
             if(.not.PAR_IND_CON_BANDS_OVLP%element_1D(ibp)) cycle
             DIP_S(ib,ibp,id+(istep-1)*3,ikbz,i_sp_pol)= &
                       &  Vstar_dot_V(wf_ng_overlaps*n_spinor,WF_ik(:,:,ib,i_sp_pol),WF_ikp(:,:,ibp,i_sp_pol)) 
           enddo ! ibp
         enddo ! ib
         !
       enddo ! istep
     enddo ! idir
     !
     call PP_redux_wait(DIP_S(:,:,:,ikbz,i_sp_pol),COMM=PAR_COM_DIPk_ibz_A2A%COMM)
     !
     call live_timing(steps=1)
     !
   enddo ! ikbz
   !
   ! I need to distributed overlaps among all processors because
   ! the construction of covariant dipoles require the first and second 
   ! neighboar of each k-point
   !
   call PP_redux_wait(DIP_S(:,:,:,:,i_sp_pol),COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   !
 enddo ! i_sp_pol
 !
 call live_timing()
 !
 wf_ng       =wf_ng_save
 wf_ng_1st_BZ=wf_ng_1st_BZ_save
 !
 YAMBO_FREE(WF_symm)
 YAMBO_FREE(WF_shifted)
 YAMBO_FREE(WF_ik)
 YAMBO_FREE(WF_ikp)
 !
 call WF_free(WF)
 !
 call timing("DIPOLE_overlaps",OPR="stop")
 !
end subroutine Dipole_Overlaps
