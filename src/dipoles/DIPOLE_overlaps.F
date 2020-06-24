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
 use R_lattice,         ONLY:k_map,bz_samp,WF_shifts,ng_closed,ng_vec
 use vec_operate,       ONLY:k_periodic_idx
 use DIPOLES,           ONLY:DIPOLE_t,DIP_S
 use electrons,         ONLY:n_spinor,n_sp_pol
 use wrapper,           ONLY:Vstar_dot_V
 use wave_func,         ONLY:wf_ng
 use IO_m,              ONLY:IO_write
 use parallel_m,        ONLY:PAR_IND_DIPk_bz,PAR_IND_DIPk_bz_ID,PAR_COM_DIPk_ibz_A2A, &
&                            PAR_IND_CON_BANDS_OVLP,PAR_IND_VAL_BANDS_OVLP,PAR_COM_DIPk_ibz_INDEX,PP_redux_wait
 use deviceXlib_m,      ONLY:dev_memcpy
 use interfaces,        ONLY:WF_shift_kpoint,WF_symm_kpoint
#include<dev_defs.h>
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk 
 type(DIPOLE_t),intent(in)    :: Dip
 !
 ! Work Space
 !
 integer           :: ikbz,ikbzp,is,isp,ib,ibp,id,i_spin,i_spinor,i_sp_pol,ik,ikp
 integer           :: idx_kp(3),idx_k(3),shift(3),max_step,istep
 complex(SP)       :: WF_symm(wf_ng,n_spinor,Dip%ib(2),n_sp_pol)
 complex(SP)       :: WF_shifted(wf_ng,n_spinor,Dip%ib(2),n_sp_pol)
 complex(SP)       :: WF_ik(wf_ng,n_spinor,Dip%ib(2),n_sp_pol),WF_ikp(wf_ng,n_spinor,Dip%ib(2),n_sp_pol)
#ifdef _CUDA
 attributes(device):: WF_symm,WF_shifted,WF_ik,WF_ikp
#endif
 logical           :: USE_shifted_wf,IO_write_save
 !
 ! This is needed to construct grot for all wf_ngs.
 !
 IO_write_save=IO_write
 IO_write     =.FALSE.
 ng_closed=ng_vec
 call G_shells_finder()
 IO_write     =IO_write_save
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
     do id=1,3
       !
       if(k_map%max_kdir(id)==1) cycle
       !
       max_step=1
       if(k_map%max_kdir(id)>=6) max_step=2
       !
       do istep=1,max_step
         !
         idx_kp=idx_k
         idx_kp(id)=idx_kp(id)+istep
         !
         USE_shifted_wf=.false.
         if(idx_kp(id)>k_map%max_kdir(id)) USE_shifted_wf=.true.
         !
         idx_kp=k_periodic_idx(idx_kp,k_map)
         ikbzp=k_map%k_map_dir(idx_kp(1),idx_kp(2),idx_kp(3)) 
         !
         ikp = Xk%sstar(ikbzp,1)
         isp = Xk%sstar(ikbzp,2)
         !
         call WF_symm_kpoint((/1,Dip%ib(2)/),ikbzp,Xk,WF_symm)
         !
         call WF_shift_kpoint((/1,Dip%ib(2)/),ikbzp,WF_shifts(ikbzp,:),Xk,WF_symm,WF_ikp)
         !
         if(USE_shifted_wf) then
           !  
           ! Check if I crossed BZ border and impose periodic Gauge
           !
           shift=0
           shift(id)=1
           call WF_shift_kpoint((/1,Dip%ib(2)/),ikbzp,shift,Xk,WF_ikp,WF_shifted)
           !
         else
           call dev_memcpy(WF_shifted,WF_ikp)
         endif
         !
         !
         do ib=1,Dip%ib(2)
           if(.not.PAR_IND_VAL_BANDS_OVLP%element_1D(ib)) cycle
           do ibp=1,Dip%ib(2)
             if(.not.PAR_IND_CON_BANDS_OVLP%element_1D(ibp)) cycle
             DIP_S(ib,ibp,id+(istep-1)*3,ikbz,i_sp_pol)= &
                       &  Vstar_dot_V(wf_ng*n_spinor,WF_ik(:,:,ib,i_sp_pol),WF_shifted(:,:,ibp,i_sp_pol)) 
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
end subroutine Dipole_Overlaps
