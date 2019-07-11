!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine DIPOLE_overlaps(Xk,X)
 !    
 use pars,              ONLY:SP,cZERO,pi
 use LIVE_t,            ONLY:live_timing
 use R_lattice,         ONLY:k_map,bz_samp,WF_shifts,ng_closed,ng_vec
 use vec_operate,       ONLY:k_periodic_idx
 use X_m,               ONLY:X_t,DIP_S
 use electrons,         ONLY:n_spinor,n_sp_pol
 use wrapper_omp,       ONLY:Vstar_dot_V_omp
 use wave_func,         ONLY:wf_ng
 use IO_m,              ONLY:IO_and_Messaging_switch
 use parallel_m,        ONLY:PAR_COM_DIPOLES,PP_redux_wait,PAR_IND_Xk_bz,PAR_IND_Xk_bz_ID
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk 
 type(X_t),     intent(in)    :: X
 !
 ! Work Space
 !
 integer           :: ikbz,ikbzp,is,isp,ib,ibp,id,i_spin,i_spinor,i_sp_pol,ik,ikp
 integer           :: idx_kp(3),idx_k(3),shift(3),max_step,istep
 complex(SP)       :: WF_symm(wf_ng,n_spinor,X%ib(2),n_sp_pol)
 complex(SP)       :: WF_shifted(wf_ng,n_spinor,X%ib(2),n_sp_pol)
 complex(SP)       :: WF_ik(wf_ng,n_spinor,X%ib(2),n_sp_pol),WF_ikp(wf_ng,n_spinor,X%ib(2),n_sp_pol)
 logical           :: USE_shifted_wf
 !
 ! This is needed to construct grot for all wf_ngs.
 !
 call IO_and_messaging_switch("-io_out")
 ng_closed=ng_vec
 call G_shells_finder()
 call IO_and_messaging_switch("+io_out")
 !
 DIP_S=cZERO
 !
 call live_timing("Overlaps:",PAR_IND_Xk_bz%n_of_elements(PAR_IND_Xk_bz_ID+1))
 !
 do ikbz=1,Xk%nbz
   !
   if(.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
   !
   idx_k=k_map%k_map_inv(ikbz,:)
   !
   ik = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   !
   call WF_symm_kpoint((/1,X%ib(2)/),ikbz,Xk,WF_symm)
   !  
   call WF_shift_kpoint((/1,X%ib(2)/),ikbz,WF_shifts(ikbz,:),Xk,WF_symm,WF_ik)
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
       call WF_symm_kpoint((/1,X%ib(2)/),ikbzp,Xk,WF_symm)
       !
       call WF_shift_kpoint((/1,X%ib(2)/),ikbzp,WF_shifts(ikbzp,:),Xk,WF_symm,WF_ikp)
       !
       if(USE_shifted_wf) then
         !  
         ! Check if I crossed BZ border and impose periodic Gauge
         !
         shift=0
         shift(id)=1
         call WF_shift_kpoint((/1,X%ib(2)/),ikbzp,shift,Xk,WF_ikp,WF_shifted)
         !
       else
         WF_shifted=WF_ikp
       endif
       !
       do i_sp_pol=1,n_sp_pol
         do ibp=1,X%ib(2)
           do ib=1,X%ib(2)
             !DIP_S(ib,ibp,id+(istep-1)*3,ikbz,i_sp_pol)=DIP_S(ib,ibp,id+(istep-1)*3,ikbz,i_sp_pol)+ &
             DIP_S(ib,ibp,id+(istep-1)*3,ikbz,i_sp_pol)= &
             &  Vstar_dot_V_omp(wf_ng*n_spinor,WF_ik(:wf_ng,:,ib,i_sp_pol),WF_shifted(:wf_ng,:,ibp,i_sp_pol)) 
           enddo ! ibp
         enddo ! ib
       enddo ! i_sp_pol
       !
     enddo ! istep
   enddo ! idir
   !
   call live_timing(steps=1)
   !
 enddo ! ikbz
 !
 call live_timing()
 !
 do i_sp_pol=1,n_sp_pol
   call PP_redux_wait(DIP_S(:,:,:,:,i_sp_pol),COMM=PAR_COM_DIPOLES%COMM)
 enddo
 !
end subroutine Dipole_Overlaps
