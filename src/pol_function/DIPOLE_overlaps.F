!
! Copyright (C) 2000-2015 M. Gruning, C. Attaccalite and the YAMBO team 
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
subroutine DIPOLE_overlaps(Xen,Xk,X)
 !    
 use pars,              ONLY:SP,cZERO,schlen,pi
 use LIVE_t,            ONLY:live_timing
 use R_lattice,         ONLY:k_map,bz_samp,rl_sop,symm_shift,g_rot
 use D_lattice,         ONLY:nsym,idt_index,sop_inv
 use vec_operate,       ONLY:k_periodic_idx,c2a
 use com,               ONLY:msg,error
 use X_m,               ONLY:X_t,DIP_S
 use electrons,         ONLY:levels,n_spinor,spin
 use memory_m,          ONLY:mem_est
 use wrapper_omp,       ONLY:Vstar_dot_V_omp
 use wave_func,         ONLY:wf_ng,WF
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(in)    :: X
 type(bz_samp), intent(in)    :: Xk 
 !
 ! Work Space
 !
 character(schlen) :: dump_ch
 integer           :: ikbz,ikbzp,is,isp,ib,ibp,id,iw,iwp,i_spin,ik,ikp,ig
 integer           :: idx_kp(3),idx_k(3),shift_ik(3),shift_ikp(3)
 complex(SP), allocatable :: WF_tmp(:,:),WF_ik(:,:),WF_ikp(:,:)
 real(SP)          :: v1(3),v2(3),PHASE
 logical           :: USE_shifted_wf
 !
 allocate(WF_tmp(wf_ng,n_spinor),WF_ik(wf_ng,n_spinor),WF_ikp(wf_ng,n_spinor))
 call mem_est("WF_shited",(/size(WF_tmp)/),(/2*SP/))
 call mem_est("WF_ikp",(/size(WF_ikp)/),(/2*SP/))
 call mem_est("WF_ik",(/size(WF_ik)/),(/2*SP/))
 !
 DIP_S=cZERO
 !
 call live_timing("Overlaps:",Xk%nbz)
 !
 do ikbz=1,Xk%nbz
   !
   idx_k=k_map%k_map_inv(ikbz,:)
   !
   ik = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   !
!   shift_ik=-symm_shift(ikbz,:)
   !
   do id=1,3
     !
     if(k_map%max_kdir(id)==1) cycle
     !
     idx_kp=idx_k
     idx_kp(id)=idx_kp(id)+1
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
!     shift_ikp=-symm_shift(ikbzp,:)
     !
     shift_ikp(:) =0 
     !
     if(USE_shifted_wf) then
       !
       ! Add the shift due to border zone
       !
!       v1    =0._SP
!       v1(id)=1._SP
!       call c2a(v_in=v1,v_out=v2,mode='ka2i')
!       v1=matmul(rl_sop(:,:,sop_inv(isp)),v2)
!       call c2a(v_in=v1,mode='ki2a')
!       shift_ikp=shift_ikp+nint(v1)
       shift_ikp(id)=1
       !
     endif
     !
     do ib=1,X%ib(2)
       !
       do i_spin=1,n_spinor
         iw   =WF%index(ib,ik,i_spin)
         WF_ik(:wf_ng,i_spin)=WF%c(:wf_ng,iw)
       enddo
       !
!       call WF_shift_by_G((/ib,ik,1/),WF_tmp,shift_ik)
       ! 
!       do ig = 1,WF_ng
!          WF_ik(ig,:)=WF_tmp(g_rot(is,ig),:)
!       enddo
       !
       do ibp=1,X%ib(2)
         ! 
         ! Check if I crossed BZ border and impose periodic Gauge
         !
         call WF_shift_by_G((/ibp,ikp,1/),WF_ikp,shift_ikp)
         !
!         do ig = 1,WF_ng
!           WF_ikp(ig,:)=WF_tmp(g_rot(isp,ig),:)
!         enddo
         !
         do i_spin=1,n_spinor
           !
           DIP_S(ib,ibp,id,ikbz,i_spin)=Vstar_dot_V_omp(wf_ng,WF_ikp(:wf_ng,i_spin),WF_ik(:wf_ng,i_spin)) 
           !
         enddo
       enddo
     enddo
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 deallocate(WF_tmp,WF_ik,WF_ikp)
 call mem_est("WF_ik WF_ikp WF_tmp")
 !
end subroutine Dipole_Overlaps
