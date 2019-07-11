!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine Dipole_spin_magnetization(Xk,X)
 !
 ! This routine returns <sigma>
 !
 use pars,                ONLY:cI,SP
 use drivers,             ONLY:l_sc_run
 use interfaces,          ONLY:WF_load,WF_free
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:n_spinor,n_sp_pol
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_spin
 use fft_m,               ONLY:fft_size
 use wave_func,           ONLY:WF
 use com,                 ONLY:error
 use wrapper,             ONLY:Vstar_dot_V
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 logical                      :: l_mat_eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 integer                      :: ik,ic_wf,iv_wf,ic,iv,&
&                                i_sp_pol,i_spinor,i_spin,n_LT_steps,ik_mem
 !
 ! WF load
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 ! This is the case, for example, of the response function calculation.
 !
 call WF_load(WF,0,1,X%ib,(/1,Xk%nibz/),space='R',title='-Spin oscillators/R space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 ! Parallelization Mask 
 !======================
 ! Depending on the environment DIPOLES are distributed in different ways
 !
 call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps)
 !
 if (n_LT_steps>0) call live_timing('Dipoles: spin magnetization',n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     !
     if (n_LT_steps==0) cycle
     ! 
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     ! 
     do iv=X%ib(1),X%ib_lim(1) 
       do ic=max(iv,X%ib_lim(2)),X%ib(2)
         !
         if (.not.l_mat_eval_DIP(ic,iv)) cycle
         !
         iv_wf=WF%index(iv,ik,i_sp_pol)
         ic_wf=WF%index(ic,ik,i_sp_pol)
         !
         if (iv_wf==0) call error(" Error in parallel wf distribution (Dipole iv)")
         if (ic_wf==0) call error(" Error in parallel wf distribution (Dipole ic)")
         !
         ! Evaluate <iv_wf| sigma_i |ic_wf>
         !=================================
         !
         if (n_sp_pol==2) then
           !
           ! mz
           !
           DIP_spin(3,ic,iv,ik_mem,i_sp_pol)=real((-1)**(i_sp_pol-1),SP)*&
&                                             Vstar_dot_V(fft_size,WF%c(:,1,ic_wf),WF%c(:,1,iv_wf))
           !
         elseif (n_spinor==2) then
           !
           ! mx
           !
           DIP_spin(1,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,1,ic_wf),WF%c(:,2,iv_wf)) &
&                                            +Vstar_dot_V(fft_size,WF%c(:,2,ic_wf),WF%c(:,1,iv_wf))
           !
           ! my
           !
           DIP_spin(2,ic,iv,ik_mem,i_sp_pol)=-cI*Vstar_dot_V(fft_size,WF%c(:,1,ic_wf),WF%c(:,2,iv_wf)) &
&                                            +cI*Vstar_dot_V(fft_size,WF%c(:,2,ic_wf),WF%c(:,1,iv_wf))
           !
           ! mz
           !
           DIP_spin(3,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,1,ic_wf),WF%c(:,1,iv_wf)) &
&                                            -Vstar_dot_V(fft_size,WF%c(:,2,ic_wf),WF%c(:,2,iv_wf))
           !
         endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     ! 
   enddo        ! sp_pol loop
   !
 enddo          ! k-points
 !
 call live_timing()
 !
 ! CLEAN
 !
 call WF_free(WF)
 !
end subroutine Dipole_spin_magnetization
