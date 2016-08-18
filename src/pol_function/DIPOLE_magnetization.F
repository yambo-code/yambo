!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine Dipole_magnetization(Xen,Xk,X)
 !
 ! This routine returns <sigma> (and <L> todo).
 !
 use pars,                ONLY:SP,schlen,cZERO,cI
 use drivers,             ONLY:l_sc_run
 use interfaces,          ONLY:WF_load
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_spin
 use memory_m,            ONLY:mem_est
 use fft_m,               ONLY:fft_size
 use wave_func,           ONLY:WF
 use com,                 ONLY:error,msg
 use wrapper,             ONLY:Vstar_dot_V
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                              PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                              PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 integer                      :: ik,ic_wf(n_spinor),iv_wf(n_spinor),ic,iv,&
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
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)
 !
 if (n_LT_steps>0) call live_timing('Dipoles spin',n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   !
   if (n_LT_steps==0) cycle
   ! 
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
     do iv=X%ib(1),X%ib_lim(1) 
       !
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       !
       do ic=max(iv,X%ib_lim(2)),X%ib(2)
         !
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         !
         do i_spinor=1,n_spinor
           !
           i_spin=i_sp_pol+i_spinor-1
           iv_wf(i_spinor)=WF%index(iv,ik,i_spin)
           ic_wf(i_spinor)=WF%index(ic,ik,i_spin)
           !
           if (iv_wf(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole iv)")
           if (ic_wf(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole ic)")
           !
         enddo
         !
         ! Evaluate <iv_wf| sigma_i |ic_wf>
         !=================================
         !
         if (n_spinor==2) then
           !
           ! mx
           !
           DIP_spin(1,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,ic_wf(1)),WF%c(:,iv_wf(2))) &
&                                            +Vstar_dot_V(fft_size,WF%c(:,ic_wf(2)),WF%c(:,iv_wf(1)))
           !
           ! my
           !
           DIP_spin(2,ic,iv,ik_mem,i_sp_pol)=-cI*Vstar_dot_V(fft_size,WF%c(:,ic_wf(1)),WF%c(:,iv_wf(2))) &
&                                            +cI*Vstar_dot_V(fft_size,WF%c(:,ic_wf(2)),WF%c(:,iv_wf(1)))
         endif
         !
         ! mz
         !
         DIP_spin(3,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,ic_wf(1)),WF%c(:,iv_wf(1))) &
&                                          -Vstar_dot_V(fft_size,WF%c(:,ic_wf(2)),WF%c(:,iv_wf(2)))
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
end subroutine Dipole_magnetization
