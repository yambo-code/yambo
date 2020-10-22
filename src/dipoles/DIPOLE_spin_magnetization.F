!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine Dipole_spin_magnetization(Xen,Xk,dip)
 !
 ! This routine returns <sigma>
 !
 use pars,                ONLY:cI,SP
 use drivers,             ONLY:l_sc_run
 use interfaces,          ONLY:WF_load,WF_free
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t
 use DIPOLES,             ONLY:DIP_spin,DIPOLE_t
 use fft_m,               ONLY:fft_size
 use wave_func,           ONLY:WF
 use com,                 ONLY:error
 use wrapper,             ONLY:Vstar_dot_V
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                              PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                              PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer                      :: ik,ic_wf,iv_wf,ic,iv,&
&                                i_sp_pol,i_spinor,i_spin,n_LT_steps,ik_mem
 !
 ! Parallelization Mask 
 !======================
 ! Depending on the environment DIPOLES are distributed in different ways
 !
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)
 !
 if (n_LT_steps>0) call live_timing('Dipoles: spin',n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 do i_sp_pol=1,n_sp_pol
   !
   if (n_LT_steps==0) cycle
   !
   do ik=1,Xk%nibz
     ! 
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     call WF_load(WF,0,1,Dip%ib,(/ik,ik/),sp_pol_to_load=(/i_sp_pol,i_sp_pol/),space='R',title='-Spin oscillators/R space',&
&               force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
     ! 
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       !
       do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
         !
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
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
&                                             Vstar_dot_V(fft_size,WF%c(:,1,iv_wf),WF%c(:,1,ic_wf))
           !
         elseif (n_spinor==2) then
           !
           ! mx
           !
           DIP_spin(1,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,1,iv_wf),WF%c(:,2,ic_wf)) &
&                                            +Vstar_dot_V(fft_size,WF%c(:,2,iv_wf),WF%c(:,1,ic_wf))
           !
           ! my
           !
           DIP_spin(2,ic,iv,ik_mem,i_sp_pol)=-cI*Vstar_dot_V(fft_size,WF%c(:,1,iv_wf),WF%c(:,2,ic_wf)) &
&                                            +cI*Vstar_dot_V(fft_size,WF%c(:,2,iv_wf),WF%c(:,1,ic_wf))
           !
           ! mz
           !
           DIP_spin(3,ic,iv,ik_mem,i_sp_pol)= Vstar_dot_V(fft_size,WF%c(:,1,iv_wf),WF%c(:,1,ic_wf)) &
&                                            -Vstar_dot_V(fft_size,WF%c(:,2,iv_wf),WF%c(:,2,ic_wf))
           !
         endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     !
     call WF_free(WF)
     ! 
   enddo        ! k-points loop
   !
 enddo          ! sp_pol   loop
 !
 call live_timing()
 !
end subroutine Dipole_spin_magnetization
