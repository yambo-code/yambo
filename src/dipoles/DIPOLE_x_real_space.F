!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine DIPOLE_x_real_space(Xen,Xk,Dip,idir_todo)
 !
 use drivers,            ONLY:l_sc_run
 use pars,               ONLY:SP,cI,cZERO
 use D_lattice,          ONLY:x_cc
 use wave_func,          ONLY:WF
 use interfaces,         ONLY:WF_load
 use wrapper_omp,        ONLY:Vstar_dot_V_omp
 use FFT_m,              ONLY:fft_size
 use R_lattice,          ONLY:bz_samp
 use electrons,          ONLY:levels,n_sp_pol,n_spinor
 use DIPOLES,            ONLY:DIPOLE_t,DIP_iR,DIP_P
 use X_m,                ONLY:l_X_terminator
 use com,                ONLY:error
 use LIVE_t,             ONLY:live_timing
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                              PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                              PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(in)    :: Dip
 logical,       intent(in)    :: idir_todo(3)
 !
 ! Work space
 !
 integer                      :: ik,ik_mem,i_sp_pol,i_spinor,i_spin,ic,iv,ic_min,&
&                                icfft(n_spinor),ivfft(n_spinor),idir,n_LT_steps
 real(SP)                     :: Ev_m_Ec
 complex(SP)                  :: DIP_tmp(3,Dip%ib_lim(2):Dip%ib(2),Dip%ib(1):Dip%ib_lim(1))
 !
 call load_cc(.false.,.true.)
 !
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)
 !
 if (n_LT_steps>0) call live_timing('Dipoles in real space',n_LT_steps)
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz 
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     call WF_load(WF,0,1,(/1,Dip%ib(2)/),(/ik,ik/),space='R',title='-Oscillators/R space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     DIP_tmp=cZERO
     !
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       !
       ic_min=max(iv,Dip%ib_lim(2))
       if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,Dip%ib(2)
         !
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         !
         do i_spinor=1,n_spinor
           !
           i_spin=i_sp_pol+i_spinor-1
           ivfft(i_spinor)=WF%index(iv,ik,i_spin)
           icfft(i_spinor)=WF%index(ic,ik,i_spin)
           !
           if (ivfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole iv)")
           if (icfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole ic)")
           !
           do idir=1,3
             !
             ! Here I assume that for the non periodic direction id_red==id_cartesian
             !
             if(.not.idir_todo(idir)) cycle
             !
             DIP_tmp(idir,ic,iv) = DIP_tmp(idir,ic,iv) + &
&                       Vstar_dot_V_omp(fft_size,WF%c(1:fft_size,ivfft(i_spinor)), &
&                             x_cc(1:fft_size,idir)*WF%c(1:fft_size,icfft(i_spinor)) )
             !
           enddo
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo 
       !
     enddo
     !
     !======
     ! <iR>
     !======
     do idir=1,3
       if(.not.idir_todo(idir)) cycle
       DIP_iR(idir,:,:,ik_mem,i_sp_pol)=cI*DIP_tmp(idir,:,:)
     enddo
     !
     !=====
     ! <P>
     !=====
     if(.not.Dip%force_v_g_space) then
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
         do ic=max(iv,Dip%ib_lim(2)),Dip%ib(2)
           if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
           ! This is directly p_eff, see Dipole transverse and PRB 48, 11789 (1993)
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           do idir=1,3
             if(.not.idir_todo(idir)) cycle
             DIP_P(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec
           enddo
         enddo
       enddo
     endif
     !
     call WF_free(WF)
     !
   enddo 
   !
 enddo
 !
 call live_timing()
 !
 YAMBO_FREE(x_cc)
 !
end subroutine Dipole_x_real_space
