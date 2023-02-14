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
subroutine FL_build_valence_bands(en,Xk,FL_V_bands,FL_V_bands_time)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:Efield,EtoT
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands,NL_damping
 use fl_optics,      ONLY:max_fl_mode,max_extra_mode,n_tot_fl_modes,FL_states,&
&                         n_tot_extra_modes,FL_QE,max_fl_mode,FL_FT_extra_M,FL_FT_extra_Mm1
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX,master_cpu
 use wrapper,        ONLY:M_by_V
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels), intent(in)   :: en
 type(bz_samp),intent(in)   :: Xk
 complex(SP),  intent(in)   :: FL_V_bands(NL_bands(2),n_tot_fl_modes,maxval(en%nbf),QP_nk,n_sp_pol)
 complex(SP),  intent(out)  :: FL_V_bands_time(NL_bands(2),n_tot_extra_modes,maxval(en%nbf),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 integer       :: i_t,i1_fl,i1_extra,i1_shifted_fl,i1,i2,ik_bz,is,i_damping
 real(SP)      :: time,w0,T_period,t_samples(n_tot_extra_modes)
 complex(SP)   :: CHECK_FL_V_bands(n_tot_extra_modes)
 complex(SP)   :: CHECK_FL_V_bands_time(n_tot_extra_modes)
 !
#if defined _TIMING
 call timing('FL Density',OPR='start')
#endif
 !
 w0 = Efield(1)%frequency(1) ! in Ha
 !
 FL_V_bands_time = cZERO 
 !
 ! Calculate FL_V_bands_time as a function of time
 ! ===============================================
 !
 do is=1,n_sp_pol
   !
   do ik_bz=1,QP_nk
     !
     if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
     !
     do i_t=1,n_tot_extra_modes
       !
       do i1_fl=1,n_tot_fl_modes
         !
         i1_shifted_fl = i1_fl -(max_fl_mode+1)
         i_damping = 1; if (i1_shifted_fl == 0) i_damping = 0
         !
         FL_V_bands_time(:,i_t,:,ik_bz,is) = &
       & FL_V_bands_time(:,i_t,:,ik_bz,is) + &
       & exp(-1*cI*real(i1_shifted_fl,SP)*2._SP*pi*(i_t-1)/n_tot_extra_modes) * &
       & FL_V_bands(:,i1_fl,:,ik_bz,is)
         !
         !& exp(NL_damping/w0*real(i_damping,SP)*2._SP*pi*(i_t-1)/n_tot_extra_modes) * &
         !
         !print*, '------------------'
         !print*, i1_shifted_fl
         !print*, FL_V_bands(:,:,i1_extra)
       enddo
     ! print*, exp(-1*cI*2*pi*(i_t-1)/n_tot_extra_modes)
       !
     enddo
     !
   enddo
   !
   call PP_redux_wait(FL_V_bands_time(:,:,:,:,is) , COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
 enddo
 !
#if defined _TIMING
 call timing('FL Density',OPR='stop')
#endif
 !
 !aim137 DEBUG<
!if (master_cpu) then
!  do ik_bz=26,26!1,QP_nk
!    print*, 'k-point ',ik_bz
!    write(*,'(100(1f12.8))') real(FL_V_bands_time(4,:,4,ik_bz,1))
!    write(*,'(100(1f12.8))') real(FL_V_bands_time(5,:,4,ik_bz,1))
!  enddo
!endif
 !aim137 DEBUG>
 !aim137 DEBUG check normalisation <
 !do ik_bz=1,QP_nk
 ! print*, '------ K-point: ',ik_bz
 ! do i_t=1,n_tot_extra_modes
 !  !
 !  print*, sum(conjg(FL_V_bands_time(:,i_t,4,ik_bz,1))*FL_V_bands_time(:,i_t,4,ik_bz,1))
 !  !
 ! enddo
 !enddo
 !aim137 DEBUG check normalisation >
 !aim137 DEBUG check FT <
 !do ik_bz=1,QP_nk
 ! print*, '------ K-point: ',ik_bz,' -- proj. of band 4 over band 5'
 ! CHECK_FL_V_bands = cZERO
 ! CHECK_FL_V_bands_time = cZERO
 ! do i1_fl=1,n_tot_fl_modes
 !  i1_shifted_fl = i1_fl -(max_fl_mode+1)
 !  i1_extra = i1_shifted_fl +(max_extra_mode+1)
 !  CHECK_FL_V_bands(i1_extra) = FL_V_bands(4,i1_fl,4,ik_bz,1)
 ! enddo
 ! print*, 'Initial:',CHECK_FL_V_bands
 ! !
 ! !from FL_V_bands to time and back
 ! call M_by_V('N',n_tot_extra_modes,n_tot_extra_modes,cONE,FL_FT_extra_M,&
 ! &n_tot_extra_modes,CHECK_FL_V_bands(:),1,cZERO,CHECK_FL_V_bands_time(:),1)
 ! !
 ! CHECK_FL_V_bands = cZERO
 ! call M_by_V('N',n_tot_extra_modes,n_tot_extra_modes,cONE,FL_FT_extra_Mm1,&
 ! &n_tot_extra_modes,CHECK_FL_V_bands_time(:),1,cZERO,CHECK_FL_V_bands(:),1)
 ! print*, 'Final 1:',CHECK_FL_V_bands
 ! !
 ! !from FL_V_bands_time to FKS
 ! CHECK_FL_V_bands = cZERO
 ! call M_by_V('N',n_tot_extra_modes,n_tot_extra_modes,cONE,FL_FT_extra_Mm1,&
 ! &n_tot_extra_modes,FL_V_bands_time(4,:,4,ik_bz,1),1,cZERO,CHECK_FL_V_bands(:),1)
 ! print*, 'Final 2:',CHECK_FL_V_bands
 ! !
 !enddo
 !aim137 DEBUG check FT >
 !
end subroutine FL_build_valence_bands
