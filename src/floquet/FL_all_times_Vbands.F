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
subroutine FL_all_times_Vbands(E,k,l_include_qe)
 !
 ! Subroutine to interace with ypp
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV,SPEED_OF_LIGHT,AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield,Divide_by_Field,A_ext
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_nbands,RT_bands,eval_DFT
 use nl_optics,      ONLY:NL_bands,NL_initial_P,NL_damping,NL_P_t,E_tot,E_ext,E_ks
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         max_time_step,FL_V_bands,FL_QE
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX,master_cpu
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k
 logical, intent(in)  :: l_include_qe
 !
 ! Work Space
 !
 integer       :: i_t,i1_fl,i1_extra,i1_shifted_fl,i1,i2,ik_bz,is,i_m
 real(SP)      :: time,w0,T_period
 complex(SP)   :: FL_V_bands_time(NL_bands(2),NE_steps,maxval(E%nbf),QP_nk,n_sp_pol)
 !
 !
 FL_V_bands_time = cZERO
 !
 ! Calculate FL_V_bands_time as a function of time
 ! ===============================================
 !
 w0 = Efield(1)%frequency(1) ! in Ha
 !
 do is=1,n_sp_pol
   !
   do ik_bz=1,QP_nk
     !
     if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
     !
     do i_t=1,NE_steps
       !
       time=real(i_t-1,SP)*RT_step
       !
       do i1_fl=1,n_tot_fl_modes
         !
         i1_shifted_fl = i1_fl -(max_fl_mode+1)
         !
         FL_V_bands_time(:,i_t,:,ik_bz,is) = &
       & FL_V_bands_time(:,i_t,:,ik_bz,is) + &
       & exp(-1*cI*real(i1_shifted_fl,SP)*w0*time) * &
       & FL_V_bands(:,i1_fl,:,ik_bz,is)
         !
        !print*, '------------------'
        !print*, i1_shifted_fl
        !print*, FL_V_bands(:,:,i1_extra)
       enddo
       !
       if (l_include_qe) then
         !
         do i_m=1,maxval(E%nbf)
           !
           FL_V_bands_time(:,i_t,i_m,ik_bz,is) = &
         & FL_V_bands_time(:,i_t,i_m,ik_bz,is) * &
         & exp(-cI*FL_QE(i_m,ik_bz,1)*time)
           !
         enddo
         !  
       endif
       !
     enddo
     !
   enddo
   !
   call PP_redux_wait(FL_V_bands_time(:,:,:,:,is) , COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
 enddo
 !
 !aim137 DEBUG<
if (master_cpu) then
  print*, '#k-point 6 - band 4'
  print*, '#time step - time[fs] - real part of coeffs for all KS bands - imag part of coeffs for all KS bands'
  do i_t=1,NE_steps
    time=real(i_t-1,SP)*RT_step
    write(*,'(I8,100(1e19.10))') i_t, time*AUT2FS, real(FL_V_bands_time(:,i_t,4,6,1)),aimag(FL_V_bands_time(:,i_t,4,6,1))
  enddo
endif
 !aim137 DEBUG>
end subroutine
