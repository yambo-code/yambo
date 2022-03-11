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
subroutine FL_determinants(en,FL_S_plus,FL_time_S_det,ikbz,ikp)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield,EtoT
 use nl_optics,      ONLY:NL_bands,NL_damping
 use fl_optics,      ONLY:max_fl_mode,max_extra_mode,n_tot_fl_modes,FL_states,&
&                         n_tot_extra_modes,FL_QE,n_tot_time_steps
 !
 implicit none
 !
 type(levels), intent(in)   :: en
 complex(SP), intent(inout) :: FL_S_plus(en%nbf,en%nbf,n_tot_extra_modes)
 complex(DP), intent(inout) :: FL_time_S_det(n_tot_time_steps)
 integer,     intent(in)    :: ikbz,ikp
 !
 ! Work Space
 !
 complex(SP)   :: FL_time_S_plus(en%nbf,en%nbf,n_tot_time_steps)
 integer       :: i_t,i1_extra,i1_shifted_fl,i1,i2
 real(SP)      :: time,w0,T_period,t_samples(n_tot_time_steps)
 !
 !
 FL_time_S_plus =cZERO 
 FL_time_S_det  =cZERO
 !
 ! Calculate S plus as a function of time
 ! ======================================
 !
 w0 = Efield(1)%frequency(1) ! in Ha
 T_period = EtoT(w0)
 forall(i_t=1:n_tot_time_steps) t_samples(i_t)=T_period*(i_t-1._SP)/n_tot_time_steps
 !
 do i_t=1,n_tot_time_steps
   !
   do i1_extra=1,n_tot_extra_modes
     !
     i1_shifted_fl = i1_extra -(max_extra_mode+1)
     !
     FL_time_S_plus(:,:,i_t)  = FL_time_S_plus(:,:,i_t) &
                            & + exp(-1._SP*cI*i1_shifted_fl*w0*t_samples(i_t)) &
                            & * FL_S_plus(:,:,i1_extra)
                            !& + exp(-1*cI*real(i1_shifted_fl,SP)*2._SP*pi*(i_t-1)/n_tot_time_steps) &
     !
    !print*, '------------------'
    !print*, i1_shifted_fl
    !print*, FL_S_plus(:,:,i1_extra)
   enddo
 ! print*, exp(-1*cI*2*pi*(i_t-1)/n_tot_time_steps)
   !
   ! Add Quasi-energies exponentials
   ! ================================
   do i1=1,en%nbf
     !
     do i2=1,en%nbf
       !
       FL_time_S_plus(i1,i2,i_t) = FL_time_S_plus(i1,i2,i_t)     &
                               &  * exp(+cI*FL_QE(i1,ikbz,1)*t_samples(i_t)) &
                               &  * exp(-cI*FL_QE(i2,ikp ,1)*t_samples(i_t))!&
                             ! &  * exp(-NL_damping**2*t_samples(i_t)**2)  

       !
     enddo
     !
   enddo
   !
   ! Calculate determinant as a function of time
   ! ============================================
   !
   !aim137 prelim
   !aim137: for now I am doing inversion, but I should really write a subroutine to just get the det
   call SERIAL_inversion(en%nbf,FL_time_S_plus(:,:,i_t),FL_time_S_det(i_t),.true.)
   !
 enddo
 !
end subroutine FL_determinants
