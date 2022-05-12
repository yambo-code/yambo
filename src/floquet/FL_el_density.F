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
subroutine FL_el_density(en,Xk,rho,FL_V_bands,lowest_band)
 !
 ! This is based on nloptics/el_density_vbands.F
 !
 use pars,          ONLY:SP,rZERO
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,PAR_Xk_nibz
 use parallel_int,  ONLY:PP_redux_wait
 use QP_m,          ONLY:QP_nk
 use nl_optics,     ONLY:NL_bands
 use fl_optics,     ONLY:max_fl_mode,FL_space_dim,n_tot_fl_modes,FL_states,n_tot_extra_modes,max_extra_mode
 !
 implicit none
 type(bz_samp),intent(in)  :: Xk
 type(levels), intent(in)  :: en       
 real(SP),     intent(out) :: rho(fft_size,n_tot_extra_modes)
 complex(SP),  intent(in)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol)
 integer,      intent(in)  :: lowest_band
 !
 ! Work Space 
 !
 integer     :: i1,i2,ik,i_sp,rho_syms,ir,ik_mem,i_m
 integer     :: i1_extra,i1_fl,i_comb_fl,i_shift_comb_fl
 integer     :: i1_wf,i2_wf
 real(SP)    :: rho_no_sym(fft_size,n_tot_extra_modes)
 !
 rho=rZERO
 rho_no_sym=rZERO
 !
 do ik=1,Xk%nibz
   !  
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do i_m=lowest_band,en%nbf
     !
     do i1_extra=1,n_tot_extra_modes
       !
       do i1=lowest_band,NL_bands(2)   !j aim137 check - this should be from 1
         do i2=lowest_band,NL_bands(2) !i aim137 but I think it is safe to choose this
           do i1_fl=1,n_tot_fl_modes   !eta
             !
             i_shift_comb_fl = (i1_extra-(max_extra_mode+1)) &
                            &+ (i1_fl-(max_fl_mode+1))
             !
             if (abs(i_shift_comb_fl) > max_fl_mode) cycle
             i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
             !
             do i_sp=1,n_sp_pol
               !
               i1_wf=WF%index(i1,ik,i_sp)
               i2_wf=WF%index(i2,ik,i_sp)
               !
               do ir=1,fft_size
                 !
                 rho_no_sym(ir,i1_extra) = rho_no_sym(ir,i1_extra)+real(spin_occ,SP)*Xk%weights(ik) *&
               & conjg(FL_V_bands(i1,i1_fl,i_m,ik,i_sp)) * FL_V_bands(i2,i_comb_fl,i_m,ik,i_sp) *&
               & conjg(sum(WF%c(ir,:,i1_wf))) * sum(WF%c(ir,:,i2_wf)) 
                 ! aim137 critical - I have serious doubts about summing over spinors
                 !
               enddo
             enddo
             !
           enddo
         enddo
       enddo
       !
     enddo ! extra modes
     !
   enddo ! i_m band
   !
 enddo ! kpts
 !
 call PP_redux_wait(rho_no_sym,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 ! Simmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 do ir=1,fft_size
   do i1=1,rho_syms
     rho(ir,:)=rho(ir,:)+real(rho_no_sym(fft_rot_r(ir,i1),:),SP)/real(nsym,SP)
   enddo
 enddo
 !
 do ir=1,fft_size
   rho(ir,:)=(1._SP+i_time_rev)*rho(ir,:)
 enddo
 !
end subroutine
