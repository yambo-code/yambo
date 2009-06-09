!
! Copyright (C) 2000-2008 C. Attaccalite and the YAMBO team
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
subroutine RT_fill_Hartree_matrix()
 !
 ! Full Hartree_Matrix(:,:,:,:) for the FastHartree
 ! spin not supported yet
 !
 use pars,          ONLY:pi,SP,DP
 use electrons,     ONLY:n_spin,spin,n_sp_pol
 use D_lattice,     ONLY:DL_Vol
 use timing,        ONLY:live_timing
 use R_lattice,     ONLY:bare_qpg
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_nk,QP_n_states,QP_table
 use wave_func,     ONLY:wf_state,wf,wf_ng
 use real_time,     ONLY:Hartree_Matrix
 use FFT_m,         ONLY:fft_size,fftw_plan,fft_dim,fft_g_table
 !
 implicit none
 !
 ! Work Space
 !
 integer :: iqp,ib,ibp,i_spin,ik,ikp,i1,i2,ifft,ifftp
 complex(DP)    :: v_rho_tmp(fft_size)
 complex(DP)    :: vhg_element(wf_ng)
 complex(SP)    :: c_ws(fft_size)
 !
#if defined _DOUBLE
 complex(DP):: zdotc
#else
 complex(SP):: cdotc
#endif  
 !
 call live_timing('[RT] Fill Hartree Matrix :',SC_bands) 
 !
 do i1=1,SC_bands
   !
   call live_timing(steps=1) 
   !
   do i2=1,SC_bands
     do ik=1,QP_nk
       !
       ifft =wf_state(i1,ik,1)
       ifftp=wf_state(i2,ik,1)
       !
       v_rho_tmp(:)=wf(:,ifft)*conjg(wf(:,ifftp))
       !
#if defined _FFTW
       fftw_plan=0
       call fft_3d(v_rho_tmp,fft_dim,-1,fftw_plan)
#else
       call fft_3d(v_rho_tmp,fft_dim,-1)
#endif  
       !
       vhg_element(:)      =(0.,0.)
       vhg_element(2:wf_ng)=((4.*pi/DL_vol)*(v_rho_tmp(fft_g_table(2:wf_ng,1))))&
&              /bare_qpg(1,2:wf_ng)**2.
       !
       v_rho_tmp(:)=(0.,0.)
       v_rho_tmp(fft_g_table(1:wf_ng,1))=vhg_element(1:wf_ng) 
       !
       !
       ! FFT call: vhr in direct space
       !
#if defined _FFTW
       fftw_plan=0
       call fft_3d(v_rho_tmp,fft_dim,1,fftw_plan)
#else
       call fft_3d(v_rho_tmp,fft_dim,1)
#endif
       !
       ! Now fill quasi particle matrix elements
       !
       do iqp=1,QP_n_states
         !
         ib = QP_table(iqp,1)
         ibp= QP_table(iqp,2)
         ikp= QP_table(iqp,3)
         !
         i_spin=1
         !
!        i_spin=spin(QP_table(iqp,:))
         !
         ifft  =wf_state(ib,ikp,i_spin)
         ifftp =wf_state(ibp,ikp,i_spin)
         c_ws(:)=v_rho_tmp(:)*wf(:,ifftp)
         !
#if defined _DOUBLE
   Hartree_Matrix(i1,i2,ik,iqp) = zdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#else
   Hartree_Matrix(i1,i2,ik,iqp) = cdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#endif
         !
         !
       enddo
       !
     enddo
   enddo
 enddo
 !
 call live_timing
 !
 fftw_plan = 0
 !
end subroutine RT_fill_Hartree_matrix
