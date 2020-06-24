!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA   
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
subroutine el_density_vbands(en,Xk,rho,VAL_BANDS)
 !
 !Electronic density.
 !
 !Note that here the identity
 !
 ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
 ! 
 !where S_k is a symm. op. in the star of k, S is a generic symm. op.
 !and R_k is the rank of the small group at k.
 !
 !Using (1) the density is calculated in two steps
 !
 ! rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
 !        = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
 !        = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
 !
 !where we have used the relation
 !
 ! w_k = nsym/(nkibz * rank(k))
 !
 !
 use pars,          ONLY:SP,cZERO,rZERO
 use electrons,     ONLY:levels,n_sp_pol,spin_occ,n_spin
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:WF
 use QP_m,          ONLY:QP_nk
 use nl_optics,     ONLY:NL_bands
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,PAR_Xk_nibz
 !
 implicit none
 type(bz_samp),intent(in)  :: Xk
 type(levels), intent(in)  :: en       
 real(SP),     intent(out) :: rho(fft_size)
 complex(SP),  intent(in)  :: VAL_BANDS(fft_size,en%nbf,PAR_Xk_nibz,n_sp_pol)
 !
 ! Work Space 
 !
 integer     :: i1,iv,ik,is,rho_syms,ir,ik_mem
 real(SP)    :: rho_no_sym(fft_size)
 !
 rho=rZERO
 rho_no_sym=rZERO
 !
 do ik=1,Xk%nibz
   !  
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
!$omp parallel do default(shared), private(ir,is)
   do ir=1,fft_size
     do is=1,n_sp_pol
       rho_no_sym(ir)=rho_no_sym(ir)+real(spin_occ,SP)*Xk%weights(ik)*sum(abs(VAL_BANDS(ir,:,ik_mem,is))**2._SP)
     enddo
   enddo
!$omp end parallel do
   !
 enddo
 !
 call PP_redux_wait(rho_no_sym,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 ! Simmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
!$omp parallel do default(shared), private(ir,i1)
 do ir=1,fft_size
   do i1=1,rho_syms
     rho(ir)=rho(ir)+real(rho_no_sym(fft_rot_r(ir,i1)),SP)/real(nsym,SP)
   enddo
 enddo
!$omp end parallel do
 !
!$omp parallel do default(shared), private(ir)
 do ir=1,fft_size
   rho(ir)=(1._SP+i_time_rev)*rho(ir)
 enddo
!$omp end parallel do
 !
end subroutine
