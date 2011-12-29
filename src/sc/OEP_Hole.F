!
! Copyright (C) 2000-2012 M. Gruening and the YAMBO team
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
subroutine OEP_Hole(E,k,Vslt,U_x)
 !
 !  Vhole(Slater) part of the exact exchange/static RPA potential 
 !  multiplied by the density 
 !
 !  Found as 
 !
 !  v_s(r) = \sum_k \sum_n phi^*_{n,k}(r)*U_x^{n,k}(r)
 !
 ! 
 use pars,            ONLY:SP,DP,pi,schlen
 use D_lattice,       ONLY:i_time_rev,nsym
 use electrons,       ONLY:levels,n_met_bands,spin_occ
 use R_lattice,       ONLY:bz_samp,nkibz
 use FFT_m,           ONLY:fft_size,fft_rot_r
 use collision,       ONLY:ggwinfo,collision_reset 
 use memory_m,        ONLY:mem_est
 use par_proc_m,      ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,   ONLY:par_indexes
 use wave_func,       ONLY:wf,wf_state
 use timing,          ONLY:live_timing
 use stderr,          ONLY:intc
 use SC,              ONLY:l_oep_EWeight
 !
 ! I/O
 !
 implicit none
 real(SP),  intent(out)   :: Vslt(fft_size)
 complex(SP),intent(in)   :: U_x(nkibz,n_met_bands,fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k
 !
 ! Work space
 !
 type(ggwinfo)    ::isc
 type(pp_indexes) ::px
 integer          ::iv1,iv2,ir,ig,i1,ikbz,ik,iq,ifft,offt,tr_syms,os,is,iGo,qs
 real(DP)         ::Vslt_DP(fft_size)
 complex(DP)      ::f(fft_size)
 real(SP)         ::EWeight
 character(schlen)::ch
 logical          ::lostr, listr     
 !
 !
 !
 EWeight = 1._SP 
 Vslt_DP= 0._DP
 do iv1 = 1 ,E%nbm
   do ik = 1,k%nibz
     if (l_oep_EWeight) EWeight = E%E(iv1,ik,1)+E%Efermi(1)
     ifft=wf_state(iv1,ik,1)
     f(:)=conjg(wf(:,ifft))*U_x(ik,iv1,:)
     Vslt_DP = Vslt_DP + E%f(iv1,ik,1)*real(f,DP)*k%weights(ik)/EWeight
   end do
 end do
 Vslt= 0._SP
 tr_syms = nsym/(i_time_rev+1)
 do i1=1,tr_syms
   Vslt(:fft_size)=Vslt(:fft_size)+real(Vslt_DP(fft_rot_r(i1,:fft_size)),SP)/real(nsym,SP)
 enddo
 Vslt=Vslt*(real(i_time_rev,SP)+1._SP)
 !
end subroutine OEP_Hole
