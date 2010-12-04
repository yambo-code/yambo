!
! Copyright (C) 2000-2010 M. Gruening and the YAMBO team
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
subroutine OEP_kli_iterative(E,k,Vkli,Vslt,U,rho)
  !
  ! Though KLI can be calculated analitically, I used here the iterative 
  ! approach ?? la Kummel-Perdew with the only difference that in KLI orbital 
  ! shifts are always 0.  
  !
 use pars,            ONLY:SP,DP,LCHLEN
 use com,             ONLY:error
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,nkibz
 use D_lattice,       ONLY:nsym,i_time_rev
 use FFT_m,           ONLY:fft_size,fft_rot_r
 use QP_m,            ONLY:QP_Vnl_xc,QP_n_states,QP_table,QP_ng_Sx
 use collision,       ONLY:collision_reset 
 use memory_m,        ONLY:mem_est
 use par_proc_m,      ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,   ONLY:par_indexes
 use wave_func,       ONLY:WF_load,wf,wf_state
 use SC,              ONLY:it_now,OEP_steps,OEP_threshold
 use timing,          ONLY:live_time_msg
 use stderr,          ONLY:intc,real2ch
 !
 ! I/O
 !
 implicit none
 real(SP),  intent(inout) :: Vkli(fft_size)
 real(SP),  intent(in) :: Vslt(fft_size),rho(fft_size)
 complex(SP), intent(in):: U(nkibz,n_met_bands,fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k
 !
 ! Work space
 !
 type(pp_indexes) ::px
 integer          ::ii,iv1,iv2,ik
 real(SP)         ::V_io(fft_size),V_nm1(fft_size)
 real(SP)         ::DeltaEps,V_resp(fft_size),reached_treshold
 integer ir,ifft,is,rho_sym
 logical          ::lerr
 character(lchlen)  ::dumb_ch
 !
 call live_time_msg("n","","","%s")
 V_io = Vkli
 lerr = .false.
 do ii=1,OEP_steps
   V_nm1 = V_io
     V_resp = 0._SP
     do ik = 1, nkibz 
       do iv1 = 1, E%nbf-1 ! -1 to fix the additive constant of the potential
         if (abs(E%E(iv1,ik,1)-E%E(E%nbf,ik,1)).lt.0.0001_SP) cycle
         ifft = wf_state(iv1,ik,1)
         DeltaEps = 0._SP
         do ir = 1, fft_size
           DeltaEps = DeltaEps + conjg(wf(ir,ifft))*wf(ir,ifft)*V_nm1(ir)-real(U(ik,iv1,ir))
         end do
         V_resp(:) = V_resp(:) + E%f(iv1,ik,1)*k%weights(ik)*DeltaEps*&
             &conjg(wf(:,ifft))*wf(:,ifft)
       end do
     end do
     rho_sym = nsym/(i_time_rev+1)
     V_io = 0._SP
     do is=1,rho_sym
       V_io(:)=V_io(:)+real(V_resp(fft_rot_r(is,:)),SP)/real(nsym,SP)
     enddo
     V_io = Vslt + V_io*(1._SP+real(i_time_rev,SP))
     V_io = V_io/rho
     !
     reached_treshold=maxval(abs(V_io(1:)-V_nm1(1:)))
     dumb_ch='[KLI] Cycle '//trim(intc(ii))//' Accuracy :'//&
&            trim(real2ch(reached_treshold))//'|'//trim(real2ch(OEP_threshold))
     call live_time_msg("r",dumb_ch,"","%s")
   if (reached_treshold<OEP_threshold) exit
   lerr=(ii==OEP_steps)
 end do
 if (lerr) call error('Procedure to calculate KLI did not converge')
 Vkli=V_io
 !
end subroutine OEP_kli_iterative

