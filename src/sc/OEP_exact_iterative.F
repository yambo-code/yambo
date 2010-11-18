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
subroutine OEP_exact_iterative(E,k,Voep,U,rho)
  !
  ! Iterative solution of OEP equation following the 
  ! approach of Kummel-Perdew PRL 90, 043004; PRB 68, 035103
  ! 
 use pars,            ONLY:SP,DP,SCHLEN,PI
 use com,             ONLY:error,warning
 use electrons,       ONLY:levels,n_met_bands,nel
 use R_lattice,       ONLY:bz_samp,nkibz
 use D_lattice,       ONLY:nsym,i_time_rev,DL_vol
 use FFT_m,           ONLY:fft_size,fft_rot_r
 use QP_m,            ONLY:QP_Vnl_xc,QP_n_states,QP_table,QP_ng_Sx
 use collision,       ONLY:collision_reset 
 use memory_m,        ONLY:mem_est
 use par_proc_m,      ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,   ONLY:par_indexes
 use wave_func,       ONLY:WF_load,wf,wf_state
 use SC,              ONLY:it_now,SC_bands,OEP_steps,OEP_threshold
 use timing,          ONLY:live_time_msg
 use stderr,          ONLY:intc,real2ch
 !
 ! I/O
 !
 implicit none
 complex(SP), intent(in)  :: U(nkibz,n_met_bands,fft_size)
 real(SP), intent(in)     :: rho(fft_size) 
 real(SP),  intent(inout) :: Voep(fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k
 !
 ! Work space
 !
 type(pp_indexes)   :: px
 integer            :: ii,iv1,iv2,ik,ifft,offt,ir,is,rho_sym
 real(SP)           :: V_nm1(fft_size),V_io(fft_size),S(fft_size),reached_treshold,C,&
      &                av_rho,rs
 complex(SP),allocatable :: Phi(:,:,:)
 real(DP),allocatable :: S_unsym(:)
 logical             :: lerr
 complex(SP) C_mn
 real Diff_mn, GaugeShift
 character(schlen)  ::dumb_ch
 !
 av_rho = sum(rho)/fft_size
 rs = 3._DP/4._DP/pi/av_rho**(1._DP/3._DP)
 C = 1.92_SP*rs**2
 ! print*, C
 !
 call live_time_msg("n","","","%s")
 V_io = Voep
 allocate(Phi(nkibz,n_met_bands,fft_size),S_unsym(fft_size))
 do ii = 1, OEP_steps 
   V_nm1 = V_io       
   !
   ! Update Shifts
   !
   do ik = 1, nkibz
     do iv1 = 1, E%nbm
       Phi(ik,iv1,:) =  (0._SP,0._SP)
       do iv2 = 1, SC_bands
         Diff_mn = E%E(iv2,ik,1) -E%E(iv1,ik,1)
         if ((iv2==iv1).or.(abs(Diff_mn)<0.0001_SP)) cycle
         C_mn = (0._SP,0._SP)
         ifft = wf_state(iv1,ik,1)
         offt = wf_state(iv2,ik,1)        
         do ir = 1, fft_size
           C_mn = C_mn + U(ik,iv1,ir)/wf(ir,ifft)*wf(ir,offt)-V_io(ir)*&
                &wf(ir,offt)*conjg(wf(ir,ifft))
         enddo
         Phi(ik,iv1,:) = Phi(ik,iv1,:) + conjg(wf(:,offt))*C_mn/Diff_mn
       end do
     end do
   end do
   !
   ! Calculate Convergence Condition
   !
   S_unsym = 0._SP
   do ik =1, nkibz
     do iv1 = 1, E%nbm
       ifft = wf_state(iv1,ik,1)
       S_unsym(:) = S_unsym(:)+ k%weights(ik)*real(Phi(ik,iv1,:)*wf(:,ifft),SP)
     end do
   end do
   S = 0._SP
   rho_sym = nsym/(i_time_rev+1)
   do is=1,rho_sym
     S(:) = S(:) + S_unsym(fft_rot_r(is,:))/real(nsym,SP)
   enddo
   S = S*(1+i_time_rev)
   reached_treshold=maxval(abs(S(1:)))
   dumb_ch='[OEP] Cycle '//trim(intc(ii))//' Max error :'//&
        &            trim(real2ch(reached_treshold))
   call live_time_msg("r",dumb_ch,"","%s")
   !
!   if (reached_treshold<OEP_threshold) exit !It does not seem the problem
!   if (reached_treshold<0.000001) exit
!   V_io = V_nm1 + C*S !very slow conv...
   V_io = V_nm1*(1._SP - S/(2._SP*rho))
   !
   ! Calculating the addictive constant to fix the Gauge
   !
   GaugeShift = 0._SP !Tested it is not the problem
   do ik = 1, nkibz
     ifft =  wf_state(E%nbm,ik,1)
     do ir = 1, fft_size
       GaugeShift = GaugeShift + k%weights(ik)*(real(U(ik,E%nbm,ir))-&
&                   wf(ir,ifft)*conjg(wf(ir,ifft))*V_io(ir))
     end do
   end do
  V_io = V_io + GaugeShift
   
  lerr=(ii==OEP_steps)
 enddo
 !
 deallocate(S_unsym,Phi)
! if (lerr) call error(' Procedure to calculate OEP did not converge')
 Voep=V_io
 !
 end subroutine OEP_exact_iterative
