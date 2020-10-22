!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine OEP_iterative(E,k,Voep,Vslt,U,rho)
 !
 ! Iteratively calculate the OEP potential (or one of its approximations) 
 ! see PRA 80, 052510 (2009)/PRB 68, 03510 (2003)
 !
 ! v_x = v_kli = v_hole + \sum_i^N P_{ii} D_{ii}/\rho 
 ! v_x = v_ced = v_kli + \sum_{i,j\=i}^N P_{ij} D_{ij}/\rho 
 ! v_x = v_wkli = v_whole + \sum_i^N \tilde P_{ii} D_{ii}/\tilde\rho 
 ! v_x = v_wced = v_kli + \sum_{i,j\=i}^N \tilde P_{ij} D_{ij}/\tilde\rho 
 !
 !
 ! P_{ij} = (\phi_i)^*\phi_j, D_{ij} = \langle \phi_i |  v_x - v_x^{NL} | \phi_j\rangle 
 ! \tilde P_{ij} = (\phi_i)^*\phi_j
 !
 ! where   v_x^{NL} is the nonlocal(state-dependent) exchange potential
 !
 use pars,            ONLY:SP,lchlen
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,nkibz
 use D_lattice,       ONLY:nsym
 use FFT_m,           ONLY:fft_size,fft_rot_r
 use wave_func,       ONLY:WF
 use SC,              ONLY:OEP_steps,OEP_threshold,SC_bands,l_oep_EWeight
 use drivers,         ONLY:l_oep_exact,l_oep_kli,l_oep_ceda
 use LIVE_t,          ONLY:LIVE_message
 use stderr,          ONLY:intc,real2ch,log_as_a_file
 !
 ! I/O
 !
 implicit none
 real(SP),  intent(inout) :: Voep(fft_size)
 real(SP),  intent(in)    :: Vslt(fft_size),rho(fft_size)
 complex(SP), intent(in)  :: U(nkibz,n_met_bands,fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k
 !
 ! Work space
 !
 integer          ::ii,iv1,iv2,ik,ir,ifft,offt,is,max_bands,ik_max
 real(SP)         ::V_io(fft_size),V_nm1(fft_size)
 real(SP)         ::V_resp(fft_size),reached_treshold,EWeight,ev_max
 complex(SP)      ::DeltaEps
 logical          ::lerr
 character(lchlen)  ::dumb_ch, timing_str
 !
 ! Find top of valence
 !
 ik_max = 0
 ev_max = -1000._SP
 do ik = 1, nkibz 
   if (E%E(E%nbf,ik,1).gt.ev_max) then
     ev_max = E%E(E%nbf,ik,1)
     ik_max = ik
   endif
 enddo
 !
 ! Set values depending on approximation level
 !
 if (l_oep_kli) then
   timing_str='[KLI] Cycle'
   max_bands =  E%nbf 
 elseif (l_oep_ceda) then
   timing_str='[CED] Cycle'
   max_bands = E%nbf 
 elseif (l_oep_exact) then
   timing_str='[OEP] Cycle'
   max_bands = SC_bands(2)
 endif
 EWeight = 1._SP
 !
 call LIVE_message("","n","","%s")
 V_io = Voep
 lerr = .false.
 do ii=1,OEP_steps
   V_nm1 = V_io
     V_resp = 0._SP
     do ik = 1, nkibz 
       do iv1 = 1, E%nbf 
         ifft = WF%index(iv1,ik,1)
         do iv2 = 1, max_bands
           if  (l_oep_kli.and.(iv1.ne.iv2)) cycle
           if  ((iv1.eq.iv2).and.(ik.eq.ik_max).and.&
                &((iv2.eq.E%nbf).or.(abs(E%E(iv2,ik_max,1)-E%E(E%nbf,ik_max,1)).lt.1.e-4))) cycle 
           offt = WF%index(iv2,ik,1)
           DeltaEps = 0._SP
           do ir = 1, fft_size
             DeltaEps = DeltaEps + conjg(WF%c(ir,1,offt))*WF%c(ir,1,ifft)*V_nm1(ir)-&
&             U(ik,iv2,ir)*conjg(WF%c(ir,1,ifft))
           end do
           if (l_oep_EWeight) EWeight = E%E(iv1,ik,1)+ E%E_Fermi 
           if ((l_oep_EWeight).and.(iv2.gt.E%nbf)) &
           &EWeight = EWeight*((E%E(iv2,ik,1)-E%E(iv1,ik,1))/(E%E(iv2,ik,1)+ E%E_Fermi))    
           V_resp(:) = V_resp(:) + E%f(iv1,ik,1)*k%weights(ik)*real((DeltaEps)*&
                &conjg(WF%c(:,1,ifft))*WF%c(:,1,offt))/EWeight
         end do
       end do
     end do
     V_io = 0._SP
     do is=1,nsym
       V_io(:)=V_io(:)+real(V_resp(fft_rot_r(:,is)),SP)/real(nsym,SP)
     enddo
     V_io = V_io/rho
     !
     reached_treshold=maxval(abs(V_io(1:)-V_nm1(1:)))
     dumb_ch=trim(timing_str)//' '//trim(intc(ii))//' Accuracy '//&
&            trim(real2ch(reached_treshold))//'|'//trim(real2ch(OEP_threshold))
     if (     log_as_a_file) call LIVE_message("r",dumb_ch,"","%s")
     if (.not.log_as_a_file) call LIVE_message("n",dumb_ch,"","%s")
   if (reached_treshold<(OEP_threshold)) exit
   lerr=(ii==OEP_steps)
 end do
 if (lerr) call warning('Procedure to calculate potential did not converge')
 Voep=V_io
 !
end subroutine OEP_iterative

