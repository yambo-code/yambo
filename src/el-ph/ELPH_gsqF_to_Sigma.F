!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine ELPH_gsqF_to_Sigma(en,q,l_GF_from_CA,Sc_W)
 !
 use pars,          ONLY:SP,DP,pi
 use functions,     ONLY:bose_f
 use units,         ONLY:HA2EV
 use LIVE_t,        ONLY:live_timing
 use R_lattice,     ONLY:nqbz
 use ELPH,          ONLY:gsqF_energy_steps,gsqF_energy_steps,gsqF_ca_corr,gsqF_fan,gsqF_life_f,&
&                        gsqF_life_bose,gsqF_dw,elph_branches,elph_nDBs_used,&
&                        elph_use_q_grid,ph_freqs_sq,QP_DW,QP_OMS_Fan,elph_use_q_grid
 use QP_m,          ONLY:QP_solver,QP_Sc_steps,QP_Sc_steps,QP_n_states,QP_Sc,On_Mass_Shell_approx,&
&                        QP_table
 use parallel_m,    ONLY:PAR_nQ_bz,PAR_nQP,PAR_IND_QP,PAR_Q_bz_index,PAR_IND_Q_bz,PAR_COM_G_b_INDEX
 use parallel_int,  ONLY:PP_redux_wait
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use frequency,     ONLY:w_samp,W_reset
 !
 implicit none
 !
 type(levels)  ::en
 type(bz_samp) ::q
 logical       ::l_GF_from_CA
 type(w_samp)  ::Sc_W(QP_n_states)
 !
 ! Work Space
 !
 integer            ::i_qp,iw,il,iq_loop,iq_mem,ib,ik,nq
 type(w_samp)       ::Sc_local_W
 complex(DP)        ::DP_FAN(gsqF_energy_steps),DP_DW
 complex(SP)        ::coefficient,Sc_local(gsqF_energy_steps)
 real(SP)           ::ph_freq
 !
 call ELPH_simmetrize_and_distribute_gsqF(en)
 !
 nq=nqbz
 if (.not.elph_use_q_grid) nq=elph_nDBs_used
 !
 if (trim(QP_solver)=='n') call live_timing('El-Ph Sc   [sum]',PAR_nQ_bz*PAR_nQP)
 if (trim(QP_solver)=='g') call live_timing('El-Ph Sc[sum+KK]',PAR_nQ_bz*PAR_nQP+PAR_nQP)
 !
 ! Setup the pre-coefficient for the lifetimes related gsqF. In case
 ! the QP corrections are required these are stored in the immaginary part of DP_FAN.
 ! If the self-energy has to be plotted, instead, the contribution from the
 ! lifetimes-related gsqF must be real (spectral function).
 !
 coefficient=(0.,1.)
 !
 ! Setup energy range for Sc/G plotting (I)
 !
 if ( trim(QP_solver)=='g' ) then
   coefficient=(1.,0.)
   call W_reset(Sc_local_W)
   Sc_local_W%n_freqs =QP_Sc_steps
 endif
 !
 do i_qp=1,QP_n_states   
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   ! Setup energy range for Sc/G plotting (II)
   !
   if ( trim(QP_solver)=='g' ) then
     call FREQUENCIES_Green_Function(i_qp,Sc_local_W,en%E,.FALSE.)
     !
     ! Overwrites the damping defined in FREQUENCIES_Green_Function
     !
     forall(iw=1:QP_Sc_steps) Sc_local_W%p(iw)= real( Sc_local_W%p(iw)) + cmplx(0.,0.00001/HA2EV)
   endif
   !
   ib   =QP_table(i_qp,1)
   ik   =QP_table(i_qp,3)
   !
   DP_FAN=cmplx(0.,0.,DP)
   DP_DW =cmplx(0.,0.,DP)
   !
   do iq_loop=1,nq
     !
     if (.not.PAR_IND_Q_bz%element_1D(iq_loop)) cycle
     iq_mem=PAR_Q_bz_index(iq_loop)
     !
     do il=elph_branches(1),elph_branches(2)
       !
       if (.not.elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq(iq_loop,il))
       if (     elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq( q%sstar(iq_loop,1) ,il))
       !
       do iw=1,gsqF_energy_steps
         !
         ! QP lifetimes/Green's function
         !-------------------------------
         if (l_GF_from_CA.and.trim(QP_solver)=='g') then
           DP_FAN(iw)=DP_FAN(iw)+(gsqF_fan(i_qp,iq_mem,il,iw)*(bose_f(ph_freq)+1.)/2.+&
&                                 gsqF_ca_corr(i_qp,iq_mem,il,iw))/real( PAR_COM_G_b_INDEX%n_CPU)
         else
           DP_FAN(iw)=DP_FAN(iw)+coefficient*(gsqF_life_bose(i_qp,iq_mem,il,iw)*(bose_f(ph_freq)/2+1.)/2.+&
&                                             gsqF_life_f(i_qp,iq_mem,il,iw))/real( PAR_COM_G_b_INDEX%n_CPU)
         endif
         !
         if ( trim(QP_solver)=='n' )  then
           !
           ! Integrate g^2 F function (QP correction).
           !
           ! QP energies: FAN+correction
           !-----------------------------
           ! 
           DP_FAN(iw)=DP_FAN(iw)+(gsqF_fan(i_qp,iq_mem,il,iw)*(bose_f(ph_freq)+1.)/2.+&
&                                 gsqF_ca_corr(i_qp,iq_mem,il,iw))/real( PAR_COM_G_b_INDEX%n_CPU)
           !
         endif
         !
       enddo
       !
       ! Integrate g^2 F function (QP correction & spectral function).
       !
       ! QP energies : DW
       !------------------
       !
       DP_DW=DP_DW+gsqF_dw(i_qp,iq_mem,il)*(bose_f(ph_freq)+1.)/2./real( PAR_COM_G_b_INDEX%n_CPU)
       ! 
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
   if ( trim(QP_solver)=='n' )  then
     !
     QP_DW(i_qp)=QP_DW(i_qp)+DP_DW
     QP_OMS_Fan(i_qp)=QP_OMS_Fan(i_qp)+DP_FAN(1)
     !
     if (On_Mass_Shell_approx) then
       QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+DP_FAN(1)+DP_DW
     else
       QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+DP_FAN(:)+DP_DW
     endif
     !
   else if ( trim(QP_solver)=='g' )  then
     !
     if (.not.l_GF_from_CA) then
       !
       Sc_local=(0.,0.)
       !
       call Kramers_Kronig(cmplx(DP_FAN(:))/pi,real(Sc_W(i_qp)%p) ,QP_Sc_steps,&
&                          Sc_local           ,conjg(Sc_local_W%p),QP_Sc_steps,(0.,0.))
       !
       QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+Sc_local(:)+DP_DW
       !
     else
       !
       QP_Sc(i_qp,:)=QP_Sc(i_qp,:)+DP_FAN(:)+DP_DW 
       !
     endif
     !
     call live_timing(steps=1)
     !
   endif
   !
 enddo
 !
 call PP_redux_wait(QP_DW)
 call PP_redux_wait(QP_OMS_Fan)
 call PP_redux_wait(QP_Sc)
 !
 call live_timing()
 !
 ! CLEAN
 !
 call W_reset(Sc_local_W)
 !
end subroutine
