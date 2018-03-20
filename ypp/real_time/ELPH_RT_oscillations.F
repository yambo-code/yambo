!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine ELPH_RT_oscillations(E)
 !
! use pars,                ONLY:SP,schlen,pi
! use units,               ONLY:HA2EV,HA2GHZ,HA2THZ 
! use com,                 ONLY:msg,of_open_close,error
 use com,                 ONLY:error
 use electrons,           ONLY:levels
 use YPP_real_time,       ONLY:RT_D_t,RT_IO_observables
!,n_met_bands,n_full_bands
! use R_lattice,           ONLY:bz_samp
! use YPP,                 ONLY:l_dos
! use YPP_ELPH,            ONLY:elph_steps,elph_Ef,elph_gamma_broad,&
!&                              ph_broad,l_eliashberg
 use ELPH,                ONLY:ph_modes
!,elph_nb,elph_gkkp,ph_freqs_sq,&
!&                              W_debye,elph_nDBs,elph_use_q_grid,&
!&                              E_k_plus_q,setup_k_plus_q_levels,elph_global_free
! use IO_m,                ONLY:io_control,OP_RD,RD_CL_IF_END,DUMP,RD_CL
  use IO_m,                ONLY:io_control,OP_RD,DUMP
  use RT_control,          ONLY:RT_IO_t,OBS_RT_IO_t

! use LIVE_t,              ONLY:live_timing
! use parallel_m,          ONLY:PP_redux_wait,PP_indexes,myid
! use interfaces,          ONLY:PARALLEL_index
! use functions,           ONLY:Fermi_fnc_derivative
! use stderr,              ONLY:set_real_printed_length
 !
#include<memory.h>
 !
 type(levels)   :: E
! type(bz_samp)::k,q
 !
 ! Work Space 
 !
! integer           ::i1,i2,im,iq,nq_todo
! real(SP)          ::dos_ef,Ef_diff,E,q_weight,aF(elph_steps),&
!&                    ph_dos(elph_steps),ph_energy(elph_steps),omega,e_step
! complex(SP)       ::d_elias_dos
! character(schlen) ::o_file_name,ch
! type(PP_indexes)  ::px
! real(SP), allocatable :: gamma(:,:)
! !
! !I/O
! !
! integer           ::io_err,ID
! integer, external ::io_ELPH
 integer           ::io_err,ID
 integer, external ::io_ELPH
 !
 call section("*","== Real-Time Phonon displacement ==")
 !
 ! I/O
 !=====
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded no_matrix_elements')
 if (io_err/=0) call error("GKKP database not found")
 !
 call RT_IO_observables_paths("D",E)
 !
 call RT_time_configuration_setup('PH_DISPLACEMENT',OBS_RT_IO_t)
 !
 call RT_IO_observables(what='D')
 !
stop
! !
! if (l_eliashberg) l_eliashberg=elph_gamma_broad>1.E-8.and.n_met_bands/=n_full_bands
! if (l_dos)        l_dos       =ph_broad>1.E-8
! !
! if (l_eliashberg.and.l_dos) then
! else if (l_eliashberg) then
!   call section("*","== Electron-Phonon Interface: Eliashberg Function ==")
! else if (l_dos) then
!   call section("*","== Electron-Phonon Interface: phonon DOS ==")
! else
!   return
! endif
! !-----------------------------------------------------------------------
! !
! Ef_diff=0.
! if (elph_Ef/=0.) Ef_diff=en%E_Fermi-elph_Ef
! !
! ! DOS @ Ef
! !
! dos_ef=0.
! do i1=1,k%nibz
!   do i2=1,en%nb
!     !
!     E=en%E(i2,i1,1)+Ef_diff
!     !             SPIN
!     !             |
!     dos_ef=dos_ef+2.*k%weights(i1)*Fermi_fnc_derivative(E,elph_gamma_broad)
!     !
!   enddo
! enddo
! !
! call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
! if (l_dos)        io_err=io_ELPH(ID,'no_gkkp')
! if (l_eliashberg) io_err=io_ELPH(ID,'gkkp')
! !
! call msg('s',':: Checking database ...')
! if (io_err<0) then
!   call msg('l','not found')
!   return
! endif
! call msg('l','sane')
! !
! ! Energy range
! !
! e_step=W_debye*1.10/(elph_steps-1)
! do i2=1,elph_steps
!   ph_energy(i2)=(i2-1)*e_step
! enddo
! !
! nq_todo=elph_nDBs
! if (elph_use_q_grid) nq_todo=q%nibz
! !
! ! Eliashberg Function & DOS function
! !====================================
! !
! aF=0.
! ph_dos=0.
! !
! call PARALLEL_index(px,(/nq_todo/))
! if (l_eliashberg.and..not.l_dos) call live_timing('Eliashberg',px%n_of_elements(myid+1))
! if (.not.l_eliashberg.and.l_dos) call live_timing('ph DOS',px%n_of_elements(myid+1))
! if (l_dos.and.l_eliashberg)      call live_timing('Eliashberg & DOS',px%n_of_elements(myid+1))
! !
! ! calculate q%weights
! !
! call k_expand(q) 
! !
! do iq=1,nq_todo 
!   !
!   ! I/O
!   !
!   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
!   if (l_dos)        io_err=io_ELPH(ID,'no_gkkp')
!   if (l_eliashberg) io_err=io_ELPH(ID,'gkkp')
!   !
!   if (.not.elph_use_q_grid.and.l_eliashberg) call setup_k_plus_q_levels(en%E_Fermi)
!   !
!   ! Q weight
!   !
!   q_weight=1./real(elph_nDBs)
!   if (elph_use_q_grid) q_weight=q%weights(iq)
!   !
!   if (.not.px%element_1D(iq)) cycle
!   !
!   if (io_err/=0) then
!     call live_timing(steps=1)
!     cycle
!   endif
!   !
!   if (l_eliashberg) then
!     !
!     ! Gamma Factors
!     !
!     call elph_gamma(iq)
!     !
!   endif
!   !
!   do im=1,ph_modes
!     !
!     omega = sqrt( MAX( ph_freqs_sq(iq,im),0. ) )
!     !
!     if (omega<1.E-10) cycle
!     !
!     do i2=1,elph_steps
!       !
!       d_elias_dos=q_weight*Fermi_fnc_derivative(ph_energy(i2)-omega,ph_broad)
!       !
!       if (l_eliashberg) aF(i2)=aF(i2)+gamma(iq,im)*d_elias_dos/omega/dos_ef
!       if (l_dos)     ph_dos(i2)=ph_dos(i2)+d_elias_dos
!       !
!     enddo
!   enddo
!   !
!   call live_timing(steps=1)
!   !
! enddo
! !
! call live_timing()
! if (l_eliashberg) then
!   call PP_redux_wait(gamma)
!   call PP_redux_wait(aF)
! endif
! if (l_dos) call PP_redux_wait(ph_dos)
! !
! ! Output file
! !
! call set_real_printed_length(f_length=20,g_length=20)
! !
! if (l_eliashberg) then
!   o_file_name='eliashberg'
!   call of_open_close(o_file_name,'ot')
!   call msg('o eli','#',' Eliashberg Function & Gamma factors',INDENT=0)
!   call msg('o eli','#','',INDENT=0)
!   call msg('o eli',         '#  Bands              :',elph_nb,INDENT=0)
!   call msg('o eli','#','',INDENT=0)
!   do iq=1,nq_todo
!     write (ch,'(a,i6.6,a)') '# Gamma (',iq,') [GHz]:'
!     call msg('o eli',trim(ch),(/gamma(iq,:)*HA2GHZ/) ,INDENT=0)
!   enddo
!   call msg('o eli','#','',INDENT=0)
!   call msg('o eli','#',(/'E(THz)','a_F(w)'/),USE_TABS=.true.)
!   call msg('o eli','#','',INDENT=0)
!   do i1=1,elph_steps
!     aF(i1)=aF(i1)/pi
!     call msg('o eli','',(/ph_energy(i1)*HA2THZ,aF(i1)/),USE_TABS=.true.)
!   enddo
!   call of_open_close(o_file_name)
! endif
! if (l_dos) then
!   o_file_name='ph_dos'
!   call of_open_close(o_file_name,'ot')
!   call msg('o dos','#',' Phonon DOS function',INDENT=0)
!   call msg('o dos','#','',INDENT=0)
!   call msg('o dos','#',(/'E(meV)','DOS(w)'/),USE_TABS=.true.)
!   call msg('o dos','#','',INDENT=0)
!   do i1=1,elph_steps
!     ph_dos(i1)=ph_dos(i1)/pi
!     call msg('o dos','',(/ph_energy(i1)*HA2EV*1000.,ph_dos(i1)/),USE_TABS=.true.)
!   enddo
!   call of_open_close(o_file_name)
! endif
! !
! call set_real_printed_length()
! !
! !CLEAN
! !
! call elph_global_free()
! !
! contains
!   !
!   subroutine elph_gamma(iq)
!   !------------------------
!   !
!   ! Taken from elphon.f90 (PWscf)
!   !
!   ! gamma = \pi \sum_k\sum_{i,j} \delta(e_{k,i}-Ef) \delta(e_{k+q,j}-Ef)
!   !         | \sum_mu z(mu,nu) <psi_{k+q,j}|dvscf_q(mu)*psi_{k,i}> |^2
!   !
!   ! where z(mu,nu) is the mu component of normal mode nu (z = dyn)
!   !
!   ! gamma(nu) is the phonon linewidth of mode nu
!   !
!   ! The factor N(Ef)^2 that appears in most formulations of el-ph interact
!   ! is absent because we sum, not average, over the Fermi surface.
!   !
!   ! lambda is the adimensional el-ph coupling for mode nu:
!   ! lambda(nu)= gamma(nu)/(pi N(Ef) \omega_{q,nu}^2)
!   !
!   ! WARNING ! SPIN NOT INCLUDED HERE !
!   !
!   use R_lattice,     ONLY:qindx_X
!   use vec_operate,   ONLY:degeneration_finder
!   implicit none
!   integer :: iq
!   !
!   !Work Space 
!   !
!   integer  ::iibz1,iibz2,im,ib1,ib2,first_el(ph_modes),n_of_el(ph_modes),&
!&             n_deg_grp
!   real(SP) ::weight,sym_gamma,Ek,Ekmq
!   !
!   do i1=1,k%nbz
!     !
!     iibz1=k%sstar(i1,1)
!     if (elph_use_q_grid) iibz2=k%sstar(qindx_X(iq,i1,1),1)
!     !
!     do ib1=1,elph_nb
!       do ib2=1,elph_nb
!         !        
!         Ek  =en%E(ib1,iibz1,1)+Ef_diff
!         !
!         if (elph_use_q_grid) Ekmq=en%E(ib2,iibz2,1)+Ef_diff
!         if (.not.elph_use_q_grid) Ekmq=E_k_plus_q(ib2,i1,1)+Ef_diff
!         !
!         weight=Fermi_fnc_derivative(Ek,  elph_gamma_broad)*&
!&               Fermi_fnc_derivative(Ekmq,elph_gamma_broad)
!         !      
!         weight=weight*2._SP/real(k%nbz)
!         !             |
!         !             SPIN
!         ! The factor 2 is provided by the sum over spins
!         !
!         ! Note that in YAMBO 
!         !
!         ! gamma \propto \sum_k\sum_{ib1,ib2} \delta(e_{k,ib1}-Ef) \delta(e_{k-q,ib2}-Ef)
!         !         | <psi_{k-q,ib2}|dvscf_q(mu)*psi_{k,ib1}> |^2
!         !
!         do im=1,ph_modes
!           gamma(iq,im)=gamma(iq,im)+&
!  &              conjg(elph_gkkp(im,ib2,ib1,i1))*elph_gkkp(im,ib2,ib1,i1)*weight
!         enddo
!       enddo
!     enddo
!   enddo
!   !
!   ! Gamma factors symmetrization
!   !
!   call degeneration_finder(abs(ph_freqs_sq(iq,:)),ph_modes,first_el,n_of_el,&
!&                           n_deg_grp,1.E-10_SP)
!   do i1=1,n_deg_grp
!     !
!     sym_gamma=0._SP
!     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
!       sym_gamma=sym_gamma+gamma(iq,i2)/real( n_of_el(i1) )
!     enddo
!     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
!       gamma(iq,i2)=sym_gamma
!     enddo
!     !
!   enddo
!   !
!   ! the factor 2 comes from the factor sqrt(hbar/2/M/omega) that appears
!   ! in the definition of the electron-phonon matrix element g
!   ! The sqrt(1/M) factor is actually hidden into the normal modes
!   !
!   gamma(iq,:)=gamma(iq,:)*pi/2.
!   !
!   end subroutine
!   !
end subroutine
