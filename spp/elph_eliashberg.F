!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine elph_eliashberg(k,en,q)
 !
 use pars,                ONLY:SP,schlen,pi,HA2GHZ,HA2THZ,HARTREE
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp
 use SPP,                 ONLY:elph_steps,elph_Ef,elph_gamma_broad,&
&                              elph_broad
 use ELPH,                ONLY:ph_modes,elph_nb,elph_gkkp,ph_freqs_sq,&
&                              max_ph_freq,elph_nq,elph_use_q_grid,&
&                              E_k_plus_q,setup_k_plus_q_levels
 use IO_m,                ONLY:io_control,OP_RD,VERIFY,RD_CL_IF_END,DUMP
 use timing,              ONLY:live_timing
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 use functions,           ONLY:Fermi_fnc_derivative
 implicit none
 type(levels) ::en
 type(bz_samp)::k,q
 !
 ! Work Space 
 !
 integer           ::i1,i2,im,iq,nq_todo
 real(SP)          ::dos_ef,Ef_diff,E,q_weight,&
&                    aF(elph_steps),aF_energy(elph_steps),omega,e_step
 character(schlen) ::o_file_name,ch
 type(pp_indexes)  ::px
 real(SP), allocatable :: gamma(:,:)
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::ioELPH
 !
 if (elph_gamma_broad<1.E-8) return
 !
 call section("*","== Electron-Phonon Interface: Eliashberg Function ==")
 !-----------------------------------------------------------------------
 !
 Ef_diff=0.
 if (elph_Ef/=0.) Ef_diff=en%Efermi(1)-elph_Ef
 !
 ! DOS @ Ef
 !
 dos_ef=0.
 do i1=1,k%nibz
   do i2=1,en%nb
     !
     E=en%E(i2,i1,1)+Ef_diff
     !             SPIN
     !             |
     dos_ef=dos_ef+2.*k%weights(i1)*Fermi_fnc_derivative(E,elph_gamma_broad)
   enddo
 enddo
 !
 ! zeroing
 !
 aF=0.
 !
 ! Energy range
 !
 e_step=max_ph_freq*1.10/(elph_steps-1)
 do i2=1,elph_steps
   aF_energy(i2)=(i2-1)*e_step
 enddo
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID)
 !
 call msg('s',':: Checking database ...')
 if (io_err<0) then
   call msg('l','not found')
   return
 endif
 call msg('l','sane')
 !
 ! Eliashberg Function
 !
 nq_todo=elph_nq
 if (elph_use_q_grid) nq_todo=q%nibz
 allocate(gamma(nq_todo,ph_modes))
 gamma=0.
 !
 call par_indexes(px,(/nq_todo/))
 call live_timing('Eliashberg',px%stps(myid+1))
 !
 ! calculate q%weights
 !
 call k_expand(q) 
 !
 do iq=1,nq_todo 
   !
   ! I/O
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID)  
   !
   if (.not.elph_use_q_grid) call setup_k_plus_q_levels(en%Efermi(1))
   !
   ! Q weight
   !
   q_weight=1./real(elph_nq)
   if (elph_use_q_grid) q_weight=q%weights(iq)
   !
   if (.not.px%i1p(iq)) cycle
   !
   if (io_err/=0) then
     call live_timing(steps=1)
     cycle
   endif
   !
   ! Gamma Factors
   !
   call elph_gamma(iq)
   !
   do im=1,ph_modes
     omega = sqrt( MAX( ph_freqs_sq(iq,im),0. ) )
     if (omega<1.E-10) cycle
     do i2=1,elph_steps
       aF(i2)=aF(i2)+gamma(iq,im)*q_weight*&
&             Fermi_fnc_derivative(aF_energy(i2)-omega,elph_broad)/omega/dos_ef
     enddo
   enddo
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 call pp_redux_wait(gamma)
 call pp_redux_wait(aF)
 !
 ! Output file
 !
 o_file_name='eliashberg'
 call of_open_close(o_file_name,'ot')
 call msg('o eli','#',' Eliashberg Function & Gamma factors',INDENT=0)
 call msg('o eli','#','',INDENT=0)
 call msg('o eli',         '#  Bands              :',elph_nb,INDENT=0)
 call msg('o eli','#','',INDENT=0)
 do iq=1,nq_todo
   write (ch,'(a,i6.6,a)') '# Gamma (',iq,') [GHz]:'
   call msg('o eli',trim(ch),(/gamma(iq,:)*HA2GHZ/) ,INDENT=0)
 enddo
 call msg('o eli','#','',INDENT=0)
 call msg('o eli','#',(/'E(THz)','a_F(w)'/),USE_TABS=.true.)
 call msg('o eli','#','',INDENT=0)
 do i1=1,elph_steps
   aF(i1)=aF(i1)/pi
   call msg('o eli','',(/aF_energy(i1)*HA2THZ,aF(i1)/),USE_TABS=.true.)
 enddo
 call of_open_close(o_file_name)
 !
 !CLEAN
 !
 deallocate(gamma)
 !
 contains
   !
   subroutine elph_gamma(iq)
   !------------------------
   !
   ! Taken from elphon.f90 (PWscf)
   !
   ! gamma = \pi \sum_k\sum_{i,j} \delta(e_{k,i}-Ef) \delta(e_{k+q,j}-Ef)
   !         | \sum_mu z(mu,nu) <psi_{k+q,j}|dvscf_q(mu)*psi_{k,i}> |^2
   !
   ! where z(mu,nu) is the mu component of normal mode nu (z = dyn)
   !
   ! gamma(nu) is the phonon linewidth of mode nu
   !
   ! The factor N(Ef)^2 that appears in most formulations of el-ph interact
   ! is absent because we sum, not average, over the Fermi surface.
   !
   ! lambda is the adimensional el-ph coupling for mode nu:
   ! lambda(nu)= gamma(nu)/(pi N(Ef) \omega_{q,nu}^2)
   !
   ! WARNING ! SPIN NOT INCLUDED HERE !
   !
   use R_lattice,     ONLY:qindx_X
   use vec_operate,   ONLY:degeneration_finder
   implicit none
   integer :: iq
   !
   !Work Space 
   !
   integer  ::iibz1,iibz2,im,ib1,ib2,first_el(ph_modes),n_of_el(ph_modes),&
&             n_deg_grp
   real(SP) ::weight,sym_gamma,Ek,Ekmq
   !
   do i1=1,k%nbz
     !
     iibz1=k%sstar(i1,1)
     if (elph_use_q_grid) iibz2=k%sstar(qindx_X(iq,i1,1),1)
     !
     do ib1=1,elph_nb
       do ib2=1,elph_nb
         !        
         Ek  =en%E(ib1,iibz1,1)+Ef_diff
         !
         if (elph_use_q_grid) Ekmq=en%E(ib2,iibz2,1)+Ef_diff
         if (.not.elph_use_q_grid) Ekmq=E_k_plus_q(ib2,i1,1)+Ef_diff
         !
         weight=Fermi_fnc_derivative(Ek,  elph_gamma_broad)*&
&               Fermi_fnc_derivative(Ekmq,elph_gamma_broad)
         !      
         weight=weight*2./real(k%nbz)
         !             |
         !             SPIN
         ! The factor 2 is provided by the sum over spins
         !
         ! Note that in SELF 
         !
         ! gamma \propto \sum_k\sum_{ib1,ib2} \delta(e_{k,ib1}-Ef) \delta(e_{k-q,ib2}-Ef)
         !         | <psi_{k-q,ib2}|dvscf_q(mu)*psi_{k,ib1}> |^2
         !
         do im=1,ph_modes
           gamma(iq,im)=gamma(iq,im)+&
  &              conjg(elph_gkkp(i1,im,ib2,ib1))*elph_gkkp(i1,im,ib2,ib1)*weight
         enddo
       enddo
     enddo
   enddo
   !
   ! Gamma factors symmetrization
   !
   call degeneration_finder(abs(ph_freqs_sq(iq,:)),ph_modes,first_el,n_of_el,&
&                           n_deg_grp,1.E-10)
   do i1=1,n_deg_grp
     !
     sym_gamma=0._SP
     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
       sym_gamma=sym_gamma+gamma(iq,i2)/real( n_of_el(i1) )
     enddo
     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
       gamma(iq,i2)=sym_gamma
     enddo
     !
   enddo
   !
   ! the factor 2 comes from the factor sqrt(hbar/2/M/omega) that appears
   ! in the definition of the electron-phonon matrix element g
   ! The sqrt(1/M) factor is actually hidden into the normal modes
   !
   gamma(iq,:)=gamma(iq,:)*pi/2.
   !
   end subroutine
   !
end subroutine
