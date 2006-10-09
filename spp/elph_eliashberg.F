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
!===================================
 use pars,                ONLY:SP,schlen,pi,HA2GHZ,HA2THZ
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,nqibz
 use SPP,                 ONLY:elph_steps,elph_Ef,elph_gamma_broad,&
&                              elph_Emax,elph_eli_broad
 use ELPH,                ONLY:ph_modes,elph_bands,elph_dV,ph_freqs_sq
 use IO_m,                ONLY:io_control,OP_RD,VERIFY,RD_CL_IF_END,DUMP
 implicit none
 type(levels) ::en
 type(bz_samp)::k,q
!
!ws
 integer           ::i1,i2,im,iq
 real(SP)          ::dFfermi,dos_ef,gamma(nqibz,ph_modes),&
&                    aF(elph_steps),aF_energy(elph_steps),omega,estep
 character(schlen) ::o_file_name,ch
!
!I/O
!
 integer           ::io_err,ID
 integer, external ::ioELPH
!
 call section("*","== Electron-Phonon Interface: Eliashberg Function ==")
!------------------------------------------------------------------------
!
 if (elph_Ef/=0.) elph_Ef=en%Efermi(1)-elph_Ef
!
! DOS @ Ef
!==========
 dos_ef=0.
 do i1=1,k%nibz
   do i2=1,en%nb
!                  SPIN
     dos_ef=dos_ef+2.*k%weights(i1)*dFfermi(en%E(i2,i1,1)+elph_Ef,elph_gamma_broad)
   enddo
 enddo
!
! zeroing
!=========
 aF=0.
 aF_energy=0.
 estep=elph_Emax/(elph_steps-1)
!
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID)
 call msg('s',':: Checking databases ...')
 if (io_err<0) then
   call msg('l','incorrect')
   return
 endif
 call msg('l','sane')
!
! Eliashberg Function
!=====================
 call k_expand(q) ! need q%weights
 do iq=1,nqibz 
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID)
   call elph_gamma(iq)
   do im=1,ph_modes
     omega = sqrt( MAX( ph_freqs_sq(iq,im),0. ) )
     if (omega<1.E-10) cycle
     do i2=1,elph_steps
       aF_energy(i2)=(i2-1)*estep
       aF(i2)=aF(i2)+gamma(iq,im)*q%weights(iq)*&
&             dFfermi(aF_energy(i2)-omega,elph_eli_broad)/omega/dos_ef
     enddo
   enddo
 enddo
!
! Output file
!=====================
!
 o_file_name='eliashberg'
 call of_open_close(o_file_name,'ot')
 call msg('o eli','#',' Eliashberg Function & Gamma factors',INDENT=0)
 call msg('o eli','#','',INDENT=0)
 call msg('o eli',         '#  Bands              :',elph_bands,INDENT=0)
 call msg('o eli','#','',INDENT=0)
 do iq=1,nqibz
   write (ch,'(a,i3.3,a)') '# Gamma (',iq,') [GHz]:'
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
 contains

  subroutine elph_gamma(iq)
!
! Taken from elphon.f90 (PWscf)
!
! the factor 2 comes from the factor sqrt(hbar/2/M/omega) that appears
! in the definition of the electron-phonon matrix element g
! The sqrt(1/M) factor is actually hidden into the normal modes
!
! gamma = \pi \sum_k\sum_{i,j} \delta(e_{k,i}-Ef) \delta(e_{k+q,j}-Ef)
!         | \sum_mu z(mu,nu) <psi_{k+q,j}|dvscf_q(mu)*psi_{k,i}> |^2
! where z(mu,nu) is the mu component of normal mode nu (z = dyn)
! gamma(nu) is the phonon linewidth of mode nu
!
! The factor N(Ef)^2 that appears in most formulations of el-ph interact
! is absent because we sum, not average, over the Fermi surface.
! The factor 2 is provided by the sum over spins
!
! lambda is the adimensional el-ph coupling for mode nu:
! lambda(nu)= gamma(nu)/(pi N(Ef) \omega_{q,nu}^2)
!
! WARNING ! SPIN NOT INCLUDED HERE !
!
  use R_lattice,   ONLY:qindx_X
  implicit none
  integer :: iq
!ws 
  integer  ::iibz1,iibz2,im,ib1,ib2
  real(SP) ::weight
 
  gamma(iq,:)=0.
  do i1=1,k%nbz
    iibz1=k%sstar(i1,1)
    iibz2=k%sstar(qindx_X(iq,i1,1),1)
    do ib1=elph_bands(1),elph_bands(2)
      do ib2=elph_bands(1),elph_bands(2)
        weight=dFfermi(en%E(ib1,iibz1,1)+elph_Ef,elph_gamma_broad)*&
 &             dFfermi(en%E(ib2,iibz2,1)+elph_Ef,elph_gamma_broad)*2./real(k%nbz)
!                                                                  |
!                                                                  SPIN
        do im=1,ph_modes
          gamma(iq,im)=gamma(iq,im)+&
 &              conjg(elph_dV(i1,im,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1))*&
 &                    elph_dV(i1,im,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1) *weight
        enddo
      enddo
    enddo
  enddo
  gamma(iq,:)=gamma(iq,:)*pi/2.

  end subroutine
!
 end subroutine
!

