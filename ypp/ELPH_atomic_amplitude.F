!
! Copyright (C) 2000-2010 A. Marini, E. Cannuccia and the YAMBO team 
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
subroutine ELPH_atomic_amplitude(q)
 !
 use pars,                ONLY:SP
 use units,               ONLY:BOHR
 use com,                 ONLY:msg,of_open_close,warning
 use R_lattice,           ONLY:bz_samp
 use D_lattice,           ONLY:n_atoms_species,Z_species,n_atoms
 use ELPH,                ONLY:ph_modes,ph_freqs_sq,&
&                              max_ph_freq,elph_nDBs,elph_use_q_grid,&
&                              elph_global_free,pol_vector
 use IO_m,                ONLY:io_control,OP_RD,RD_CL_IF_END,DUMP,RD_CL
 use stderr,              ONLY:set_real_printed_length
 use functions,           ONLY:bose_f
 use matrix_operate,      ONLY:mat_dia_inv,DIAGO,USE_LK
 implicit none
 type(bz_samp)::q
 !
 ! Work Space 
 !
 integer           ::nq_to_sum,iq,im,is,i,j
 real(SP)          ::q_weight,omega,U_eigenvalues(3)
 complex(SP)       ::mean_ui_uj(n_atoms,3,3)
 !
 !TEMP>
 character(1), parameter :: atom_name(4)=(/'C','C','H','H'/)
 !TEMP<
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::ioELPH
 !
 call section("*","== Electron-Phonon Interface: mean atomic amplitudes ==")
 !---------------------------------------------------------------------------
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID,'no_gkkp')
 !
 call msg('s',':: Checking database ...')
 if (io_err<0) then
   call msg('l','not found')
   return
 endif
 call msg('l','sane')
 !
 nq_to_sum=elph_nDBs
 if (elph_use_q_grid) nq_to_sum=q%nibz
 !
 ! calculate q%weights
 !
 call k_expand(q) 
 !
 mean_ui_uj=(0.,0.)
 !
 do iq=1,nq_to_sum 
   !
   ! I/O
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID,'no_gkkp')  
   !
   if (.not.allocated(pol_vector)) then
     call warning('Polarization vectors not found!')
     call elph_global_free()
     return
   endif
   !
   ! Q weight
   !
   q_weight=1./real(elph_nDBs)
   if (elph_use_q_grid) q_weight=q%weights(iq)
   !
   do im=1,ph_modes
     !
     omega = sqrt( MAX( ph_freqs_sq(iq,im),0. ) )
     !
     if (omega<1.E-10) cycle
     !
     do is=1,n_atoms
       do i=1,3
         do j=1,3
           mean_ui_uj(is,i,j)=mean_ui_uj(is,i,j)+q_weight*1./2./omega*&
&                             conjg(pol_vector(im,is,i))*pol_vector(im,is,j)*&
&                             (bose_f(omega)+1.)
           mean_ui_uj(is,i,j)=real ( mean_ui_uj(is,i,j) )
         enddo
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 do is=1,n_atoms
   !
   call mat_dia_inv(DIAGO,USE_LK,mean_ui_uj(is,:,:),E_real=U_eigenvalues)
   !
   call msg('s','Atom '//trim(atom_name(is)))
   call msg('s','Displacement  [a.u.]/A #1: ',sqrt(U_eigenvalues(1))*(/1.,BOHR/) )
   call msg('s','Normalized direction   #1: ',real(mean_ui_uj(is,:,1)))
   call msg('s','                       #2: ', sqrt(U_eigenvalues(2))*(/1.,BOHR/) )
   call msg('s','                       #2: ',real(mean_ui_uj(is,:,2)))
   call msg('s','                       #3: ', sqrt(U_eigenvalues(3))*(/1.,BOHR/) )
   call msg('s','                       #3: ',real(mean_ui_uj(is,:,3)))
   !
 enddo
 !
 !CLEAN
 !
 call elph_global_free()
 !
end subroutine
