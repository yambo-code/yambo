!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine RT_PHEL_initialize(k,q)
 !
 ! Reading the gkkp m.e. with q in the ibz and k in the bz
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:Q_io_map,K_io_map,RT_bands
 use PH_real_time,   ONLY:phel_gkkp_expanded_sq
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_phel_scatt
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:io_control,REP,OP_RD_CL
 use ELPH,           ONLY:ph_freqs_sq,ph_modes,elph_gkkp,elph_nb,W_debye,MAX_gkkp
 use parser_m,       ONLY:parser
 use parallel_m,     ONLY:PAR_IND_Q_ibz,PAR_Xk_bz_index,PAR_IND_Xk_bz,PAR_Q_ibz_index,&
 &                        PAR_nQ_ibz,PAR_Xk_nbz
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,io_err,ik,iq_io,i_n,i_np,ik_mem,iq_mem
 real(SP)            ::MAX_gkkp_expanded_at_kpq(k%nibz,q%nbz)
 integer, external   ::io_ELPH
 logical             ::l_use_Debye_E,SETUP_failure
 !
 ! Logicals
 !==========
 !
 if (.not.l_phel_scatt) return
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded')
 !
 SETUP_failure=io_err/=0.or.elph_nb<RT_bands(2)
 !
 if (SETUP_failure) then
   l_phel_scatt   =.FALSE.
   call warning('ELPH database not found and/or incompatible OR QP widths not provided. E-P scattering switched off')
   return
 endif
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 !
 ! Q/K grid remapping
 !====================
 call ELPH_Q_databases_map(k,q)
 !
 ! Allocation
 !============
 YAMBO_ALLOC(phel_gkkp_expanded_sq,(ph_modes,elph_nb,elph_nb,PAR_nQ_ibz,PAR_Xk_nbz))
 !
 MAX_gkkp_expanded_at_kpq=0._SP
 !
 do iq=1,q%nibz
   !
   if (.not.PAR_IND_Q_ibz%element_1D(iq)) cycle
   !
   iq_io=Q_io_map(iq)
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq_io+1/),ID=ID)
   io_err=io_ELPH(ID,'gkkp_expanded')
   !
   do ik=1,k%nbz
     !
     if (.not.PAR_IND_Xk_bz%element_1D(ik)) cycle
     !
     ik_mem = PAR_Xk_bz_index(ik)
     iq_mem = PAR_Q_ibz_index(iq)
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         phel_gkkp_expanded_sq(:,i_n,i_np,iq_mem,ik_mem)=abs(elph_gkkp(:,i_n,i_np,K_io_map(ik_mem)))**2
         !
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 if (any(ph_freqs_sq<0._SP)) call warning('Imaginary phonon frequencies found. Please check.')
 !
 ! Test of the effect of using a single optical mode
 !
 if (l_use_Debye_E) ph_freqs_sq=W_debye**2
 !
end subroutine RT_PHEL_initialize
