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
! PARTICULAR PURPOSE. See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine ELPH_alloc(what,GKKP,Nb,Nm,Nk,Nq_mem)
 !
 use pars,          ONLY:rZERO,cZERO
 use D_lattice,     ONLY:n_atoms
 use R_lattice,     ONLY:nkbz
 use electrons,     ONLY:E_reset
 use QP_m,          ONLY:QP_n_states
 use ELPH,          ONLY:GKKP_me,PH_pol_vector,ph_modes,PH_kpt_bz,PH_freqs_sq,&
&                        PH_acoustic_speed,PH_acoustic_branch,gsqF_life_f,gsqF_life_bose,gsqF_fan,&
&                        gsqF_ca_corr,gsqF_dw,PH_Q_modulus,PH_qpt,elph_nb,elph_nQ,eval_G_using_KK,&
&                        gsqF_energy_steps,elph_nk_bz
 use parallel_m,    ONLY:PAR_nQ_bz
 !
#include<memory.h>
 !
 character(*)            ::what
 type(GKKP_me), optional ::GKKP
 integer,       optional ::Nb(2),Nk,Nm(2),Nq_mem
 !
 ! Work Space
 !
 integer :: iq
 !
 if (present(GKKP)) then
   GKKP%Nq_mem=1
   GKKP%Nb=(/1,elph_nb/)
   GKKP%Nmodes=(/1,ph_modes/)
   GKKP%Nk=nkbz
   if (present(Nq_mem)) then
     GKKP%Nq_mem=Nq_mem
   endif
   if (present(Nb)) then
     GKKP%Nb=Nb
   endif
   if (present(Nm)) then
     GKKP%Nmodes=Nm
   endif
   if (present(Nk)) then
     GKKP%Nk=Nk
   endif
 endif
 !
 select case (trim(what))
   !
   case ('DW')
     GKKP%KIND="DW"
     YAMBO_ALLOC(GKKP%dVr,(GKKP%Nmodes(1):GKKP%Nmodes(2),GKKP%Nb(1):GKKP%Nb(2),GKKP%Nb(1):GKKP%Nb(2),GKKP%Nk,GKKP%Nq_mem))
     GKKP%dVr=rZERO
     !
   case ('dV','dV_bare')
     GKKP%KIND=trim(what)
     YAMBO_ALLOC(GKKP%dVc,(GKKP%Nmodes(1):GKKP%Nmodes(2),GKKP%Nb(1):GKKP%Nb(2),GKKP%Nb(1):GKKP%Nb(2),GKKP%Nk,GKKP%Nq_mem))
     GKKP%dVc=cZERO
     allocate(GKKP%E_kpq(elph_nQ))
     do iq=1,elph_nQ
       YAMBO_ALLOC(GKKP%E_kpq(iq)%E,(elph_nb,nkbz,1))
       YAMBO_ALLOC(GKKP%E_kpq(iq)%f,(elph_nb,nkbz,1))
     enddo
   case ('gFsq')
     !
     if (.not.allocated(gsqF_fan)) then
       if (.not.eval_G_using_KK) then
         YAMBO_ALLOC(gsqF_fan,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
         YAMBO_ALLOC(gsqF_ca_corr,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
         gsqF_fan=rZERO
         gsqF_ca_corr=rZERO
       endif
       YAMBO_ALLOC(gsqF_dw,(QP_n_states,PAR_nQ_bz,ph_modes))
       YAMBO_ALLOC(gsqF_life_bose,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
       YAMBO_ALLOC(gsqF_life_f,(QP_n_states,PAR_nQ_bz,ph_modes,gsqF_energy_steps))
       gsqF_dw=rZERO
       gsqF_life_bose=rZERO
       gsqF_life_f=rZERO
     endif
     !
   case ('PHONONS')
     !
     if (.not.allocated(PH_freqs_sq)) then
       !
       YAMBO_ALLOC(PH_freqs_sq,(elph_nQ,ph_modes))
       YAMBO_ALLOC(PH_pol_vector,(ph_modes,n_atoms,3,elph_nQ))
       YAMBO_ALLOC(PH_qpt,(elph_nQ,3))
       YAMBO_ALLOC(PH_kpt_bz,(elph_nk_bz,3))
       YAMBO_ALLOC(PH_Q_modulus,(elph_nQ))
       YAMBO_ALLOC(PH_acoustic_speed,(elph_nQ,ph_modes))
       YAMBO_ALLOC(PH_acoustic_branch,(ph_modes))
       PH_pol_vector=rZERO
       PH_qpt=rZERO
       PH_freqs_sq=rZERO
       PH_Q_modulus=rZERO
       PH_acoustic_speed=rZERO
       PH_acoustic_branch=.FALSE.
       !
     endif
     !
   case ('FREE')
     if (.not.present(GKKP)) then
       YAMBO_FREE(gsqF_fan)
       YAMBO_FREE(gsqF_dw)
       YAMBO_FREE(gsqF_ca_corr)
       YAMBO_FREE(gsqF_life_bose)
       YAMBO_FREE(gsqF_life_f)
       !
       YAMBO_FREE(PH_Q_modulus)
       YAMBO_FREE(PH_acoustic_speed)
       YAMBO_FREE(PH_acoustic_branch)
       !
       YAMBO_FREE(PH_freqs_sq)
       YAMBO_FREE(PH_pol_vector)
       YAMBO_FREE(PH_qpt)
       YAMBO_FREE(PH_kpt_bz)
       !
     else
       YAMBO_FREE(GKKP%dVr)
       YAMBO_FREE(GKKP%dVc)
       if (.not.allocated(GKKP%E_kpq)) return
       do iq=1,elph_nQ
         call E_reset( GKKP%E_kpq(iq) )
       enddo
     endif
     !
 end select
 !
end subroutine
