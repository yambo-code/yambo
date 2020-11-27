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
subroutine ELPH_simmetrize_and_distribute_gsqF(en)
 !
 use electrons,     ONLY:levels
 use QP_m,          ONLY:QP_states_simmetrize
 use R_lattice,     ONLY:nqbz
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_COM_Q_A2A,PAR_COM_Q_A2A,&
&                        PAR_Q_bz_index,PAR_IND_Q_bz
 use ELPH,          ONLY:gsqF_energy_steps,gsqF_ca_corr,gsqF_fan,gsqF_life_f,gsqF_life_bose,&
&                        gsqF_dw,eval_G_using_KK,elph_branches,elph_nQ_used,elph_use_q_grid
 !
 implicit none
 type(levels)  ::en
 !
 ! Work Space
 !
 integer            ::iw,iq_loop,iq_mem,il,nq
 !
 nq=nqbz
 if (.not.elph_use_q_grid) nq=elph_nQ_used
 !
 ! Degenerate bands average
 !
 do iq_loop=1,nq
   !
   if (.not.PAR_IND_Q_bz%element_1D(iq_loop)) cycle
   iq_mem=PAR_Q_bz_index(iq_loop)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     call PP_redux_wait(gsqF_dw(:,iq_mem,il),COMM=PAR_COM_Q_A2A%COMM)
     call QP_states_simmetrize(en,V_real=gsqF_dw(:,iq_mem,il))
     !
     do iw=1,gsqF_energy_steps
       !
       call PP_redux_wait(gsqF_life_bose(:,iq_mem,il,iw),COMM=PAR_COM_Q_A2A%COMM)
       call QP_states_simmetrize(en,V_real=gsqF_life_bose(:,iq_mem,il,iw))
       !
       call PP_redux_wait(gsqF_life_f(:,iq_mem,il,iw),COMM=PAR_COM_Q_A2A%COMM)
       call QP_states_simmetrize(en,V_real=gsqF_life_f(:,iq_mem,il,iw))
       !
       if (.not.eval_G_using_KK) then
         !
         call PP_redux_wait(gsqF_fan(:,iq_mem,il,iw),COMM=PAR_COM_Q_A2A%COMM)
         call QP_states_simmetrize(en,V_real=gsqF_fan(:,iq_mem,il,iw))
         !
         call PP_redux_wait(gsqF_ca_corr(:,iq_mem,il,iw),COMM=PAR_COM_Q_A2A%COMM)
         call QP_states_simmetrize(en,V_real=gsqF_ca_corr(:,iq_mem,il,iw))
         !
       endif
       !
     enddo
     !
   enddo
 enddo
 !
end subroutine
