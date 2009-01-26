!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine elph_general_gFsq(k,en,q)
 !
 use pars,                ONLY:SP,schlen
 use units,               ONLY:HARTREE
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 use frequency,           ONLY:w_samp,W_reset
 use YPP,                 ONLY:elph_steps,ph_broad
 use stderr,              ONLY:intc
 use ELPH,                ONLY:max_ph_freq,elph_nq_used,ph_modes,elph_global_free,&
&                              gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,&
&                              gsqF_life_f ,elph_use_q_grid,ph_freqs_sq,elph_global_alloc
 use IO_m,                ONLY:io_control,OP_RD,RD_CL,DUMP,RD_CL
 use BS,                  ONLY:BS_mat
 use QP_m,                ONLY:QP_n_states,QP_table
 use timing,              ONLY:live_timing
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset,myid
 use par_indexes_m,       ONLY:par_indexes
 use functions,           ONLY:Fermi_fnc_derivative
 implicit none
 type(levels) ::en
 type(bz_samp)::k,q
 !
 ! Work Space
 !
 integer              ::iq,il,iqp,iw
 type(w_samp)         ::g_sq_F_E
 logical, allocatable ::state_is_2do(:)
 logical              ::gFsq_is_excitonic
 real(SP)             ::delta_E,ph_freq
 real(SP),allocatable ::g_sq_F(:,:,:)
 type(pp_indexes)     ::px
 character(schlen)    ::ch
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::ioELPH
 !
 gFsq_is_excitonic=allocated(BS_mat)
 if (gFsq_is_excitonic) then
   call section("+","Excitonic Generalized Eliashberg functions")
 else
   call section("*","Generalized Eliashberg functions")
 endif
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID,'gFsq')
 !
 if (io_err/=0) return
 !
 allocate(state_is_2do(QP_n_states),QP_table(QP_n_states,3+n_sp_pol-1),&
&         g_sq_F(QP_n_states,elph_steps,5))
 call elph_global_alloc('gFsq')
 !
 call io_control(ACTION=RD_CL,SEC=(/2/),ID=ID)
 io_err=ioELPH(ID,'gFsq')
 !
 ! Resets and energy setup
 !
 call W_reset(g_sq_F_E)
 g_sq_F_E%n=elph_steps
 g_sq_F_E%er=(/0._SP,max_ph_freq*1.1/)
 g_sq_F_E%dr=ph_broad
 call freqs_setup(g_sq_F_E)
 call pp_indexes_reset(px)
 g_sq_F=0.
 !
 ! Degenerate bands average
 !
 do iq=1,elph_nq_used
   do il=1,ph_modes
     call QP_states_simmetrize(gsqF_fan(:,iq,il),en,state_is_2do)
     call QP_states_simmetrize(gsqF_dw(:,iq,il),en,state_is_2do)
     call QP_states_simmetrize(gsqF_ca_corr(:,iq,il),en,state_is_2do)
     call QP_states_simmetrize(gsqF_life_bose(:,iq,il),en,state_is_2do)
     call QP_states_simmetrize(gsqF_life_f(:,iq,il),en,state_is_2do)
   enddo
 enddo
 !
 call par_indexes(px,(/elph_nq_used,QP_n_states/))
 call pp_redux_wait
 !
 call live_timing('gF^2 [el]',px%stps(myid+1))
 !
 do iqp=1,QP_n_states  
   !
   do iq=1,elph_nq_used
     !
     if (.not.px%i2p(iq,iqp)) cycle
     !
     do il=1,ph_modes
       !
       if (.not.elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq(iq,il))
       if (     elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq( q%sstar(iq,1) ,il))
       !
       ! g^2 F function(s)
       !
       do iw=1,elph_steps
         !
         delta_E=real(g_sq_F_E%p(iw))-ph_freq
         !
         g_sq_F(iqp,iw,1)=g_sq_F(iqp,iw,1)+gsqF_fan(iqp,iq,il)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,2)=g_sq_F(iqp,iw,2)+gsqF_dw(iqp,iq,il)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,3)=g_sq_F(iqp,iw,3)+gsqF_ca_corr(iqp,iq,il)*&
&                     Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,4)=g_sq_F(iqp,iw,4)+gsqF_life_bose(iqp,iq,il)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,5)=g_sq_F(iqp,iw,5)+gsqF_life_f(iqp,iq,il)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         !
       enddo
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 call pp_redux_wait(g_sq_F(:,:,:))
 !
 do iqp=1,QP_n_states  
   !
   if (.not.state_is_2do(iqp)) cycle
   !
   ch='g_sq_F_b_'//trim(intc(QP_table(iqp,1)))//'_k_'//trim(intc(QP_table(iqp,3)))
   call of_open_close(trim(ch),'ot')
   call msg('o g_sq','#',(/'E  [meV]','gF^2 sum','gF^2 Fan','gF^2  DW',&
                           'gF^2 Cor','Gamma(G)','G   Bose','G F     '/),&
                         INDENT=0,USE_TABS=.true.)
   call msg('o g_sq','#')
   !
   do iw=1,elph_steps
     call msg('o g_sq','',&
             (/real(g_sq_F_E%p(iw))*HARTREE*1000.,&
               g_sq_F(iqp,iw,1)+g_sq_F(iqp,iw,2)+g_sq_F(iqp,iw,3),&
               g_sq_F(iqp,iw,1),g_sq_F(iqp,iw,2),g_sq_F(iqp,iw,3),&
               g_sq_F(iqp,iw,4)+g_sq_F(iqp,iw,5),&
               g_sq_F(iqp,iw,5),g_sq_F(iqp,iw,5)/),&
             INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(trim(ch))
   !
 enddo
 !
 call W_reset(g_sq_F_E)
 call elph_global_free()
 call pp_indexes_reset(px)
 deallocate(state_is_2do,QP_table,g_sq_F)
 !
end subroutine
