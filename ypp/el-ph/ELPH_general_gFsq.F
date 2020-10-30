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
subroutine ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs)
 !
 use pars,                ONLY:SP,schlen
 use units,               ONLY:HA2EV
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 use frequency,           ONLY:w_samp,W_reset
 use YPPm,                ONLY:EXCITONS_user_indexes,EXCITONS_n_user_states
 use YPP_ELPH,            ONLY:elph_steps,ph_broad
 use stderr,              ONLY:intc
 use interfaces,          ONLY:ELPH_alloc
 use ELPH,                ONLY:W_debye,elph_nQ_used,ph_modes,&
&                              gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,&
&                              gsqF_life_f ,elph_use_q_grid,ph_freqs_sq
 use IO_m,                ONLY:io_control,OP_RD,RD,DUMP,RD_CL
 use BS_solvers,          ONLY:BS_mat,BSS_eh_table
 use BS,                  ONLY:BS_K_dim,BS_bands
 use QP_m,                ONLY:QP_n_states,QP_table,QP_state,QP_nb,QP_nk,QP_states_simmetrize
 use LIVE_t,              ONLY:live_timing
 use parallel_m,          ONLY:PP_indexes,PP_indexes_reset,myid,PAR_nQ_bz
 use parallel_int,        ONLY:PP_redux_wait,PARALLEL_index 
 use functions,           ONLY:Fermi_fnc_derivative
 !
#include<memory.h>
 !
 type(levels) ::en,Xen
 type(bz_samp)::k,Xk,q
 integer,  optional      ::BS_E_degs(:)
 !
 ! Work Space (electrons)
 !
 integer              ::iqp,QP_n_states_2do,QP_n_states_DB 
 logical, allocatable ::state_is_2do(:)
 integer, allocatable ::QP_table_map(:,:),QP_table_DB(:,:) 
 !
 ! Work Space (excitons)
 !
 integer              ::i_l,lambda,n_lambda_deg,i_lambda,neh,ikibz,iv,ic,nq_loop
 real(SP)             ::g_sq_F_exciton(elph_steps)
 !
 ! Work Space (common)
 !
 integer              ::iq,il,iw,i1
 type(w_samp)         ::g_sq_F_E
 logical              ::gFsq_is_excitonic
 real(SP)             ::delta_E,ph_freq
 real(SP),allocatable ::g_sq_F(:,:,:)
 type(PP_indexes)     ::px
 character(schlen)    ::ch
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::io_ELPH
 !
 ! calculate q%weights
 !
 call k_expand(q) 
 !
 gFsq_is_excitonic=allocated(BS_mat)
 if (gFsq_is_excitonic) then
   call section("+","Excitonic Generalized Eliashberg functions")
   !
   ! Internal QP states
   !
   QP_nb=BS_bands(2)
   QP_nk=Xk%nibz
   YAMBO_ALLOC(QP_state,(QP_nb,QP_nk))
   QP_state=.FALSE.
   forall( i1=BS_bands(1):BS_bands(2) ) QP_state(i1,:)=.TRUE.
   call QP_state_table_setup(Xen)
   QP_n_states_DB=QP_n_states
   !
 else
   call section("*","Generalized Eliashberg functions")
   !
   ! Input file QP states
   !
   call QP_state_table_setup(en)
   QP_n_states_DB=QP_n_states
   !
 endif
 !
 YAMBO_ALLOC(QP_table_DB,(QP_n_states_DB,3+n_sp_pol-1))
 QP_table_DB=QP_table
 YAMBO_FREE(QP_table)
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_ELPH(ID,'gFsq')
 !
 if (io_err/=0) return
 !
 YAMBO_ALLOC(QP_table,(QP_n_states,3+n_sp_pol-1)) ! Read from the gFsq DB 
 !
 if (     elph_use_q_grid) nq_loop=q%nbz
 if (.not.elph_use_q_grid) nq_loop=elph_nQ_used
 PAR_nQ_bz=nq_loop
 !
 call ELPH_alloc('gFsq')
 ! 
 do iq=1,nq_loop
   if (iq==nq_loop) then
     call io_control(ACTION=RD_CL,SEC=(/iq+1/),ID=ID)
   else
     call io_control(ACTION=RD,SEC=(/iq+1/),ID=ID)
   endif
   io_err=io_ELPH(ID,'gFsq')
 enddo
 !
 ! Energy setup
 !
 call W_reset(g_sq_F_E)
 g_sq_F_E%n_freqs=elph_steps
 g_sq_F_E%er=(/0._SP,W_debye*1.1/)
 g_sq_F_E%dr=ph_broad
 call FREQUENCIES_setup(g_sq_F_E)
 call PP_indexes_reset(px)
 !
 ! Local QP states
 !
 if (.not.gFsq_is_excitonic) then
   !
   YAMBO_ALLOC(state_is_2do,(QP_n_states))
   call QP_states_simmetrize(en,state_is_2do=state_is_2do)
   !
 endif
 !
 YAMBO_ALLOC(g_sq_F,(QP_n_states,elph_steps,5))
 !
 g_sq_F=0.
 !
 ! I need to find the QP states that I really need to calculate
 ! considering degeneracies and user defined states
 !
 YAMBO_ALLOC(QP_table_map,(QP_n_states_DB,1))
 QP_table_map=-1
 do iqp=1,QP_n_states_DB
   do i1=1,QP_n_states
      if (all(QP_table_DB(iqp,:)==QP_table(i1,:))) QP_table_map(iqp,1)=i1
   enddo
 enddo
 !
 QP_n_states_2do=count(QP_table_map(:,1)>0)
 call PP_redux_wait
 !
 call live_timing('gF^2 [el]',QP_n_states_2do*nq_loop)
 !
 do i1=1,QP_n_states_DB
   !
   if (QP_table_map(i1,1)<0) cycle
   !
   iqp=QP_table_map(i1,1)
   !
   do iq=1,nq_loop
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
         g_sq_F(iqp,iw,1)=g_sq_F(iqp,iw,1)+gsqF_fan(iqp,iq,il,1)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,2)=g_sq_F(iqp,iw,2)+gsqF_dw(iqp,iq,il)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,3)=g_sq_F(iqp,iw,3)+gsqF_ca_corr(iqp,iq,il,1)*&
&                     Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,4)=g_sq_F(iqp,iw,4)+gsqF_life_bose(iqp,iq,il,1)*&
&                    Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
         g_sq_F(iqp,iw,5)=g_sq_F(iqp,iw,5)+gsqF_life_f(iqp,iq,il,1)*&
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
 ! gF^2 plotting (electrons)
 !
 if (.not.gFsq_is_excitonic) then
   !
   do i1=1,QP_n_states_DB
     !
     if (QP_table_map(i1,1)<0) cycle
     !
     iqp=QP_table_map(i1,1)
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
               (/real(g_sq_F_E%p(iw))*HA2EV*1000.,&
                 g_sq_F(iqp,iw,1)+g_sq_F(iqp,iw,2)+g_sq_F(iqp,iw,3),&
                 g_sq_F(iqp,iw,1),g_sq_F(iqp,iw,2),g_sq_F(iqp,iw,3),&
                 g_sq_F(iqp,iw,4)/2.+g_sq_F(iqp,iw,5),&
                 g_sq_F(iqp,iw,4)/2.,g_sq_F(iqp,iw,5)/),&
               INDENT=-2,USE_TABS=.TRUE.)
     enddo
     call of_open_close(trim(ch))
     !
   enddo
   !
 endif
 !
 ! ###########################
 ! SPECIFIC EXCITONIC SECTION
 ! ###########################
 !
 if (gFsq_is_excitonic) then
   !
   call msg('s',' Building the correspondance map')
   !
   YAMBO_FREE(QP_table_map)
   YAMBO_ALLOC(QP_table_map,(BS_K_dim,2))
   QP_table_map=-1
   !
   do neh = 1,BS_K_dim
     !
     ikibz = Xk%sstar(BSS_eh_table(neh,1),1) 
     iv    = BSS_eh_table(neh,2)
     ic    = BSS_eh_table(neh,3)
     !
     do iqp=1,QP_n_states
       if (QP_table(iqp,1)==ic.and.QP_table(iqp,3)==ikibz) QP_table_map(neh,1)=iqp
       if (QP_table(iqp,1)==iv.and.QP_table(iqp,3)==ikibz) QP_table_map(neh,2)=iqp
     enddo
     !
   enddo
   !
   call msg('s',' Processing '//trim(intc(EXCITONS_n_user_states))//' state(s)')
   !
   call PARALLEL_index(px,(/BS_K_dim/))
   call PP_redux_wait
   !
   do i_lambda=1,EXCITONS_n_user_states
     !
     g_sq_F_exciton=0.
     !
     lambda=EXCITONS_user_indexes(i_lambda)
     !
     n_lambda_deg=count(BS_E_degs==BS_E_degs(lambda))
     !
     if (n_lambda_deg>1) call msg('s',' State '//trim(intc(lambda))//' Merged with states '//&
&                                     trim(intc(BS_E_degs(lambda)))//' -> '//&
&                                     trim(intc(BS_E_degs(lambda)+n_lambda_deg-1)))
     !
     call live_timing('gF^2 [exc] @ state '//trim(intc(lambda)),px%n_of_elements(myid+1))
     !
     do neh = 1,BS_K_dim
       !
       if (.not.px%element_1D(neh)) cycle
       !
       do i_l=BS_E_degs(lambda),BS_E_degs(lambda)+n_lambda_deg-1
         !
         ic    = QP_table_map(neh,1)
         iv    = QP_table_map(neh,2)
         !
         g_sq_F_exciton(:)=g_sq_F_exciton(:)+abs(BS_mat(neh,i_l))**2.*& 
&                         (g_sq_F(ic,:,1)+g_sq_F(ic,:,2)+g_sq_F(ic,:,3)&
&                         -g_sq_F(iv,:,1)-g_sq_F(iv,:,2)-g_sq_F(iv,:,3) )
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
     !
     ch='g_sq_F_exc_state_'//trim(intc(lambda))
     call of_open_close(trim(ch),'ot')
     call msg('o g_sq','#',(/'E [meV]','gF^2   '/),INDENT=0,USE_TABS=.true.) 
     call msg('o g_sq','#')
     !
     do iw=1,elph_steps
       call msg('o g_sq','',&
               (/real(g_sq_F_E%p(iw))*HA2EV*1000.,g_sq_F_exciton(iw)/),&
&              INDENT=-2,USE_TABS=.TRUE.)
     enddo
     call of_open_close(trim(ch))
     !
   enddo
   !
 endif
 !
 call W_reset(g_sq_F_E)
 call ELPH_alloc('FREE')
 call PP_indexes_reset(px)
 YAMBO_FREE(QP_table)
 YAMBO_FREE(g_sq_F)
 YAMBO_FREE(state_is_2do)
 YAMBO_FREE(QP_table_map)
 YAMBO_FREE(QP_table_DB)
 !
end subroutine
