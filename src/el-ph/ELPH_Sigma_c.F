!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine ELPH_Sigma_c(en,k,q,qp)
 !
 ! This routine calculates the QP shifts due to el-ph intercation
 ! following the Allen-Cardona formulation (see for example 
 ! PRB 23, 1495 (1981) )
 !
 use pars,          ONLY:SP,schlen,pi,rZERO,cZERO
 use units,         ONLY:HA2EV,HA2THZ
 use parser_m,      ONLY:parser
 use frequency,     ONLY:w_samp,W_reset
 use electrons,     ONLY:levels
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use drivers,       ONLY:l_elel_corr
 use parallel_int,  ONLY:PP_wait,PARALLEL_global_indexes
 use parallel_m,    ONLY:PAR_IND_Q_bz,PAR_Q_bz_index,PAR_IND_QP,&
&                        PAR_IND_Q_bz_ID,PAR_IND_G_b,PAR_IND_G_b_ID,PAR_IND_QP_ID,master_cpu,&
                         PAR_COM_Q_A2A,PAR_COM_Q_INDEX,myid
 use IO_int,        ONLY:io_control,IO_and_Messaging_switch
 use IO_m,          ONLY:manage_action,OP_RD,REP,RD_CL_IF_END,OP_WR,WR,WR_CL,&
&                        deliver_IO_error_message,OP_WR_CL,OP_APP_CL,DUMP
 use QP_ctl_m,      ONLY:QP_apply
 use QP_m,          ONLY:QP_t,QP_G_damp,QP_Sc,QP_n_states,QP_table,&
&                        QP_dSc_steps,QP_dSc_delta,QP_solver,QP_Sc_steps,&
&                        On_Mass_Shell_approx,QP_time_order_sign
 use D_lattice,     ONLY:sop_inv,nsym,i_time_rev,sop_tab
 use R_lattice,     ONLY:qindx_S,bz_samp,RL_vol,nqibz
 use ELPH,          ONLY:elph_bands,PH_freqs_sq,GKKP,ph_modes,PH_freqs,FineGd_E_kpq_components_reset,&
&                        QP_PH_n_G_bands,PH_W_debye,use_PH_DbGd,EkplusQ_mode,gkkp_db,elph_grids_are_expanded, &
&                        elph_nQ,elph_use_q_grid,setup_k_plus_q_levels,elph_nQ_used,&
&                        E_kpq_sh_fact,Efermi_DbGd,gsqF_energy_steps,eval_G_using_KK,PH_qpt,ID_E_kpq,ID_E_kpq_obj, &
&                        elph_branches,QP_DW,QP_OMS_Fan,FAN_deltaE_treshold
 use interfaces,    ONLY:QP_state_print,ELPH_alloc
 use interpolate,   ONLY:INTERPOLATE_is_serial,INTERPOLATION_coefficients,INTERP_obj,INTERP_shell_factor,&
&                        INTERPOLATE_is_quiet    
 use stderr,        ONLY:STRING_same
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(levels)  ::en
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 ! WorkSpace  
 !
 integer          ::iq_db,iq_bz,iq_loop,iq_db_ref,iq_s,iq_ibz,ik,ok,ik_bz,okbz,ib,ob,is,il,i_qp,i2,&
&                   ik_bz_gkkp,ib_gkkp,ob_gkkp,os,nq_to_sum,iq_mem,live_timing_steps
 type(w_samp)     ::Sc_W(qp%n_states)
 integer          ::io_err,ID,IO_ACT,ID_DG
 integer, external::io_ELPH,io_Double_Grid,io_gFsq
 character(schlen)::ch
 real(SP)         ::ph_E,factor,elph_gkkp_sq,E_random_shift
 logical          ::l_WRgFsq,l_GF_from_CA,load_gkkp_DB
 real(SP), allocatable :: q_weight(:)
 real(SP), external :: Eval_Efermi_DbGd
 ! 
 call timing('GW(e-p)',OPR='start')
 !
 ! Zeroing
 !
 do i_qp=1,qp%n_states
   call W_reset(Sc_W(i_qp))
 enddo
 QP_DW=rZERO
 QP_OMS_Fan=rZERO
 !
 if (     l_elel_corr) call section('+','Correlation: Phonon-mediated Self-energy')
 if (.not.l_elel_corr) call section('=','Correlation: Phonon-mediated Self-energy')
 !
 ! gFsq coefficients 2 DB ?
 !
 call parser('WRgFsq',l_WRgFsq)
 !
 ! Eval Green's function directly from Allen-Cardona expression ?
 !
 call parser('GF_from_CA',l_GF_from_CA)
 !
 ! Eval Green's functions using KK
 !
 eval_G_using_KK=trim(QP_solver)=='g'.and..not.l_GF_from_CA
 if (eval_G_using_KK) l_WRgFsq=.FALSE.
 !
 call k_build_up_BZ_tables(k)
 call k_expand(k)
 !
 if (QP_PH_n_G_bands(2)<=elph_bands(1).or.QP_PH_n_G_bands(2)>elph_bands(2)) QP_PH_n_G_bands=elph_bands
 if (QP_PH_n_G_bands(1)<=elph_bands(1).or.QP_PH_n_G_bands(1)>elph_bands(2)) QP_PH_n_G_bands=elph_bands
 !
 call msg('r', '[GW/El-Ph] Bands range       ',(/QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)/))
 if (trim(QP_solver)=='n') &
&  call msg('r', '[GW/El-Ph] G damping         ',QP_G_damp*HA2EV,"[eV]")
 !
 ! QP_table -> report
 !
 call QP_state_print( )
 !
 ! ELPH DB
 ! =======================
 select case(trim(gkkp_db)) 
   case("gkkp")
     load_gkkp_DB=.true.
   case("gkkp_expanded")
     load_gkkp_DB=.true.
   case("genFroh")
     load_gkkp_DB=.false.
     call error("Generalized Frohlich model not implemented yet ")
 end select
 !
 call msg('sr','GKKP database/model: '//gkkp_db)
 !
 if(load_gkkp_DB) then
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_ELPH(ID,gkkp_db)
   call deliver_IO_error_message(io_err,"GKKP")
   if (io_err/=0) call error('El-Ph database not found')
 endif
 !
 !Sc Energy points
 !
 if (trim(QP_solver)=='g') then
   !
   do i_qp=1,qp%n_states
     !
     Sc_W(i_qp)%n_freqs =QP_Sc_steps
     !
     call FREQUENCIES_Green_Function(i_qp,Sc_W(i_qp),en%E,.not.l_GF_from_CA)
     !
   enddo
   QP_Sc_steps      =Sc_W(1)%n_freqs
   gsqF_energy_steps=Sc_W(1)%n_freqs
   !
   if (.not.l_elel_corr) QP_Sc=cZERO
   !
   call msg('nr', '[GW/El-Ph] gsqF E range      ',Sc_W(1)%er*HA2EV,"[eV]")
   !
 else if (trim(QP_solver)=='n') then
   !
   do i_qp=1,qp%n_states
     Sc_W(i_qp)%n_freqs=QP_dSc_steps
     YAMBO_ALLOC(Sc_W(i_qp)%p,(Sc_W(i_qp)%n_freqs))
     forall (i2=1:QP_dSc_steps) Sc_W(i_qp)%p(i2)=&
&           en%E(QP_table(i_qp,1),QP_table(i_qp,3),1)+(i2-1)*QP_dSc_delta+&
&           cmplx(0.,QP_G_damp,SP)
   enddo
   !
   gsqF_energy_steps=QP_dSc_steps
   if (On_Mass_Shell_approx) gsqF_energy_steps=1
   !
 endif
 !
 ! Parallelization
 !-----------------
 call PARALLEL_global_indexes(en,k,q,"Self_Energy")
 !
 ! Note that only by using causal ordering it is correct to use 
 ! the KK to calculate the Green's function
 !
 if (trim(QP_solver)=='g')  QP_time_order_sign=1
 !
 ! Q ranges and Spherical RIM
 !============================
 !
 !             / ypp_ph            elph_use_q_grid=F
 ! elph_nQ = | 
 !             \ nqibz             elph_use_q_grid=T
 !
 !                  / <user defined>    elph_use_q_grid=F
 ! elph_nQ_used = | 
 !                  \ nqibz             elph_use_q_grid=T
 !
 !              / elph_nQ_used     elph_use_q_grid=F
 ! nq_to_sum = | 
 !              \ nqbz             elph_use_q_grid=T
 !
 factor=(3._SP*RL_vol/4._SP/pi)**(1._SP/3._SP)
 if (elph_use_q_grid.and..not.elph_grids_are_expanded) then
   nq_to_sum=q%nbz
   call k_ibz2bz(q,'i',.TRUE.)
   YAMBO_ALLOC(q_weight,(nq_to_sum))
   call rim_spherical(nq_to_sum,q%ptbz,q_weight,factor/real(nq_to_sum,SP),2,.TRUE.)
   call k_ibz2bz(q,'d',.TRUE.)
 else
   nq_to_sum=elph_nQ_used
   YAMBO_ALLOC(q_weight,(nq_to_sum))
   call rim_spherical(nq_to_sum,PH_qpt(:nq_to_sum,:),q_weight,factor/real(nq_to_sum,SP),2,.TRUE.)
 endif
 !
 if(elph_nQ_used/=elph_nQ) call warning(" Number of q-used different from total number of q-points")
 !
 ! Check double-grid
 !-------------------
 !
 ! Fill the PH_E array
 !
 PH_freqs%nk=nq_to_sum
 PH_freqs%nb=ph_modes
 YAMBO_ALLOC(PH_freqs%E,(nq_to_sum,ph_modes,1))
 ! 
 call io_control(ACTION=OP_RD,COM=REP,MODE=DUMP,SEC=(/1,2/),ID=ID_DG)
 io_err=io_Double_Grid(PH_freqs,q,ID_DG,'phonons')
 !
 if(io_err==0) then
   if(elph_use_q_grid) then
     call msg('sr','Using PH-double-grid ')
     use_PH_DbGd=.TRUE.
     call ELPH_alloc("DGRID",GKKP)
   else
     call msg('sr','PH-double-grid not yet compatible with Random q-grids and Eliashberg functions!')
     use_PH_DbGd=.FALSE.
   endif
   !
 endif
 !
 ! Interpolate the E(k+q) energies for the double-grid
 !
 if(use_PH_DbGd.and.STRING_same(EkplusQ_mode,'interp')) then
   !      
   call msg('sr','E(k+q) calculated using a smooth Fourier interpolation')
   INTERPOLATE_is_serial=.TRUE.
   INTERPOLATE_is_quiet =.TRUE.
   INTERP_shell_factor  =E_kpq_sh_fact
   call INTERPOLATION_BZ_setup(k)
   INTERP_obj(ID_E_kpq_obj)%what="E(k+q)"
   call INTERPOLATION_coefficients(E=en,k=k,ID=ID_E_kpq,ID_obj=ID_E_kpq_obj)
   !
   ! Evaluate the new Fermi-Energy
   !
   if(any(en%nbm/=en%nbf)) Efermi_DbGd=Eval_Efermi_DbGd(k,q,en)
   !
   call En_k_plus_q_interpolate(k,q,en,1)  ! I need them for the DW term
   !
 endif
 !
 ! Timing steps
 !--------------
 live_timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&                  PAR_IND_Q_bz%n_of_elements(PAR_IND_Q_bz_ID+1)*&
&                  PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
 !
 call PP_wait()
 !
 ! g^2F(w) residuals Allocation
 !
 call ELPH_alloc('gFsq')
 !
 call section('=','Momenta and branches loop')
 !
 iq_db_ref=0
 !
 do iq_loop=1,nq_to_sum
   !
   iq_bz = iq_loop
   if (elph_use_q_grid.and..not.elph_grids_are_expanded) then
     iq_ibz =q%sstar(iq_bz,1)
     iq_s   =q%sstar(iq_bz,2)
     iq_db = iq_ibz
   else
     iq_ibz= iq_loop
     iq_s  = 0
     iq_db = iq_loop
   endif
   !
   ! DB I/O can be skipped except at the gamma point which is used to define E_k_plus_q_at_gamma array
   !
   if (.not.PAR_IND_Q_bz%element_1D(iq_loop).and.iq_db>1) cycle
   iq_mem=PAR_Q_bz_index(iq_loop)
   !
   if (iq_db/=iabs(iq_db_ref)) then
     !
     if (load_gkkp_DB) then
       !      
       if (     elph_use_q_grid.and..not.elph_grids_are_expanded) IO_ACT=manage_action(RD_CL_IF_END,iq_ibz,1,nqibz)
       if (.not.elph_use_q_grid.or.elph_grids_are_expanded)       IO_ACT=manage_action(RD_CL_IF_END,iq_db,1,nq_to_sum)
       !
       call io_control(ACTION=IO_ACT,SEC=(/iq_db+1/),ID=ID)
       io_err=io_ELPH(ID,gkkp_db)
       !
       if (io_err<0) call error('Missing Q-database')
       !
     endif
     !
     if (.not.elph_use_q_grid.or.elph_grids_are_expanded) then
       !
       if (iq_bz==1) E_random_shift=GKKP%E_kpq(iq_db)%E(en%nbf(1),1,1)-en%E(en%nbf(1),1,1)
       !
       call setup_k_plus_q_levels(iq_db,E_random_shift)
       !
       call QP_apply(elph_bands,GKKP%E_kpq(iq_db),k,"G",msg_fmt=' ')
       !
     endif
     !
     ! I call live_timing here as in ioELPH the global_alloc() can 
     ! send a screen message about the allocated memory that can interferee
     ! with the live_timing hashes
     !
     if (iq_db_ref==0) then
       call live_timing('El-Ph Sc [coeff]',live_timing_steps)
     endif
     !
     iq_db_ref=iq_db
     !
   endif
   !
   if (.not.PAR_IND_Q_bz%element_1D(iq_loop)) cycle
   !
   if(use_PH_DbGd.and.STRING_same(EkplusQ_mode,'interp').and.iq_bz/=1) call En_k_plus_q_interpolate(k,q,en,iq_bz)
   !
   do i_qp=1,QP_n_states
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     ib   =QP_table(i_qp,1)
     ik   =QP_table(i_qp,3)
     ik_bz=sum(k%nstar(:ik-1))+1
     okbz=0
     ok  =0
     !
     do ob=QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)
       !
       if (.not.PAR_IND_G_b%element_1D(ob)) cycle
       !
       ik_bz_gkkp=ik_bz
       ib_gkkp   =ib
       ob_gkkp   =ob
       !
       if (elph_use_q_grid.and..not.elph_grids_are_expanded) then
         okbz=qindx_S(ik,iq_bz,1)
         ok=k%sstar(okbz,1)
         os=k%sstar(okbz,2)
         !
         ! When using a uniform Q grid I cycle on the q symmetries 
         ! as well. To rotate the gkkp m.e. I use:
         ! 
         ! gkkp_{I_need}= <k+Rq n'|dV_{SCF}/du^{Rq nu}|k n>=
         !                <(R^-1 k)+q n'|dV_{SCF}/du^{q nu}|(R^-1 k) n>= 
         !                gkkp(ik_bz,nu,n',n)
         !
         ik_bz_gkkp=k%k_table(ik,sop_inv(iq_s))  
         !
         ! COMMENT: I do not have sop_tab(:,is) because is=1 (identity)
         ! 
         ! gkkp_{I_need}= <k+IRq n'|dV_{SCF}/du^{IRq nu}|k n>=
         !                [<(R^-1 S p)+q n|dV_{SCF}/du^{q nu}|(R^-1 S p) n'>]^*= 
         !                [gkkp(ik_bz,nu,n,n')]^*
         ! 
         ! with k + IRq = Sp 
         !
         if (iq_s>nsym/(i_time_rev+1)) then
           ib_gkkp   =ob
           ob_gkkp   =ib
           !                                 R^-1                 S
           !                                 -------------------- --
           ik_bz_gkkp=k%k_table(ok,sop_tab(sop_inv(iq_s-nsym/2),os))
         endif
         !
         ! k is in the IBZ and q is the PW_q (=-YAMBO_q)
         !
       endif
       !
       do il=elph_branches(1),elph_branches(2)
         !
         ph_E=sqrt(abs(PH_freqs_sq(iq_ibz,il)))
         !
         ! Skip modes @ Gamma (1st point is always gamma, either with random
         ! or uniform grids, as it is needed to evaluate the DW factor) 
         !
         if (abs(ph_E)<FAN_deltaE_treshold) cycle
         !
         ! In the SE expression I have the m.e. 
         !
         !  <ib ik|g(q_YAMBO l r)|ob ik-q_YAMBO> = [<ob ik+q_PW|g(q_PW l r)|ib ik>]^* = 
         !                              elph_gkkp(il,ob_gkkp,ib_gkkp,ik_bz_gkkp)^*
         !
         ! with q_YAMBO = - q_PW
         !
         elph_gkkp_sq=real(conjg(GKKP%dVc(il,ob_gkkp,ib_gkkp,ik_bz_gkkp,1))*&
&                           GKKP%dVc(il,ob_gkkp,ib_gkkp,ik_bz_gkkp,1)/2._SP/ph_E,SP)
         !
         ! ELPH_Sigma_c_engine evaluates real and imaginary part of the electron-phonon self-energy
         !
         call ELPH_Sigma_c_engine(i_qp,ob,ok,iq_bz,iq_ibz,iq_mem,il,elph_gkkp_sq,en,k,q,& 
&                                 Sc_W(i_qp),q_weight(iq_bz))
         !
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
   enddo  ! i_qp loop
   !
   if(use_PH_DbGd.and.iq_loop/=1) call FineGd_E_kpq_components_reset(GKKP%E_dg_kpq(iq_loop))
   !
 enddo ! iq_bz loop
 !
 call live_timing()
 !
 ! Debye energy
 !
 call msg('nrs','[Ph] Debye energy         ',(/PH_W_debye*HA2EV,PH_W_debye*HA2THZ/),"[ev/ThZ]")
 !
 ! Here I use the Cardona-Allen g^2 F functions to evaluate their integrated value.
 ! I also calculate the full frequency dependent self-energy
 !
 call ELPH_gsqF_to_Sigma(en,q,l_GF_from_CA,Sc_W)
 !
 ! Dump on file gFsq coefficients 
 !
 if (l_WRgFsq) then
   !
   if (master_cpu) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
     io_err=io_gFsq(ID)
   endif
   call PP_wait()
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
   do iq_loop=1,nq_to_sum
     if (.not.PAR_IND_Q_bz%element_1D(iq_loop)) cycle
     call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/iq_loop+1/),ID=ID)
     io_err=io_gFsq(ID)
   enddo
   call IO_and_Messaging_switch("+io_out",CONDITION=master_cpu)
   !
 endif
 !
 ! CLEAN
 !
 call ELPH_alloc('FREE')
 YAMBO_FREE(q_weight)
 do i_qp=1,qp%n_states
   call W_reset(Sc_W(i_qp))
 enddo
 call timing('GW(e-p)',OPR='stop')
 !
end subroutine
