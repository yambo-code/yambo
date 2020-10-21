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
 use electrons,     ONLY:levels,spin_occ
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg
 use drivers,       ONLY:l_elel_corr
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q_bz,PAR_Q_bz_index,PAR_IND_QP,&
&                        PAR_IND_Q_bz_ID,PAR_IND_G_b,PAR_IND_G_b_ID,PAR_IND_QP_ID,master_cpu,&
                         PAR_COM_Q_A2A,PAR_COM_Q_INDEX,myid
 use IO_m,          ONLY:io_control,manage_action,OP_RD,REP,RD_CL_IF_END,OP_WR,WR,WR_CL,IO_and_Messaging_switch,&
&                        deliver_IO_error_message,OP_WR_CL,OP_APP_CL
 use QP_ctl_m,      ONLY:QP_apply
 use QP_m,          ONLY:QP_t,QP_G_damp,QP_Sc,QP_n_states,QP_table,&
&                        QP_dSc_steps,QP_dSc_delta,QP_solver,QP_Sc_steps,&
&                        On_Mass_Shell_approx,QP_time_order_sign
 use D_lattice,     ONLY:sop_inv,nsym,i_time_rev,sop_tab
 use R_lattice,     ONLY:qindx_S,bz_samp,RL_vol,nqibz
 use ELPH,          ONLY:elph_nb,ph_freqs_sq,elph_gkkp,&
&                        elph_global_free,QP_PH_n_G_bands,W_debye,elph_DW,&
&                        elph_nDBs,E_k_p_q,elph_use_q_grid,&
&                        setup_k_plus_q_levels,f_k_plus_q,elph_nDBs_used,&
&                        gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f ,&
&                        elph_global_alloc,gsqF_energy_steps,eval_G_using_KK,ph_qpt,&
&                        elph_branches,E_k_plus_q,QP_DW,QP_OMS_Fan,FAN_deltaE_treshold,DW_deltaE_treshold
 use interfaces,    ONLY:PARALLEL_global_indexes,QP_state_print
 use functions,     ONLY:Fermi_fnc_derivative
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 type(levels)  ::en
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 ! WorkSpace  
 !
 integer          ::iq_db,iq_bz,iq_loop,iq_db_ref,iq_s,iq_ibz,ik,ok,ik_bz,okbz,ib,ob,is,il,i_qp,i2,&
&                   ik_bz_gkkp,ib_gkkp,ob_gkkp,os,iw,nq_to_sum,iq_mem,live_timing_steps,iq_range(2)
 type(w_samp)     ::Sc_W(qp%n_states)
 integer          ::io_err,ID,IO_ACT
 integer, external::io_ELPH
 character(schlen)::ch
 real(SP)         ::ph_E,delta_E_at_gamma,one_minus_2f_occ,f_occ,factor,&
&                   E_k_plus_q_at_gamma(elph_nb,k%nbz),elph_gkkp_sq,gsqF_damping,E_random_shift
 complex(SP)      ::delta_E
 logical          ::l_WRgFsq,l_GF_from_CA
 integer , allocatable :: q_each_cpu(:)
 real(SP), allocatable :: q_weight(:)
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
 if (l_elel_corr) then
   call section('+','Correlation: Phonon-mediated Self-energy')
 else
   call section('=','Correlation: Phonon-mediated Self-energy')
 endif
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
 if (QP_PH_n_G_bands<=0.or.QP_PH_n_G_bands>elph_nb) QP_PH_n_G_bands=elph_nb
 !
 call msg('r', '[GW/El-Ph] Bands range       ',(/1,QP_PH_n_G_bands/))
 if (trim(QP_solver)=='n') &
&  call msg('r', '[GW/El-Ph] G damping         ',QP_G_damp*HA2EV,"[eV]")
 !
 ! QP_table -> report
 !
 call QP_state_print( )
 !
 ! ELPH DB
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 call deliver_IO_error_message(io_err,"GKKP")
 if (io_err/=0) call error('El-Ph database not found')
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
 ! elph_nDBs = | 
 !             \ nqibz             elph_use_q_grid=T
 !
 !                  / <user defined>    elph_use_q_grid=F
 ! elph_nDBs_used = | 
 !                  \ nqibz             elph_use_q_grid=T
 !
 !              / elph_nDBs_used   elph_use_q_grid=F
 ! nq_to_sum = | 
 !              \ nqbz             elph_use_q_grid=T
 !
 factor=(3._SP*RL_vol/4._SP/pi)**(1._SP/3._SP)
 if (elph_use_q_grid) then
   nq_to_sum=q%nbz
   call k_ibz2bz(q,'i',.TRUE.)
   YAMBO_ALLOC(q_weight,(nq_to_sum))
   YAMBO_ALLOC(q_each_cpu,(q%nibz))
   call rim_spherical(nq_to_sum,q%ptbz,q_weight,factor/real(nq_to_sum,SP),2,.TRUE.)
   call k_ibz2bz(q,'d',.TRUE.)
 else
   nq_to_sum=elph_nDBs_used
   YAMBO_ALLOC(q_weight,(nq_to_sum))
   call rim_spherical(nq_to_sum,ph_qpt(:nq_to_sum,:),q_weight,factor/real(nq_to_sum,SP),2,.TRUE.)
 endif
 !
 ! Timing steps
 !--------------
 live_timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&                  PAR_IND_Q_bz%n_of_elements(PAR_IND_Q_bz_ID+1)*&
&                  PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
 !
 call PP_redux_wait
 !
 ! g^2F(w) residuals Allocation
 !
 call elph_global_alloc('gFsq')
 !
 iq_db_ref=0
 !
 do iq_loop=1,nq_to_sum
   !
   iq_bz = iq_loop
   if (elph_use_q_grid) then
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
     if (elph_use_q_grid)      IO_ACT=manage_action(RD_CL_IF_END,iq_ibz,1,nqibz)
     if (.not.elph_use_q_grid) IO_ACT=manage_action(RD_CL_IF_END,iq_db,1,nq_to_sum)
     call io_control(ACTION=IO_ACT,SEC=(/iq_db+1/),ID=ID)
     io_err=io_ELPH(ID,'gkkp')
     if (io_err<0) call error('Missing Q-database')
     !
     if (.not.elph_use_q_grid) then
       !
       if (iq_bz==1) E_random_shift=E_k_p_q%E(en%nbf,1,1)-en%E(en%nbf,1,1)
       !
       call setup_k_plus_q_levels(E_random_shift)
       !
       E_k_p_q%E(:,:,1)=E_k_plus_q(:,:,1)
       !
       call QP_apply((/1,elph_nb/),E_k_p_q,k,"G",msg_fmt=' ')
       !
     endif
     !
     ! I call live_timing here as in ioELPH the global_alloc() can 
     ! send a screen message about the allocated memory that can interferee
     ! with the live_timing hashes
     !
     if (iq_db_ref==0) then
       call live_timing('El-Ph Sc [coeff]',live_timing_steps)
       if (.not.elph_use_q_grid) E_k_plus_q_at_gamma(:,:)=E_k_p_q%E(:,:,1)
     endif
     !
     iq_db_ref=iq_db
     !
   endif
   !
   if (.not.PAR_IND_Q_bz%element_1D(iq_loop)) cycle
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
     do ob=1,QP_PH_n_G_bands
       !
       if (.not.PAR_IND_G_b%element_1D(ob)) cycle
       !
       ik_bz_gkkp=ik_bz
       ib_gkkp   =ib
       ob_gkkp   =ob
       !
       if (elph_use_q_grid) then
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
         ph_E=sqrt(abs(ph_freqs_sq(iq_ibz,il)))
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
         elph_gkkp_sq=conjg(elph_gkkp(il,ob_gkkp,ib_gkkp,ik_bz_gkkp))*&
&                           elph_gkkp(il,ob_gkkp,ib_gkkp,ik_bz_gkkp)/2._SP/ph_E
         !
         ! Frequency Loop
         !
         do iw=1,gsqF_energy_steps
           !
           ! Define gsqF denominators
           !
           if (elph_use_q_grid) then
             delta_E         =Sc_W(i_qp)%p(iw)-en%E(ob,ok,1)
             delta_E_at_gamma=en%E(ib,ik,1)  -en%E(ob,ik,1)
             f_occ=en%f(ob,ok,1)/spin_occ
             one_minus_2f_occ=(1._SP-2._SP*f_occ)
           else
             delta_E         =Sc_W(i_qp)%p(iw)-E_k_p_q%E(ob,ik_bz,1)
             delta_E_at_gamma=en%E(ib,ik,1)   -E_k_plus_q_at_gamma(ob,ik_bz)
             f_occ=f_k_plus_q(ob,ik_bz,1)/spin_occ
             one_minus_2f_occ=(1._SP-2._SP*f_occ)
           endif
           !
           gsqF_damping=aimag( Sc_W(i_qp)%p(iw) )
           !
           ! Lifetimes
           !-----------
           !
           ! "Bose" Term
           !
           gsqF_life_bose(i_qp,iq_mem,il,iw)= &
&              gsqF_life_bose(i_qp,iq_mem,il,iw)+2.*pi*elph_gkkp_sq*q_weight(iq_loop)*&
&              ( Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) + &
&              QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping) )
           !
           ! "f" Term
           !
           gsqF_life_f(i_qp,iq_mem,il,iw)= &
&               gsqF_life_f(i_qp,iq_mem,il,iw)+pi*elph_gkkp_sq*q_weight(iq_loop)*&
&               ( ( Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) - &
&               QP_time_order_sign* Fermi_fnc_derivative(real(delta_E)-ph_E,gsqF_damping) )*f_occ-&
&               Fermi_fnc_derivative(real(delta_E)+ph_E,gsqF_damping) )
           !
           if (eval_G_using_KK) cycle
           !
           ! QP's energies
           !--------------
           !
           ! Correction to the Fan-DW term (not in Cardona-Allen paper, possibly
           ! important for metals).
           !
           gsqF_ca_corr(i_qp,iq_mem,il,iw)=gsqF_ca_corr(i_qp,iq_mem,il,iw)+elph_gkkp_sq*&
&                                              ph_E*one_minus_2f_occ/(delta_E**2-ph_E**2)*q_weight(iq_loop)
           !
           ! Cardona-Allen formulation
           !
           ! (a) Fan Term                                          this 2 is to be consistent with AC definition
           !                                                       |
           !                                                      \/
           gsqF_fan(i_qp,iq_mem,il,iw)=gsqF_fan(i_qp,iq_mem,il,iw)+2._SP*elph_gkkp_sq*&
&                                      delta_E/(delta_E**2-ph_E**2)*q_weight(iq_loop)
           !
         enddo
         !
         ! The DW term comes from a perturbation theory expansion
         ! which does not allow zero-energy transitions between the
         ! perturbed stated and itself
         !
         if (abs(delta_E_at_gamma)<DW_deltaE_treshold) cycle
         !
         ! (b) Debye Waller Term
         !
         gsqF_dw(i_qp,iq_mem,il)=gsqF_dw(i_qp,iq_mem,il)-1._SP/2._SP*&
&                                    2._SP*elph_DW(il,ib,ob,ik_bz)/delta_E_at_gamma*q_weight(iq_loop)/2._SP/ph_E
         !                          /\
         !                           |
         !                           this 2 is to be consistent with AC definition (see Eq. 5, PRB 31, 2163)
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
   enddo
 enddo
 !
 call live_timing()
 !
 ! Debye energy
 !
 call msg('nr','[Ph] Debye energy         ',(/W_debye*HA2EV,W_debye*HA2THZ/),"[ev/ThZ]")
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
   ! AndreaM, March 2015.
   !
   ! In the iq_loop loop the q=1 is read by ALL cpu's. This is in order
   ! to get the DW elements written only for q=0. This means that all cpu's has the ph_freqs_sq(1,:)
   ! 
   ph_freqs_sq(1,:)=ph_freqs_sq(1,:)/PAR_COM_Q_INDEX%n_CPU
   !
   ! AndreaM, Feb 2017
   !
   ! In addition to the gamma point also other points can be read by 2 cpu's in the case of uniform grids
   ! as the BZ points are distributed.
   ! The only case where the Q is read only by one cpu (of the PAR_COM_Q_A2A comm) is when all the star is assigned to 
   ! that cpu
   !
   if (elph_use_q_grid) then
     !
     q_each_cpu=0
     do iq_ibz=2,q%nibz
       do iq_s=1,q%nstar(iq_ibz)
         if ( PAR_IND_Q_bz%element_1D(sum(q%nstar(:iq_ibz-1))+iq_s) ) q_each_cpu(iq_ibz)=1
       enddo
     enddo
     !
     call PP_redux_wait(q_each_cpu,COMM=PAR_COM_Q_INDEX%COMM)
     !
     do iq_ibz=2,q%nibz
       if (q_each_cpu(iq_ibz)>0) ph_freqs_sq(iq_ibz,:)=ph_freqs_sq(iq_ibz,:)/q_each_cpu(iq_ibz)
     enddo
     !
   endif
   !
   call PP_redux_wait(ph_freqs_sq,COMM=PAR_COM_Q_INDEX%COMM)
   !
   iq_range=(/nq_to_sum,1/)
   do iq_loop=1,nq_to_sum
     if (PAR_Q_bz_index(iq_loop)>0) then
       iq_range(1)=min(iq_range(1),iq_loop)
       iq_range(2)=max(iq_range(2),iq_loop)
     endif
   enddo
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=master_cpu)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_ELPH(ID,'gFsq')
   !
   call PP_redux_wait
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_Q_A2A%CPU_id==0)
   !
   do iq_loop=iq_range(1),iq_range(2)
     call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/iq_loop+1/),ID=ID)
     io_err=io_ELPH(ID,'gFsq')
   enddo
   !
   call IO_and_Messaging_switch("+io_out",CONDITION=master_cpu)
   !
 endif
 !
 ! CLEAN
 !
 call elph_global_free()
 YAMBO_FREE(q_each_cpu)
 YAMBO_FREE(q_weight)
 do i_qp=1,qp%n_states
   call W_reset(Sc_W(i_qp))
 enddo
 call timing('GW(e-p)',OPR='stop')
 !
end subroutine
