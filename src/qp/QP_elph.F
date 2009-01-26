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
subroutine QP_elph(en,k,q,qp)
 !
 ! This routine calculates the QP shifts due to el-ph intercation
 ! following the Allen-Cardona formulation (see for example 
 ! PRB 23, 1495 (1981) )
 !
 use pars,          ONLY:SP,DP,schlen,pi
 use units,         ONLY:HARTREE,HA2THZ
 use parser_m,      ONLY:parser
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp,W_reset
 use electrons,     ONLY:levels,spin_occ
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error,warning
 use drivers,       ONLY:Finite_Tel,l_el_corr
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END,OP_WR_CL
 use QP_m,          ONLY:QP_t,QP_G_damp,QP_Sc,QP_n_states,QP_table,&
&                        QP_dSc_steps,QP_dSc_delta
 use D_lattice,     ONLY:sop_inv,nsym,i_time_rev,sop_tab
 use R_lattice,     ONLY:qindx_S,bz_samp,ik_is_table
 use ELPH,          ONLY:ph_modes,elph_nb,ph_freqs_sq,elph_gkkp,&
&                        elph_global_free,QP_PH_n_G_bands,max_ph_freq,elph_DW,&
&                        elph_nq,E_k_plus_q,elph_use_q_grid,&
&                        setup_k_plus_q_levels,f_k_plus_q,PH_null_E_rgn,elph_nq_used,&
&                        gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f ,&
&                        elph_global_alloc
 use functions,     ONLY:Fermi_fnc_derivative
 !
 implicit none
 type(levels)  ::en
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 ! WorkSpace  
 !
 integer          ::iq_db,iqbz,iq,iq_ref,iqs,ik,ok,ik_bz,okbz,ib,ob,is,il,iqp,i2,&
&                   time_order_sign,ik_bz_gkkp,ib_gkkp,ob_gkkp,os,incomplete_Q_grid
 type(w_samp)     ::Sc_W(qp%n_states)
 type(pp_indexes) ::px
 integer          ::io_err,ID,iv4(4)
 integer, external::ioELPH,QP_state_extract
 character(schlen)::ch
 real(SP)         ::q_weight,ph_E,delta_E,delta_E_at_gamma,one_minus_2f_occ,f_occ,&
&                   E_k_plus_q_at_gamma(elph_nb,k%nbz),elph_gkkp_sq
 logical          ::l_WRgFsq
 !
 ! Zeroing
 !
 call pp_indexes_reset(px)
 do iqp=1,qp%n_states
   call W_reset(Sc_W(iqp))
 enddo
 !
 if (l_el_corr) then
   call section('+','Correlation: Phonon Self-energy')
 else
   call section('=','Correlation: Phonon Self-energy')
 endif
 !
 ! gFsq coefficients 2 DB ?
 !
 call parser('WRgFsq',l_WRgFsq)
 !
 call k_sym2sym(k,'k')
 call k_expand(k)
 !
 if (QP_PH_n_G_bands<=0.or.QP_PH_n_G_bands>elph_nb) QP_PH_n_G_bands=elph_nb
 !
 call msg('r', '[GW/El-Ph] Bands range       :',(/1,QP_PH_n_G_bands/))
 call msg('r', '[GW/El-Ph] G damping     [ev]:',QP_G_damp*HARTREE)
 call msg('r','')
 !
 iv4=(/1,1,0,0/)
 do while(QP_state_extract(iv4)>0)
   write (ch,'(4(a,i3.3))') 'QP @ K ',iv4(1),' - ',iv4(2),' : b ',iv4(3),' - ',iv4(4)
   call msg('r',trim(ch))
 enddo
 !
 call msg('r','')
 !
 ! ELPH DB
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp')
 if (io_err/=0) call error('El-Ph database not found')
 !
 !Sc Energy points
 !
 do iqp=1,qp%n_states
   Sc_W(iqp)%n=QP_dSc_steps
   allocate(Sc_W(iqp)%p(Sc_W(iqp)%n(1)))
   forall (i2=1:QP_dSc_steps) Sc_W(iqp)%p(i2)=&
&         en%E(QP_table(iqp,1),QP_table(iqp,3),1)+(i2-1)*QP_dSc_delta+&
&         cmplx(0.,QP_G_damp,SP)
 enddo
 !
 iq_ref=0
 !
 time_order_sign=-1
 if (Finite_Tel) time_order_sign=1
 !
 if (elph_use_q_grid) elph_nq_used=q%nbz
 q_weight=1./real(elph_nq_used)
 !
 ! g^2F(w) residuals
 !
 call elph_global_alloc('gFsq')
 !
 incomplete_Q_grid=0
 !
 call par_indexes(px,(/elph_nq_used,QP_PH_n_G_bands/))
 call pp_redux_wait
 !
 do iq_db=1,elph_nq_used
   !
   iqbz = iq_db
   iq   = iq_db
   if (elph_use_q_grid) then
     iqbz=iq_db
     iq =q%sstar(iqbz,1)
     iqs=q%sstar(iqbz,2)
   endif
   !
   if (iq/=iabs(iq_ref)) then
     !
     call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
     io_err=ioELPH(ID,'gkkp')
     iq_ref=iq
     if (io_err<0) iq_ref=-iq
     !
     if (.not.elph_use_q_grid) call setup_k_plus_q_levels(en%Efermi(1))
     !
     ! I call live_timing here as in ioELPH the global_alloc() can 
     ! send a screen message about the allocated memory that can interferee
     ! with the live_timing hashes
     !
     if (iq==1) then
       call live_timing('El-Ph Sc [coeff]',px%stps(myid+1)*QP_n_states)
       if (.not.elph_use_q_grid) E_k_plus_q_at_gamma(:,:)=E_k_plus_q(:,:,1)
     endif
     !
   endif
   !
   if (iq_ref<0) then
     call live_timing(steps=QP_n_states*QP_PH_n_G_bands)
     if (incomplete_Q_grid==0) incomplete_Q_grid=iabs(iq_ref)
     cycle
   endif
   !
   do iqp=1,QP_n_states
     !
     ib   =QP_table(iqp,1)
     ik   =QP_table(iqp,3)
     ik_bz=sum(k%nstar(:ik-1))+1
     okbz=0
     ok  =0
     !
     do ob=1,QP_PH_n_G_bands
       !
       if (.not.px%i2p(iqbz,ob)) cycle
       !
       ik_bz_gkkp=ik_bz
       ib_gkkp   =ib
       ob_gkkp   =ob
       !
       if (elph_use_q_grid) then
         okbz=qindx_S(ik,iqbz,1)
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
         ik_bz_gkkp=ik_is_table(ik,sop_inv(iqs))
         ! 
         ! gkkp_{I_need}= <k+IRq n'|dV_{SCF}/du^{IRq nu}|k n>=
         !                [<(R^-1 S p)+q n|dV_{SCF}/du^{q nu}|(R^-1 S p) n'>]^*= 
         !                [gkkp(ik_bz,nu,n,n')]^*
         ! 
         ! with k + IRq = Sp 
         !
         if (iqs>nsym/(i_time_rev+1)) then
           ib_gkkp   =ob
           ob_gkkp   =ib
           ik_bz_gkkp=ik_is_table(ok,sop_inv(sop_tab(iqs-nsym/2,os)))
         endif
         !
         ! k is in the IBZ and q is the PW_q (=-YAMBO_q)
         !
       endif
       !
       if (elph_use_q_grid) then
         delta_E=en%E(ib,ik,1)-en%E(ob,ok,1)
         delta_E_at_gamma=en%E(ib,ik,1)-en%E(ob,ik,1)
         f_occ=en%f(ob,ok,1)/spin_occ
         one_minus_2f_occ=(1.-2.*f_occ)
       else
         delta_E=en%E(ib,ik,1)-E_k_plus_q(ob,ik_bz,1)
         delta_E_at_gamma=en%E(ib,ik,1)-E_k_plus_q_at_gamma(ob,ik_bz)
         f_occ=f_k_plus_q(ob,ik_bz,1)/spin_occ
         one_minus_2f_occ=(1.-2.*f_occ)
       endif
       !
       do il=1,ph_modes
         !
         ! Skip modes @ Gamma
         !
         if (iq==1) cycle
         !
         ph_E=sqrt(abs(ph_freqs_sq(iq,il)))
         !
         ! In the SE expression I have the m.e. 
         !
         !  <ib ik|g(q_YAMBO l r)|ob ik-q_YAMBO> = [<ob ik+q_PW|g(q_PW l r)|ib ik>]^* = 
         !                              elph_gkkp(ik_bz_gkkp,il,ob_gkkp,ib_gkkp)^*
         !
         ! with q_YAMBO = - q_PW
         !
         elph_gkkp_sq=conjg(elph_gkkp(ik_bz_gkkp,il,ob_gkkp,ib_gkkp))*&
&                           elph_gkkp(ik_bz_gkkp,il,ob_gkkp,ib_gkkp)/2./ph_E
         !
         ! Lifetimes
         !-----------
         !
         ! "Bose" Term
         !
         gsqF_life_bose(iqp,iq,il)= gsqF_life_bose(iqp,iq,il)+2.*pi*elph_gkkp_sq*q_weight*&
&          ( Fermi_fnc_derivative(delta_E+ph_E,QP_G_damp) + &
&            time_order_sign* Fermi_fnc_derivative(delta_E-ph_E,QP_G_damp) )
         !
         ! "f" Term
         !
         gsqF_life_f(iqp,iq,il)= gsqF_life_f(iqp,iq,il)+pi*elph_gkkp_sq*q_weight*&
&          ( ( Fermi_fnc_derivative(delta_E+ph_E,QP_G_damp) - &
&              time_order_sign* Fermi_fnc_derivative(delta_E-ph_E,QP_G_damp) )*f_occ-&
&            Fermi_fnc_derivative(delta_E+ph_E,QP_G_damp) )
         !
         ! QP's energies
         !--------------
         !
         ! Security cycle to avoid divergencies in the denominators
         !
         if (abs(delta_E/ph_E)-1.<0.01) cycle
         !
         ! Correction to the Fan-DW term (not in Cardona-Allen paper, possibly
         ! important for metals).
         !
         if (abs(delta_E)>PH_null_E_rgn) &
&          gsqF_ca_corr(iqp,iq,il)= gsqF_ca_corr(iqp,iq,il)+elph_gkkp_sq*&
&                  ph_E*one_minus_2f_occ/(delta_E**2.-ph_E**2.)*q_weight
         !
         ! Cardona-Allen formulation
         !
         ! The Energy term contributing to gsqF_fan/dw
         ! is and odd function of E-E'. So it should integrate
         ! to zero for E\simE'. This defines the PH_null_E_rgn treshold
         !
         ! (a) Fan Term
         !
         if (abs(delta_E)>PH_null_E_rgn) &
&          gsqF_fan(iqp,iq,il)=gsqF_fan(iqp,iq,il)+2.*elph_gkkp_sq*&
&                  delta_E/(delta_E**2.-ph_E**2.)*q_weight
         !
         ! (b) Debye Waller Term
         !
         if (abs(delta_E_at_gamma)>PH_null_E_rgn) &
&          gsqF_dw(iqp,iq,il)= gsqF_dw(iqp,iq,il)-1./2.*&
&                  2.*elph_DW(ik_bz,il,ib,ob)/delta_E_at_gamma*q_weight/2./ph_E
         !
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
 ! If any Q point was missing give a warning
 !
 if (incomplete_Q_grid/=0) call &
&    warning('Incomplete Q grid ! DBs @ IQ '//trim(intc(incomplete_Q_grid))//' missing')
 !
 ! Debye energy
 !
 call msg('nr','[Ph] Debye energy [ev/ThZ]:',(/max_ph_freq*HARTREE,max_ph_freq*HA2THZ/))
 !
 ! All 2 All
 !
 call pp_redux_wait(QP_Sc)
 call pp_redux_wait(gsqF_dw(:,:,:))
 call pp_redux_wait(gsqF_fan(:,:,:))
 call pp_redux_wait(gsqF_ca_corr(:,:,:))
 call pp_redux_wait(gsqF_life_bose(:,:,:))
 call pp_redux_wait(gsqF_life_f(:,:,:))
 !
 ! Cardona-Allen g^2 F function and its integrated value
 !
 call Cardona_Allen_report()
 !
 ! Dump on file gFsq coefficients 
 !
 if (l_WRgFsq) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioELPH(ID,'gFsq')
 endif
 !
 ! CLEAN
 !
 call elph_global_free()
 call pp_indexes_reset(px)
 !
 contains
   !
   subroutine Cardona_Allen_report()
     !
     use functions,  ONLY:bose_f
     complex(DP)        ::DP_corr
     real(SP)           ::ph_freq
     logical            ::state_is_2do(QP_n_states)
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
     call live_timing('El-Ph Sc   [sum]',elph_nq_used*QP_n_states)
     !
     do iqp=1,QP_n_states  
       !
       DP_corr=cmplx(0.,0.,DP)
       !
       do iq=1,elph_nq_used
         do il=1,ph_modes
           !
           if (.not.elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq(iq,il))
           if (     elph_use_q_grid) ph_freq=sqrt(ph_freqs_sq( q%sstar(iq,1) ,il))
           !
           ! QP lifetimes
           !--------------
           DP_corr=DP_corr+(0.,1.)*(gsqF_life_bose(iqp,iq,il)*(bose_f(ph_freq)+1.)/2.+&
&                          gsqF_life_f(iqp,iq,il))
           !
           ! QP energies
           !-------------
           !
           ! Integrate g^2 F function (QP correction)
           ! 
           DP_corr=DP_corr+(gsqF_fan(iqp,iq,il)+gsqF_dw(iqp,iq,il))*(bose_f(ph_freq)+1.)/2.+&
&                          gsqF_ca_corr(iqp,iq,il)
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo
       !
       QP_Sc(iqp,:)=QP_Sc(iqp,:)+DP_corr
       !
     enddo
     !
     call live_timing()
     !
   end subroutine
   !
end subroutine
