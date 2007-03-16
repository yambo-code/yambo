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
 subroutine QP_elph(en,k,q,qp)
 !
 ! This routine calculates the QP shifts due to el-ph intercation
 ! following the Allen-Cardona formulation (see for example 
 ! PRB 23, 1495 (1981) )
 !
 use pars,          ONLY:SP,DP,schlen,HARTREE,HA2THZ
 use frequency,     ONLY:w_samp,W_reset
 use electrons,     ONLY:levels
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error
 use drivers,       ONLY:Finite_Tel,l_el_corr
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset,master_node
 use par_indexes_m, ONLY:par_indexes
 use functions,     ONLY:bose_f
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END
 use QP_m,          ONLY:QP_t,QP_G_damp,QP_Sc,QP_n_states,QP_table,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,QP_solver,QP_dSc_steps,&
&                        QP_dSc_delta
 use D_lattice,     ONLY:sop_inv
 use R_lattice,     ONLY:qindx_S,bz_samp,ik_is_table,q_norm
 use ELPH,          ONLY:ph_modes,elph_nb,ph_freqs_sq,elph_gkkp,&
&                        elph_gkkp_free,QP_PH_n_G_bands,max_ph_freq,elph_DW,&
&                        PH_damping,elph_nq,E_k_plus_q,elph_use_q_grid,&
&                        setup_k_plus_q_levels,f_k_plus_q,PH_null_E_rgn
!DEBUG >
 use parser_m, ONLY:parser
!DEBUG <
 implicit none
 type(levels)  ::en
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 ! WorkSpace  -- GPL_END --
 !
 integer          ::iq_db,iqbz,iq,iq_ref,iqs,ik,ok,ikbz,okbz,ib,ob,is,il,iqp,i2,iw
 type(w_samp)     ::Sc_W(qp%n_states)
 type(pp_indexes) ::px
 integer          ::io_err,ID,iv4(4),nq_to_loop
 integer, external::ioELPH,QP_state_extract
 character(schlen)::ch
 real(SP)         ::q_weight,ph_energy
 complex(SP)      ::DP_corr
 real(SP),allocatable :: g_sq_F_res(:,:,:,:)
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
 call k_sym2sym(k,'k')
 call k_expand(k)
 !
 if (QP_PH_n_G_bands<=0.or.QP_PH_n_G_bands>elph_nb) QP_PH_n_G_bands=elph_nb
 !
 call msg('r', '[GW/El-Ph] Bands range       :',(/1,QP_PH_n_G_bands/))
 call msg('r', '[GW/El-Ph] PH damping   [mev]:',PH_damping*HARTREE*1000.)
 if (trim(QP_solver)/="g") call msg('r', &
&              '[GW/El-Ph] G damping     [ev]:',QP_G_damp*HARTREE)
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
 io_err=ioELPH(ID)
 if (io_err/=0) call error('El-Ph database not found')
 !
 !Sc Energy points
 !
 if (trim(QP_solver)=='n') then
   do iqp=1,qp%n_states
     Sc_W(iqp)%n=QP_dSc_steps
     allocate(Sc_W(iqp)%p(Sc_W(iqp)%n(1)))
     forall (i2=1:QP_dSc_steps) Sc_W(iqp)%p(i2)=&
&           en%E(QP_table(iqp,1),QP_table(iqp,3),1)+(i2-1)*QP_dSc_delta+&
&           cmplx(0.,QP_G_damp,SP)
   enddo
 else if (trim(QP_solver)=='g') then
   do iqp=1,qp%n_states
     Sc_W(iqp)%n=QP_Sc_steps
     Sc_W(iqp)%er=QP_G_er
     Sc_W(iqp)%dr=QP_G_dr
     call freqs_setup(Sc_W(iqp))
     QP_Sc_steps=Sc_W(iqp)%n(1)
   enddo
 endif
 !
 iq_ref=0
 !
 nq_to_loop=q%nbz
 if (.not.elph_use_q_grid) nq_to_loop=elph_nq
!DEBUG >
 call parser('nq_to_loop',nq_to_loop)
!DEBUG<
 q_weight=1./real(nq_to_loop)
 !
 allocate(g_sq_F_res(QP_n_states,nq_to_loop,ph_modes,2))
 g_sq_F_res=0.
 !
 call par_indexes(px,(/nq_to_loop,QP_PH_n_G_bands/))
 call pp_redux_wait
 !
 do iq_db=1,nq_to_loop
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
     call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
     io_err=ioELPH(ID)
     iq_ref=iq
     if (io_err<0) iq_ref=-iq
     !
     if (.not.elph_use_q_grid) call setup_k_plus_q_levels(en%Efermi(1))
     !
     ! I call live_timing here as in ioELPH the elph_alloc() can 
     ! send a screen message about the allocated memory that can interferee
     ! with the live_timing hashes
     !
     if (iq==1) call live_timing('El-Ph Sc [1]',px%stps(myid+1)*QP_n_states)
     !
   endif
   !
   if (iq_ref<0) then
     call live_timing(steps=QP_n_states)
     cycle
   endif
   !
   do iqp=1,QP_n_states
     !
     ib  =QP_table(iqp,1)
     ik  =QP_table(iqp,3)
     ikbz=sum(k%nstar(:ik-1))+1
     okbz=0
     ok  =0
     if (elph_use_q_grid) then
       okbz=qindx_S(ik,iqbz,1)
       ok=k%sstar(okbz,1)
       ! 
       ! When using a uniform Q grid I cycle on the q symmetries 
       ! as well. To rotate the gkkp m.e. I use:
       ! 
       ! <k-Rq n|dV_{SCF}/du^{q nu}|k n'>=
       ! <(R^-1 k)-q n|dV_{SCF}/du^{q nu}|(R^-1 k) n'>= gkkp(ikbz,nu,n,n' )
       !
       ikbz=ik_is_table(ik,sop_inv(iqs))
       !
     endif
     !                 
     DP_corr=cmplx(0.,0.,SP)
     !
     do ob=1,QP_PH_n_G_bands
       !
       if (.not.px%i2p(iqbz,ob)) cycle
       !
       do il=1,ph_modes
         !
         ! Skip Acoustic modes @ Gamma
         !
         if (iq==1.and.il<=3) cycle
         !
         ph_energy=sqrt(abs(ph_freqs_sq(iq,il)))
         !
         ! Cardona-Allen formulation
         !---------------------------
         !
         ! Fan Term
         !
         g_sq_F_res(iqp,iq,il,1)= g_sq_F_res(iqp,iq,il,1)+&
&                  conjg(elph_gkkp(ikbz,il,ob,ib))*elph_gkkp(ikbz,il,ob,ib)*&
&                  Gf_real(ib,ik,ikbz,ob,ok,okbz,ph_energy,1)*q_weight/2./ph_energy
         !
         g_sq_F_res(iqp,iq,il,2)= g_sq_F_res(iqp,iq,il,2)+&
&                  conjg(elph_gkkp(ikbz,il,ob,ib))*elph_gkkp(ikbz,il,ob,ib)*&
&                  Gf_real(ib,ik,ikbz,ob,ok,okbz,ph_energy,2)*q_weight/2./ph_energy
         !
         ! Debye Waller Term
         !
         g_sq_F_res(iqp,iq,il,1)= g_sq_F_res(iqp,iq,il,1)-1./2.*&
&                  elph_DW(ikbz,il,ob,ib)*Gf_real(ib,ik,ikbz,ob,ik,ikbz,0.,1)*&
&                  q_weight/ph_energy
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
 ! Debye energy
 !
 call msg('nr','[Ph] Debye energy [ev/ThZ]:',(/max_ph_freq*HARTREE,max_ph_freq*HA2THZ/))
 !
 ! All 2 All
 !
 call pp_redux_wait(QP_Sc)
 call pp_redux_wait(g_sq_F_res(:,:,:,1))
 call pp_redux_wait(g_sq_F_res(:,:,:,2))
 !
 ! Cardona-Allen g^2 F function and its integrated value
 !
 call Cardona_Allen_report()
 !
 ! CLEAN
 !
 call elph_gkkp_free()
 call pp_indexes_reset(px)
 deallocate(g_sq_F_res)
 !
 contains
   !
   pure function Gf_real(i_b,ik_ibz,ik_bz,o_b,ok_ibz,ok_bz,ph_E,i_typ)
     !
     implicit none
     real(SP)            ::Gf_real
     real(SP),intent(in) ::ph_E
     integer, intent(in) ::i_b,ik_ibz,ik_bz,o_b,ok_ibz,ok_bz,i_typ
     !
     ! Work Space
     !
     integer      ::i_rand,ok_rand
     real(SP)     ::delta_E,occupation_factor
     !
     Gf_real=0._SP
     !
     if (elph_use_q_grid)      delta_E=en%E(i_b,ik_ibz,1)-en%E(o_b,ok_ibz,1)
     if (elph_use_q_grid)      occupation_factor=(2.-en%f(o_b,ok_ibz,1))
     if (.not.elph_use_q_grid) delta_E=en%E(i_b,ik_ibz,1)-E_k_plus_q(o_b,ik_bz,1)
     if (.not.elph_use_q_grid) occupation_factor=(2.-f_k_plus_q(o_b,ik_bz,1))/2.
     if (i_typ==1) Gf_real=delta_E/(delta_E**2.-ph_E**2.)
     !
     ! The type 1 Gf is and odd function of E-E'. So it should integrate
     ! to zero for E\simE'. This defines the PH_null_E_rgn treshold
     !
     if (i_typ==1.and.abs(delta_E)<PH_null_E_rgn) Gf_real=0.
     if (i_typ==2) Gf_real=ph_E*occupation_factor/(delta_E**2.-ph_E**2.)
     !
     return
     !
   end function
   !
   subroutine Cardona_Allen_report()
     !
     use functions,  ONLY:Fermi_fnc_derivative,bose_f
     use stderr,     ONLY:intc
     use com,        ONLY:msg,of_open_close
     !
     integer, parameter ::n_g_sq_freq=500
     !
     real(SP)     ::g_sq_F(n_g_sq_freq),delta_E,ph_freq
     type(w_samp) ::g_sq_F_E
     !
     call W_reset(g_sq_F_E)
     g_sq_F_E%n=n_g_sq_freq
     g_sq_F_E%er=(/0._SP,max_ph_freq*1.1/)
     g_sq_F_E%dr=PH_damping
     if (PH_damping>0.) call freqs_setup(g_sq_F_E)
     !
     ! Degenerate bands average
     !
     do iq=1,nq_to_loop
       do il=1,ph_modes
         call QP_states_simmetrize(g_sq_F_res(:,iq,il,1),en)
       enddo
     enddo
     !
     call live_timing('El-Ph Sc [2]',nq_to_loop*QP_n_states)
     !
     do iqp=1,QP_n_states  
       !
       g_sq_F=0.
       DP_corr=0._DP
       !
       do iq=1,nq_to_loop
         do il=1,ph_modes
           !
           ph_freq=sqrt(ph_freqs_sq(iq,il))
           !
           ! Integrate g^2 F function (QP correction)
           ! 
           DP_corr=DP_corr+g_sq_F_res(iqp,iq,il,1)*(bose_f(ph_freq)+1.)+g_sq_F_res(iqp,iq,il,2)
           !
           ! g^2 F function
           !
           if (.not.PH_damping>0.) cycle
           !
           do iw=1,n_g_sq_freq
             !
             delta_E=real(g_sq_F_E%p(iw))-ph_freq
             !
             g_sq_F(iw)=g_sq_F(iw)+(g_sq_F_res(iqp,iq,il,1)+g_sq_F_res(iqp,iq,il,2))*&
&                       Fermi_fnc_derivative(delta_E,aimag(g_sq_F_E%p(iw)))
             !
           enddo
         enddo
         !
         call live_timing(steps=1)
         !
       enddo
       !
       QP_Sc(iqp,:)=QP_Sc(iqp,:)+DP_corr
       !
       if (.not.PH_damping>0.) cycle
       !
       ch='g_sq_F_b_'//trim(intc(QP_table(iqp,1)))//'_k_'//trim(intc(QP_table(iqp,3)))
       call of_open_close(trim(ch),'ot')
       call msg('o g_sq','#',(/'E  [meV]','g^2 F(E)'/),INDENT=0,USE_TABS=.true.)
       call msg('o g_sq','#')
       !
       do iw=1,n_g_sq_freq
         call msg('o g_sq','',&
&                (/real(g_sq_F_E%p(iw))*HARTREE*1000.,g_sq_F(iw)/),&
&                INDENT=-2,USE_TABS=.TRUE.)
       enddo
       call of_open_close(trim(ch))
       !
     enddo
     !
     call live_timing()
     !
     ! Clean
     !
     if (PH_damping>0.) call W_reset(g_sq_F_E)
     !
   end subroutine
   !
   ! -- GPL_START --
   !
 end subroutine
