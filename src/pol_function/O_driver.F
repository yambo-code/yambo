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
subroutine O_driver(Xen,Xk,q,wv,X)
 ! 
 ! Optics driver.
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:pi,SP,HARTREE,schlen
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:intc
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc,l_col_cut
 use frequency,     ONLY:w_samp,rg_index_bg,bg_npts,cg_pt,cg_index_bg
 use electrons,     ONLY:levels,BZ_RIM_tot_nkpts
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg
 use com,           ONLY:msg,of_open_close
 use vec_operate,   ONLY:v_norm
 use X_m,           ONLY:X_t,X_epsilon,X_mat,X_alloc,X_poles_tab,X_fxc,&
&                        alpha_dim,eps_2_alpha,use_X_RIM
 use par_proc_m,    ONLY:pp_redux_wait
 use wave_func,     ONLY:WF_free
 use TDDFT,         ONLY:FXC_n_descs,FXC_description,FXC_type,&
&                        FXC,FXC_K_diagonal,FXC_n_g_corr
 use global_XC,     ONLY:X_WF_xc_string,X_E_xc_string
 !
 implicit none
 type(levels)::Xen
 type(bz_samp) ::Xk,q
 type(X_t):: X
 type(w_samp):: wv 
 ! 
 ! Work Space
 !
 integer          :: i1,i2,fr(2),iq
 real(SP)         :: od(5)
 character(10)    :: headings(10)
 character(schlen):: ch,of_name(4)
 logical          :: lnoe0
 !
 call section('*','Optics')
 !
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]:',eps_2_alpha)
   call msg('rn','Polarizability dimension      :',trim(alpha_dim))
 endif
 !
 ! Basical Checks
 !
 call X_pre_setup(Xen,X)
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! TDDFT setup
   !
   call tddft_do_X_W_typs(iq,X,wv)
   if (l_alda_fxc) call tddft_alda_g_space(Xen,Xk)
   !
   ! In TDDFT with BS based kernel eps0 is non-sense
   ! as shifted of the K diagonal
   !
   lnoe0=l_bs_fxc
   !
   ! Arrays to be written in the o. files
   !
   allocate(X_epsilon(8,wv%n(1)))
   if (l_bs_fxc) then
     allocate(X_fxc(wv%n(1)))
     X_fxc=(0._SP,0._SP)
   endif
   !
   ! X_mat allocation
   !
   call X_alloc('X',(/X%ng,X%ng,wv%n(2)/))
   !
   ! Frequencies (AFTER TDDFT SETUP!)
   !
   call freqs_setup(wv)
   !
   ! OutPut files...
   !
   of_name(:)=' '
   !
   ! ... epsilon ...
   !
   write (of_name(1),'(a,i3.3,a)') 'eps_q',iq,trim(FXC_type)
   call of_open_close(of_name(1),'ot')
   !
   ! ... fxc ...
   !
   if (l_bs_fxc) then
     write (of_name(3),'(a,i3.3)')   'bs_fxc_q',iq
     call of_open_close(of_name(3),'ot')
   endif
   !
   ! ... eels ...
   !
   if (index(X%ordering,'r')==0) then
     write (of_name(2),'(a,i3.3,a)') 'eel_q',iq,trim(FXC_type)
     call of_open_close(of_name(2),'ot')
   endif
   !
   ! ... alpha ...
   !
   if (trim(alpha_dim)/='adim') then
     write (of_name(4),'(a,i3.3,a)') 'alpha_q',iq,trim(FXC_type)
     call of_open_close(of_name(4),'ot')
   endif
   !
   ! Titles 
   !
   ch = '# Absorption @ IQ '//trim(intc(iq))//' [iku]:' 
   if (iq==1) ch = '# Absorption @ IQ '//trim(intc(iq))//' [q->0 dir]:'
   if (iq==1) call msg("o eps",trim(ch),X%q0/v_norm(X%q0),INDENT=0)
   if (iq/=1) call msg("o eps",trim(ch),q%pt(iq,:),INDENT=0)
   ch = '# EELS @ IQ '//trim(intc(iq))//' [iku]:'
   if (iq==1) ch = '# EELS @ IQ '//trim(intc(iq))//' [q->0 dir]:'
   if (iq==1) call msg("o eel",trim(ch),X%q0/v_norm(X%q0),INDENT=0)
   if (iq/=1) call msg("o eel",trim(ch),q%pt(iq,:),INDENT=0)
   ch = '# Polarizability ( -<<X>> ) ['//&
&       trim(alpha_dim)//'] @ IQ '//trim(intc(iq))//' [iku]:'
   if (iq==1) call msg("o alpha",trim(ch),X%q0/v_norm(X%q0),INDENT=0)
   if (iq/=1) call msg("o alpha",trim(ch),q%pt(iq,:),INDENT=0)
   !
   if (l_bs_fxc) then
     call msg('o eps eel alpha','#')
     do i1=1,FXC_n_descs
       call msg('o eps eel fxc alpha','# ',trim(FXC_description(i1)),INDENT=0)    
     enddo
     call msg('o eps eel alpha fxc','#')
     ch=' TDDFT|Fxc size             :'//trim(intc(FXC_n_g_corr))
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
     ch='      |Hartree size         :'//trim(intc(X%ng))
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
     ch='      |Ordering             :'//trim(X%ordering)
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
     !
     ! Fxc file titles
     !
     headings(1)  ='E/ev[1]'
     headings(2:3)=(/'q^2*Fxc/Re','q^2*Fxc/Im'/)
     call msg('o fxc','#',headings(:3),INDENT=0,USE_TABS=.true.)    
     call msg('o fxc','#')
     !
   endif
   !
   ! BZ RIM
   !
   if (use_X_RIM) then
     call msg('o eps eel fxc alpha','#')
     call msg('o eps eel fxc alpha','# BZ Energy RIM points :',BZ_RIM_tot_nkpts,INDENT=0)
   endif
   !
   call pp_redux_wait
   !
   call of_open_close(of_name(1))
   call of_open_close(of_name(2))
   call of_open_close(of_name(3))
   call of_open_close(of_name(4))
   !
   ! e Table
   ! 1:e0 2:e 3:eel0 4:eel 5:alpha0 6:alpha 
   !
   X_epsilon=(0.,0.)
   do i1=1,wv%n(1),wv%n(2)
     fr=(/i1,i1+wv%n(2)-1/)
     call X_os(X_mat,iq,fr,Xen,Xk,wv,X)     
     !
     ! Without LF eps/alpha
     !
     X_epsilon(1,fr(1):fr(2))=1.0_SP-X_mat(1,1,:)*4.0_SP*pi/q_norm(iq)**2
     !
     ! ... for alpha_0 (2) and (3) coincides
     !
     X_epsilon(5,fr(1):fr(2))=(X_epsilon(1,fr(1):fr(2))-1.)/4./pi*eps_2_alpha  !(3)
     !
     ! X Dyson equation solver
     !
     call X_s(iq,fr,X,wv)
     !
     X_epsilon(2,fr(1):fr(2))=1./(X_mat(1,1,:)*bare_qpg(iq,1)**2/q_norm(iq)**2+1.)
     X_epsilon(3,fr(1):fr(2))=-1./X_epsilon(1,:)
     X_epsilon(4,fr(1):fr(2))=-1./X_epsilon(2,:)
     !
     ! This relation is exact
     !
     ! alpha = -eps_2_alpha X(1,1) / |q|^2      (1)
     !       =  (1 -eps_M^-1) eps_2_alpha/4/pi  (2)
     !
     ! and BOTH need the antiresonant part of X ( or eps_M(w) )
     !
     ! In the limit of eps_2_alpha (or the volume) -> \infty we get
     !
     ! eps_M^-1 \sim 1 - 4 pi /|q|^2 X(1,1)
     !
     ! so that
     !
     ! alpha=(epsilon_M-1) eps_2_alpha/4/pi (3)
     !
     ! this does not need the antiresonant part of X. Now when using the 
     ! columbian cutoff OR when the Green's function is not resonant or
     ! antiresonant only I use the full expression
     !
     if (l_col_cut.or.(trim(X%ordering)/='a'.and.trim(X%ordering)/='r')) then
       !
       X_epsilon(6,fr(1):fr(2))=(1.-1./X_epsilon(2,fr(1):fr(2)))/4./pi*eps_2_alpha ! (2)
       !
     else
       !
       X_epsilon(6,fr(1):fr(2))=(X_epsilon(2,fr(1):fr(2))-1._SP)*eps_2_alpha/4./pi ! (3)
       !
     endif
     !
     call of_open_close(of_name(1),'oa')
     call of_open_close(of_name(2),'oa')
     call of_open_close(of_name(4),'oa')
     !
     if (i1==1) then
       !
       ! T-ordering
       !
       !
       call msg('o eps eel alpha','#')
       call msg('o eps eel alpha','# ','- Energies      are '//trim(X_E_xc_string(4)),INDENT=0)
       call msg('o eps eel alpha','# ','- Wavefunctions are '//trim(X_WF_xc_string(4)),INDENT=0)
       ch='- The Green`s function is T-ordered -'
       if (trim(X%ordering)=='a') ch='- The Green`s function is antiresonant -'
       if (trim(X%ordering)=='r') ch='- The Green`s function is resonant -'
       if (trim(X%ordering)=='c') ch='- The Green`s function is causal -'
       call msg('o eps eel alpha','# ',trim(ch),INDENT=0)
       !
       ! Gauges
       !
       if (iq==1) then
         ch='- The gauge is transverse -'
         if (X%Vnl_included) ch='- The gauge is transverse + [r,Vnl] -'
         if (X%long_gauge)   ch='- The gauge is longitudinal -'
         call msg('o eps eel alpha','# ',trim(ch),INDENT=0)
       endif
       !
       ! Titles 
       !
       headings(:3)=(/'E/ev[1]','e/Im[2]','e/Re[3]'/)
       headings(4:5)=(/'eo/Im[4]','eo/Re[5]'/)
       headings(6:7)=(/'alpha/Im','alpha/Re'/)
       headings(8:9)=(/'alpha0/Im','alpha0/Re'/)
       call msg('o eps eel fxc alpha','#')
       if (.not.lnoe0) call msg('o eps','#',headings(:5),INDENT=0,USE_TABS=.true.)    
       if (lnoe0)      call msg('o eps','#',headings(:3),INDENT=0,USE_TABS=.true.)    
       headings(2:3)=(/'EEL/Im[2]','EEL/Re[3]'/)
       headings(4:5)=(/'EELo/Im[4]','EELo/Re[5]'/)
       if (.not.lnoe0) call msg('o eel','#',headings(:5),INDENT=0,USE_TABS=.true.)    
       if (lnoe0)      call msg('o eel','#',headings(:3),INDENT=0,USE_TABS=.true.)    
       if (lnoe0) then
         call msg('o alpha','#',(/headings(1),headings(6),headings(7)/),&
&                                 INDENT=0,USE_TABS=.true.)
       else
         call msg('o alpha','#',(/headings(1),headings(6),headings(7),&
&                                 headings(8),headings(9)/),INDENT=0,USE_TABS=.true.)
       endif
       call msg('o eps eel alpha','#')
     endif
     !
     do i2=fr(1),fr(2)
       !
       ! Eps
       !
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(2,i2)),real(X_epsilon(2,i2)),&
&           aimag(X_epsilon(1,i2)),real(X_epsilon(1,i2))/)
       if (lnoe0)      call msg('o eps','',od(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.lnoe0) call msg('o eps','',od,INDENT=-2,USE_TABS=.true.)
       !
       ! EEL
       !
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(4,i2)),real(X_epsilon(4,i2)),&
&           aimag(X_epsilon(3,i2)),real(X_epsilon(3,i2))/)
       if (lnoe0)      call msg('o eel','',od(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.lnoe0) call msg('o eel','',od,INDENT=-2,USE_TABS=.true.)
       !
       ! Alpha
       !
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(6,i2)),real(X_epsilon(6,i2)),&
&                                  aimag(X_epsilon(5,i2)),real(X_epsilon(5,i2))/)
       if (lnoe0)      call msg('o alpha','',od(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.lnoe0) call msg('o alpha','',od,INDENT=-2,USE_TABS=.true.)
     enddo
     call of_open_close(of_name(1))
     call of_open_close(of_name(2))
     call of_open_close(of_name(4))
   enddo
   !
   ! TDDFT Kernel output
   !
   if (l_bs_fxc) then
     call pp_redux_wait(X_fxc)
     call of_open_close(of_name(3),'oa')
     do i1=1,wv%n(1)
       call msg('o fxc','',(/real(wv%p(i1))*HARTREE,real(X_fxc(i1)*q_norm(iq)**2.),&
&                           aimag(X_fxc(i1)*q_norm(iq)**2.)/),INDENT=-2,USE_TABS=.true.)
     enddo
     call of_open_close(of_name(3))
   endif
   !
   ! CLEAN (each q)
   !
   call X_alloc('X')
   deallocate(X_epsilon,wv%p)
   call mem_est("W-p")
   if (allocated(X_fxc)) deallocate(X_fxc)
   if (allocated(rg_index_bg)) deallocate(rg_index_bg)
   deallocate(X_poles_tab,bg_npts,cg_pt,cg_index_bg)
   call mem_est("X_poles_tab RGi BGn CGp CGi")
 enddo
 !
 ! CLEAN
 !
 call X_alloc('OptOsc')
 call WF_free()
 call pp_redux_wait
 !
 if (l_bs_fxc.and.allocated(FXC)) then
   deallocate(FXC,FXC_K_diagonal)
   call mem_est("FXC FXC_K_diagonal")
 endif
 !
end subroutine
