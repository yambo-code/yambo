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
!==================================
 use pars,          ONLY:pi,SP,HARTREE,schlen
 use D_lattice,     ONLY:dl_sop
 use memory_m,      ONLY:mem_est
 use drivers,       ONLY:l_bs_fxc,l_lrc_fxc,l_alda_fxc
 use frequency,     ONLY:w_samp,rg_index_bg,bg_npts,cg_pt,cg_index_bg
 use electrons,     ONLY:levels,BZ_RIM_tot_nkpts
 use R_lattice,     ONLY:bz_samp,q_module
 use com,           ONLY:msg,of_open_close
 use vec_operate,   ONLY:v_module,iku_v_module
 use X_m,           ONLY:X_t,X_epsilon,X_mat,X_alloc,X_poles_tab,X_fxc,&
&                        alpha_dim,eps_2_alpha,use_X_RIM
 use parser_m,      ONLY:parser
 use par_proc_m,    ONLY:pp_redux_wait
 use wave_func,     ONLY:WF_free
 use TDDFT,         ONLY:FXC_n_descs,FXC_description,FXC_type,&
&                        FXC,FXC_K_diagonal,FXC_n_g_corr
 implicit none
 type(levels)::Xen
 type(bz_samp) ::Xk,q
 type(X_t):: X
 type(w_samp):: wv 
!ws
 integer          :: i1,i2,fr(2),iq
 real(SP)         :: od(5)
 character(10)    :: headings(10)
 character(schlen):: ch,of_name(4)
 logical          :: lnoe0
!----------------------------------------------------------------
! 
 call section('*','Optics')
!--------------------------

 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]:',eps_2_alpha)
   call msg('rn','Polarizability dimension      :',trim(alpha_dim))
 endif

 call X_checks(Xen,X)
!
 do iq=X%iq(1),X%iq(2)
!
! TDDFT setup
!------------------------------------------------------
   if (l_bs_fxc) call tddft_do_X_W_typs(iq,X,wv)
   if (l_lrc_fxc) FXC_type='-tddft_lrc'
   if (l_alda_fxc) call tddft_alda_g_space(Xen,Xk)
!
!In TDDFT with BS based kernel eps0 is non-sense
!as shifted of the K diagonal
!-----------------------------------------------
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
! OutPut files
!==============
   of_name(:)=' '
!epsilon
   write (of_name(1),'(a,i3.3,a)') 'eps_q',iq,trim(FXC_type)
   call of_open_close(of_name(1),'ot')
!fxc
   if (l_bs_fxc) then
     write (of_name(3),'(a,i3.3)')   'bs_fxc_q',iq
     call of_open_close(of_name(3),'ot')
   endif
!eels
   if (index(X%ordering,'r')==0) then
     write (of_name(2),'(a,i3.3,a)') 'eel_q',iq,trim(FXC_type)
     call of_open_close(of_name(2),'ot')
   endif
!alpha
   if (trim(alpha_dim)/='adim') then
     write (of_name(4),'(a,i3.3,a)') 'alpha_q',iq,trim(FXC_type)
     call of_open_close(of_name(4),'ot')
   endif
!
! Titles & headings
!///////////////////////////////////////////////////////////////////////
   write (ch,'(a,i3,a)') '# Absorption @ IQ',iq,' [iku]:'
   if (iq==1) call msg("o eps",trim(ch),X%q0/v_module(X%q0),INDENT=0)
   if (iq/=1) call msg("o eps",trim(ch),q%pt(iq,:),INDENT=0)
   write (ch,'(a,i3,a)') '# EELS @ IQ',iq,' [iku]:'
   if (iq==1) call msg("o eel",trim(ch),X%q0/v_module(X%q0),INDENT=0)
   if (iq/=1) call msg("o eel",trim(ch),q%pt(iq,:),INDENT=0)
   write (ch,'(3a,i3,a)') '# Polarizability ( -<<X>> ) [',trim(alpha_dim),'] @ IQ',iq,' [iku]:'
   call msg("o alpha",trim(ch),q%pt(iq,:),INDENT=0)
   if (l_bs_fxc) then
     call msg('o eps eel alpha','#')
     do i1=1,FXC_n_descs
       ch=FXC_description(i1)
       call msg('o eps eel fxc alpha','#',trim(ch(2:)),INDENT=0)    
     enddo
     write (ch,'(a,i3)') ' TDDFT|Fxc size             :',FXC_n_g_corr
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
     write (ch,'(a,i3)') '      |Hartree size         :',X%ng
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
     write (ch,'(2a)')   '      |Ordering             :',X%ordering
     call msg('o eps eel alpha','#',trim(ch),INDENT=0)    
!
! Fxc file titles
     headings(1)  ='E/ev[1]'
     headings(2:3)=(/'q^2*Fxc/Re','q^2*Fxc/Im'/)
     call msg('o fxc','#',headings(:3),INDENT=0,USE_TABS=.true.)    

   endif
!
! BZ RIM
   if (use_X_RIM) then
     call msg('o eps eel fxc alpha','#')
     call msg('o eps eel fxc alpha','# BZ Energy RIM points :',BZ_RIM_tot_nkpts,INDENT=0)
   endif
   
   headings(:3)=(/'E/ev[1]','e/Im[2]','e/Re[3]'/)
   headings(4:5)=(/'eo/Im[4]','eo/Re[5]'/)
   headings(6:7)=(/'alpha0/Im','alpha0/Re'/)
   headings(8:9)=(/'alpha/Im','alpha/Re'/)
   call msg('o eps eel fxc alpha','#')
   if (.not.lnoe0) call msg('o eps','#',headings(:5),INDENT=0,USE_TABS=.true.)    
   if (lnoe0)      call msg('o eps','#',headings(:3),INDENT=0,USE_TABS=.true.)    
   headings(2:3)=(/'EEL/Im[2]','EEL/Re[3]'/)
   headings(4:5)=(/'EELo/Im[4]','EELo/Re[5]'/)
   if (.not.lnoe0) call msg('o eel','#',headings(:5),INDENT=0,USE_TABS=.true.)    
   if (lnoe0)      call msg('o eel','#',headings(:3),INDENT=0,USE_TABS=.true.)    
   if (lnoe0) then
     call msg('o alpha','#',(/headings(1),headings(6),headings(7)/),&
&                             INDENT=0,USE_TABS=.true.)
   else
     call msg('o alpha','#',(/headings(1),headings(6),headings(7),&
&                             headings(8),headings(9)/),INDENT=0,USE_TABS=.true.)
   endif
   call msg('o eps eel alpha','#')
   call pp_redux_wait
   call of_open_close(of_name(1))
   call of_open_close(of_name(2))
   call of_open_close(of_name(3))
   call of_open_close(of_name(4))
!///////////////////////////////////////////////////////////////////////
! e Table
! 1:e0 2:e 3:eel0 4:eel 5:alpha0 6:alpha 
!============================================================
   X_epsilon=(0.,0.)
   do i1=1,wv%n(1),wv%n(2)
     fr=(/i1,i1+wv%n(2)-1/)
     call X_os(X_mat,iq,fr,Xen,Xk,wv,X)     
     X_epsilon(1,fr(1):fr(2))=1.0_SP-X_mat(1,1,:)*4.0_SP*pi/q_module(iq)**2
     X_epsilon(6,fr(1):fr(2))=-X_mat(1,1,:)*eps_2_alpha/q_module(iq)**2
     call X_s(iq,fr,X,wv)
     X_epsilon(2,fr(1):fr(2))=1./(X_mat(1,1,:)+1.)
     X_epsilon(3,fr(1):fr(2))=-1./X_epsilon(1,:)
     X_epsilon(4,fr(1):fr(2))=-1./X_epsilon(2,:)
!
! This relation is exact
!
! alpha=-eps_2_alpha X(1,1) / |q|^2 = (1 -eps_M^-1) eps_2_alpha/4/pi
!
! lhs does not need the antiresonant part of X
! rhs DOES need the antiresonant part of X
!
!     X_epsilon(5,fr(1):fr(2))=-X_mat(1,1,:)/4./pi*eps_2_alpha
!
! In the limit of eps_2_alpha -> \infty
!
! alpha=(epsilon_M-1) eps_2_alpha/4/pi
!
! this does not need the antiresonant part of X
!
     X_epsilon(5,fr(1):fr(2))=(X_epsilon(2,fr(1):fr(2))-1._SP)*eps_2_alpha/4./pi
!
     call of_open_close(of_name(1),'oa')
     call of_open_close(of_name(2),'oa')
     call of_open_close(of_name(4),'oa')
     do i2=fr(1),fr(2)
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(2,i2)),real(X_epsilon(2,i2)),&
&           aimag(X_epsilon(1,i2)),real(X_epsilon(1,i2))/)
       if (lnoe0)      call msg('o eps','',od(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.lnoe0) call msg('o eps','',od,INDENT=-2,USE_TABS=.true.)
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(4,i2)),real(X_epsilon(4,i2)),&
&           aimag(X_epsilon(3,i2)),real(X_epsilon(3,i2))/)
       if (lnoe0)      call msg('o eel','',od(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.lnoe0) call msg('o eel','',od,INDENT=-2,USE_TABS=.true.)
       od=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(5,i2)),real(X_epsilon(5,i2)),&
&                                  aimag(X_epsilon(6,i2)),real(X_epsilon(6,i2))/)
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
      call msg('o fxc','',(/real(wv%p(i1))*HARTREE,real(X_fxc(i1)*q_module(iq)**2.),&
&                          aimag(X_fxc(i1)*q_module(iq)**2.)/),INDENT=-2,USE_TABS=.true.)
    enddo
    call of_open_close(of_name(3))
  endif
!
!  CLEAN
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
!CLEAN
!
 call X_alloc('OptOsc')
 call WF_free()
 call pp_redux_wait

! -- GPL_END --
 if (l_bs_fxc) then
   deallocate(FXC,FXC_K_diagonal)
   call mem_est("FXC FXC_K_diagonal")
 endif
! -- GPL_START --

 end subroutine

