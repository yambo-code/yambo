!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine acfdt_tot_energy(Xen,Xk,En,k,q,X)
 !
 use pars,           ONLY:SP,schlen,pi,DP
 use units,          ONLY:HA2EV
 use ACFDT,          ONLY:ACFDT_n_freqs,ACFDT_n_lambda,ACFDT_E_range
 use FFT_m,          ONLY:fft_size
 use frequency,      ONLY:w_samp,W_reset
 use wave_func,      ONLY:WF
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use linear_algebra, ONLY:INV,USE_LK
 use interfaces,     ONLY:PARALLEL_index,WF_load,el_density_and_current,LINEAR_ALGEBRA_driver 
 use R_lattice,      ONLY:bz_samp,bare_qpg,nqibz
 use electrons,      ONLY:levels,nel
 use D_lattice,      ONLY:DL_vol
 use com,            ONLY:msg
 use X_m,            ONLY:X_t,X_alloc,X_mat
 use QP_m,           ONLY:QP_nb,QP_nk,QP_n_states,QP_table,QP_state,QP_Vnl_xc,QP_Vxc
 use collision_el,   ONLY:elemental_collision,elemental_collision_free
 use IO_m,           ONLY:io_control,RD_CL_IF_END,OP_RD
 use xc_functionals, ONLY:E_xc,XC_EXCHANGE_CORRELATION,XC_LDA_C_KP,XC_EXCHANGE,&
&                         XC_LDA_X,XC_CORRELATION,XC_potential_driver
 use global_XC,      ONLY:WF_xc_functional
#include<memory.h>
 !
 type(levels) ::Xen,En
 type(bz_samp)::Xk,q,k
 type(X_t)    ::X
 ! 
 ! Work Space
 !
 integer  ::i1,ib,ik
 real(SP) ::Er(9),rfac(4)
 character(9)     ::ttl(4)
 character(schlen)::ch
 !
 ! Xo
 !
 type(w_samp)      ::Xw
 integer           ::iqibz,i_err,ID,io_err
 integer, external ::X_dielectric_matrix,io_X
 complex(SP), allocatable ::X_s(:,:),tddftk(:,:)
 complex(DP), allocatable ::dX(:)
 !
 ! ACFDT
 !
 integer       ::iw,il,FUNCTIONAL_,KIND_
 real(SP)      ::Ex_LDA,Ec_LDA,dEc_LDA,ACFDT_EXX,ACFDT_Ec,sug_E_rng
 type(elemental_collision)::isc
 type(PP_indexes)         ::plw
 real(SP), allocatable    ::lambda(:)
 real(SP), allocatable :: lambda_weight(:)
 real(SP), allocatable :: freq(:)
 real(SP), allocatable :: freq_weight(:)
 logical               :: do_Ec
 !
 ! DFT 
 !
 real(SP),allocatable  :: rho(:)
#if defined _DOUBLE
 real(DP):: ddot
#else
 real(SP):: sdot
#endif
 !
 call section('*','ACFDT Total Energy')
 !
 call elemental_collision_free(isc)
 call PP_indexes_reset(plw)
 !
 call msg('s','[ACFDT] LDA Total energy')
 !
 call WF_load(WF,0,1,(/1,Xen%nbm/),(/1,Xk%nibz/),title='-Density')
 !
 YAMBO_ALLOC(E_xc,(fft_size))
 YAMBO_ALLOC(rho,(fft_size))
 call el_density_and_current(Xen,Xk,rho=rho)
 !
 KIND_=XC_EXCHANGE_CORRELATION
 FUNCTIONAL_=XC_LDA_C_KP
 call XC_potential_driver(Xen,Xk,KIND_,FUNCTIONAL_,0)
#if defined _DOUBLE
 dEc_LDA=ddot(fft_size,rho,1,E_xc,1)
#else 
 dEc_LDA=sdot(fft_size,rho,1,E_xc,1)
#endif
 !
 KIND_=XC_EXCHANGE
 FUNCTIONAL_=XC_LDA_X
 call XC_potential_driver(Xen,Xk,KIND_,FUNCTIONAL_,0)
#if defined _DOUBLE
 Ex_LDA=ddot(fft_size,rho,1,E_xc,1)
#else 
 Ex_LDA=sdot(fft_size,rho,1,E_xc,1)
#endif
 !
 FUNCTIONAL_=XC_CORRELATION
 call XC_potential_driver(Xen,Xk,KIND_,WF_xc_functional,0)
#if defined _DOUBLE
 Ec_LDA=ddot(fft_size,rho,1,E_xc,1)
#else 
 Ec_LDA=sdot(fft_size,rho,1,E_xc,1)
#endif
 !
 YAMBO_FREE(E_xc)
 YAMBO_FREE(rho)
 call WF_free(WF)
 !
 ! Correlatin Setup
 !
 do_Ec=ACFDT_n_freqs>0.and.ACFDT_n_lambda>0
 ACFDT_Ec=0.
 !
 if (do_Ec) then
   !
   call section('+','Gauss-Legendre grids ([E]nergy/[L]ambda)')
   !
   !GL-grid setup (energy)
   !
   YAMBO_ALLOC(freq,(2*ACFDT_n_freqs+1))
   YAMBO_ALLOC(freq_weight,(2*ACFDT_n_freqs+1))
   !
   call gauss_leg_grid(0._SP,ACFDT_E_range(1),freq(:ACFDT_n_freqs),&
&                      freq_weight(:ACFDT_n_freqs),ACFDT_n_freqs)
   !
   call gauss_leg_grid(ACFDT_E_range(1),ACFDT_E_range(2),freq(ACFDT_n_freqs+1:),&
&                      freq_weight(ACFDT_n_freqs+1:),ACFDT_n_freqs)
   !
   ACFDT_n_freqs=2*ACFDT_n_freqs+1
   !
   freq(ACFDT_n_freqs)=1.2*freq(ACFDT_n_freqs-1)
   freq_weight(ACFDT_n_freqs)=0.
   !
   !GL-grid setup (coupling)
   !
   YAMBO_ALLOC(lambda,(ACFDT_n_lambda))
   YAMBO_ALLOC(lambda_weight,(ACFDT_n_lambda))
   call gauss_leg_grid(0._SP,1._SP,lambda,lambda_weight,ACFDT_n_lambda)
   lambda_weight(:)=lambda_weight(:)*2.*pi
   !
   ! Report
   !
   sug_E_rng=(nel/DL_vol)**(2./3.)*HA2EV
   call msg('r','[E] Energy borders [ev][suggested]:',(/25.*sug_E_rng,250*sug_E_rng/))
   call msg('r','                            [used]:',(/ACFDT_E_range*HA2EV/))
   call msg('r','    Energy points                 :',(/(ACFDT_n_freqs-1)/2+1,ACFDT_n_freqs/))
   call msg('r','    First point               [ev]:',(/freq(1)*HA2EV/))
   call msg('nr','[L] Pts(1-5):',(/lambda(1:min(5,ACFDT_n_lambda))/))
   call msg(' r','    Wgs(1-5):',(/lambda_weight(1:min(5,ACFDT_n_lambda))/))
   call msg(' r','    Points  :',(/ACFDT_n_lambda/))
   !
   call section('=','Non-interacting Response Function [Xo]')
   !
   call X_pre_setup(Xen,Xk,X)
   !
   do iqibz=1,nqibz
     X%iq=iqibz
     X%ordering='R'
     call W_reset(Xw)
     Xw%n_freqs=ACFDT_n_freqs
     Xw%er=0.
     Xw%dr=(/freq(1),freq(ACFDT_n_freqs)/)
     YAMBO_ALLOC(Xw%p,(ACFDT_n_freqs))
     Xw%p(:)=(0.,1.)*freq(:)
     i_err=X_dielectric_matrix(Xen,Xk,q,X,Xw,iqibz>1)
     if (i_err==0) exit
   enddo
   !
   call section('=','Correlation Energy Loop')
   !
   call msg('r','[Ec] RL components :',X%ng)
   !
   YAMBO_ALLOC(X_s,(X%ng,X%ng))
   YAMBO_ALLOC(isc%gamp,(X%ng,1))
   YAMBO_ALLOC(tddftk,(X%ng,X%ng))
   YAMBO_ALLOC(dX,(X%ng))
   call X_alloc('X',(/X%ng,X%ng,ACFDT_n_freqs/))
   !
   ACFDT_Ec=0.
   !
   call PARALLEL_index(plw,(/ACFDT_n_lambda,ACFDT_n_freqs/))
   call live_timing('Ec',plw%n_of_elements(myid+1)*q%nibz)
   !
   ! X I/O (section 1)
   !
   call io_control(ACTION=OP_RD,SEC=(/1/),ID=ID)
   io_err=io_X(X,Xw,ID)
   !
   do iqibz=1,q%nibz
     !
     ! X I/O
     !
     call io_control(ACTION=RD_CL_IF_END,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
     io_err=io_X(X,Xw,ID)
     !
     isc%qs(2)=iqibz
     call scatter_Gamp(isc,'x')
     !
     do il=1,ACFDT_n_lambda
       do iw=1,ACFDT_n_freqs
         if (.not.plw%element_2D(il,iw)) cycle
         !
         ! no Fxc [delta_(g1,g2)-Xo(g1,g2)*v(gp)*Lambda]
         !
         do i1=1,X%ng 
           tddftk(:,i1)=-X_mat(:,i1,iw)*4.*pi*lambda(il)/bare_qpg(iqibz,i1)**2.
           tddftk(i1,i1)=tddftk(i1,i1)+1.
         enddo
         call LINEAR_ALGEBRA_driver(INV,M=tddftk,lib_in=USE_LK)
         X_s=matmul(tddftk,X_mat(:,:,iw))
         forall(i1=1:X%ng) dX(i1)=(X_s(i1,i1)-X_mat(i1,i1,iw))*4.*pi*isc%gamp(i1,1)
         ACFDT_Ec=ACFDT_Ec-q%nstar(iqibz)*lambda_weight(il)*freq_weight(iw)*sum(dX)
         call live_timing(steps=1)
       enddo
     enddo
   enddo
   !
   call PP_redux_wait(ACFDT_Ec)
   call live_timing
   !
   ! Partial CLEAN
   !
   call X_alloc('X')
   call elemental_collision_free(isc)
   call PP_indexes_reset(plw)
   YAMBO_FREE(X_s)
   YAMBO_FREE(dX)
   YAMBO_FREE(dX)
   !
 endif
 !
 ! Exchange Energy (EXX)
 !
 QP_nb=En%nbm
 QP_nk=k%nibz
 YAMBO_FREE(QP_state)
 YAMBO_ALLOC(QP_state,(QP_nb,QP_nk))
 QP_state=.TRUE.
 !
 call XCo_driver(En,k,Xk,q)
 !
 ACFDT_EXX=0.
 !
 do i1=1,QP_n_states
   ib=QP_table(i1,1)
   ik=QP_table(i1,3)
   ACFDT_EXX=ACFDT_EXX+En%f(ib,ik,1)*k%weights(ik)*QP_Vnl_xc(i1)
 enddo
 ACFDT_EXX=ACFDT_EXX/DL_vol/2._SP
 !
 call section('=','Summary')
 !
 Er(:4)=(/Ex_LDA,ACFDT_EXX*DL_vol,Ec_LDA,ACFDT_Ec*DL_vol/)
 Er(5)=Er(1)+Er(3)
 Er(6)=Er(2)+Er(4)
 Er(7)=dEc_LDA
 Er(8)=Er(5)+Er(7)
 Er(9)=Er(6)+Er(7)
 ttl=(/'[meV/Vol]',' [eV/Vol]','[mHa/Vol]',' [mHa/el]'/)
 rfac=(/HA2EV*1000.d0/DL_vol,HA2EV/dble(nel),&
&       1000.d0/dble(DL_vol),1000.d0/dble(nel)/)
 do i1=1,4
   write (ch,'(2a)') ttl(i1),' Ex         LDA-EXX    :'
   call msg('r',trim(ch),Er(1:2)*rfac(i1))
   call msg('r','          Ec         LDA-Rpa    :',Er(3:4)*rfac(i1))
   call msg('r','          Exc        LDA-EXX/Rpa:',Er(5:6)*rfac(i1))
   call msg('r','          HEGc(orrection)       :',(/Er(7)*rfac(i1)/))
   call msg('rn','          Exc+HEGc   LDA-EXX/Rpa:',Er(8:9)*rfac(i1))
 enddo
 !
 ! CLEAN
 !
 YAMBO_FREE(freq)
 YAMBO_FREE(freq_weight)
 YAMBO_FREE(lambda)
 YAMBO_FREE(lambda_weight)
 YAMBO_FREE(Xw%p)
 YAMBO_FREE(QP_Vnl_xc)
 YAMBO_FREE(QP_Vxc)
 YAMBO_FREE(QP_table)
 YAMBO_FREE(QP_state)
 !
end subroutine
