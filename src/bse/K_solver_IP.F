!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team 
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
subroutine K_solver_IP(iq,W) 
 !
 use pars,           ONLY:SP,DP,pi
 use com,            ONLY:warning
 use units,          ONLY:HA2EV
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BSS_n_freqs
#if defined _KERR
 use KERR,           ONLY:chi_jj,iw_ref
 use drivers,        ONLY:l_kerr
#endif
 use memory_m,       ONLY:mem_est
 use frequency,      ONLY:w_samp
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 logical       :: l_kk
 complex(SP)   :: eps_kk(2,BSS_n_freqs)
 integer       :: iw
 complex(SP)   :: w_kk(BSS_n_freqs),winv_kk(BSS_n_freqs)
 !  
 ! X_epsilon: 1=w , 2=eps_xx  3=eps0_xx  4=eps0_xy  5=eps_xy
 !
 X_epsilon(1,:)=W%p(:)
#if defined _KERR
 X_epsilon(5,:)=0.
#endif
 X_epsilon(2,:)=0.
 !
 ! Kramers-Kroning transformation of the absorbitive part
 !
 call parser('KrmKrn',l_kk)
 !
 if(l_kk) then
   eps_kk=(0.,0.)
   w_kk=real(W%p)+(0.,0.0001)/HA2EV
   eps_kk(1,:)=-1/pi*aimag(X_epsilon(3,:)) 
#if defined _KERR
   if (chi_jj) eps_kk(1,:)=eps_kk(1,:)*real(W%p(:))**2
#endif
   !
   if ( (maxval(aimag(eps_kk(1,:)))/aimag(eps_kk(1,1))<100..or.&
&        maxval(aimag(eps_kk(1,:)))/aimag(eps_kk(1,BSS_n_freqs))<100.)) &
&     call warning(' [KERR] Possible inaccuracy in the EPS prolungation')
   !
   eps_kk(1,1)=(0.,0.)
   eps_kk(1,BSS_n_freqs)=(0.,0.) 
   call Kramers_Kronig(eps_kk(1,:),real(w_kk),BSS_n_freqs,eps_kk(2,:),w_kk,BSS_n_freqs,(0.,0.))
   !forall(iw=1:BSS_n_freqs) eps_kk(1,iw)=1./pi*aimag(X_epsilon(3,BSS_n_freqs-iw+1))
   !forall(iw=1:BSS_n_freqs) winv_kk(iw)=-conjg(w_kk(BSS_n_freqs-iw+1))
   !eps_kk(1,1)=(0.,0.)
   !eps_kk(1,BSS_n_freqs)=(0.,0.)
   !call Kramers_Kronig(eps_kk(1,:),real(winv_kk),BSS_n_freqs,eps_kk(2,:),w_kk,BSS_n_freqs,(0.,0.))
   !
#if defined _KERR
   if (chi_jj) eps_kk(2,:)=eps_kk(2,:)/(real(W%p(:))**2+epsilon(1.))
#endif
   X_epsilon(3,:)=eps_kk(2,:)+1.
   !
#if defined _KERR
   eps_kk=(0.,0.)
   eps_kk(1,:)= 1/pi*real(X_epsilon(4,:))
   eps_kk(1,1)=(0.,0.)
   eps_kk(1,BSS_n_freqs)=(0.,0.)
   if (chi_jj) eps_kk(1,:)=eps_kk(1,:)*real(W%p(:))**2
   !
   call Kramers_Kronig(eps_kk(1,:),real(w_kk),BSS_n_freqs,eps_kk(2,:),w_kk,BSS_n_freqs,(0.,0.))
   !forall(iw=1:BSS_n_freqs) eps_kk(1,iw)=-1./pi*real(X_epsilon(4,BSS_n_freqs-iw+1))
   !eps_kk(1,1)=(0.,0.)
   !eps_kk(1,BSS_n_freqs)=(0.,0.)
   !call Kramers_Kronig(eps_kk(1,:),real(winv_kk),BSS_n_freqs,eps_kk(2,:),w_kk,BSS_n_freqs,(0.,0.))
   !
   if (chi_jj) eps_kk(2,:)=eps_kk(2,:)/(real(W%p(:))**2+epsilon(1.))
   X_epsilon(4,:)=(0.,1.)*eps_kk(2,:)
   !
#endif
 endif
 !
 ! Initialize & write the output file 
 !
 call K_output_file(iq,-2)
 call K_output_file(iq,2)
 !
#if defined _KERR
 if(l_kerr) call K_kerr_output()
#endif
 !
end subroutine
