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
subroutine X_pre_setup(Xen,Xk,X)
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:X_t,use_X_DbGd,X_DbGd_nkpts,l_X_terminator,X_use_lin_sys,&
&                        X_use_gpu,Chi_linalg_mode,X_ng
 use electrons,     ONLY:levels
 use drivers,       ONLY:l_bs_fxc,l_col_cut,Finite_Tel,l_cohsex,l_rt_carriers_in_use
 use R_lattice,     ONLY:nqibz,bz_samp
 use QP_m,          ONLY:COHSEX_use_empties
 use TDDFT,         ONLY:FXC_n_g_corr
 use interfaces,    ONLY:eval_G_minus_G
 use parallel_m,    ONLY:PAR_COM_WORLD
 use stderr,        ONLY:intc
 use cuda_m,        ONLY:have_cuda
 !
 implicit none
 !
 type(X_t)    :: X 
 type(bz_samp):: Xk
 type(levels) :: Xen
 ! 
 ! Work Space
 !
 real(SP), external :: G2E
 !
 if (X%ib(1)<0) X%ib(1)=1
 if (X%ib(2)<0) X%ib(2)=1
 if (X%iq(2)<0.or.X%iq(2)>nqibz) X%iq(2)=nqibz
 if (X%iq(1)<0) X%iq(1)=1
 if (X%iq(1)>X%iq(2)) X%iq(1)=X%iq(2)
 !
 ! In these two lines I ensure the at least all the metallic
 ! bands are included in the X summation
 !
 if (X%ib(1)>Xen%nbf+1) X%ib(1)=Xen%nbf+1
 if (X%ib(2)<Xen%nbm)   X%ib(2)=Xen%nbm
 !
 ! Other checks
 !
 if (X%ib(1)<0.or.X%ib(1)>Xen%nbm.or.X%ib(2)<X%ib(1)) X%ib(1)=1
 if (X%ib(2)>Xen%nb.or.X%ib(2)<Xen%nbf)               X%ib(2)=Xen%nb
 !
 ! Here I set up the Double Grid flag to (for example) skip the coarse 
 ! grid generator in the Xo calculation
 !
 use_X_DbGd=Xen%nb_FineGd/=0.and.X%ib(1)<Xen%nb_FineGd.and.(.not.l_bs_fxc)
 !
 if (use_X_DbGd) X_DbGd_nkpts=Xk%nbz+Xk%FGbz%N
 !
 ! COH check: when the X terminator is used or the empty bands are NOT used the
 ! response function is accessed in G-G' mode. Here I ensure that all G-G' are in the
 ! range provided externally
 !
 if ((l_cohsex.and..not.COHSEX_use_empties).or.l_X_terminator) then
     X%ng=eval_G_minus_G(X%ng,0,COMM=PAR_COM_WORLD)
     call warning(' Response block size reduced to '//intc(X%ng) &
&     //'RL ('//intc(int(1000.*G2E(X%ng)))//'mHa)')
 endif
 !
 ! Fxc Checks
 !
 if (FXC_n_g_corr>X%ng) FXC_n_g_corr=X%ng
 !
 ! In case the columbian is cutoffed a resonant/antiresonant 
 ! only calculation is meaningless because of the dependence 
 ! of the alpha from X
 !
 if (l_col_cut) then
   if ( trim(X%ordering) == 'a' .or. trim(X%ordering) == 'r' )  X%ordering='T'
 endif
 !
 ! Finite temperature X MUST be causal. This is because in X_eh_setup the
 ! "resonant" or "antiresonant" par of X (in the many body language) include
 ! negative poles as well. Only the sum of the two is meaningful, then.
 !
 if (Finite_Tel .or. l_rt_carriers_in_use) X%ordering="R"
 !
 ! Transfer the GF's ordering 
 !
 if ( allocated(Xen%GreenF).and.     Xen%GreenF_is_causal ) X%ordering='R'
 if ( allocated(Xen%GreenF).and..not.Xen%GreenF_is_causal ) then
   if ( .not.(Finite_Tel.or.l_rt_carriers_in_use) ) X%ordering='T'
   if (      (Finite_Tel.or.l_rt_carriers_in_use) ) then
     call error('Fractional occupations but GF is not retarded (causal in yambo language)')
   endif 
 endif
 !
 ! Dyson solver
 !
 X_use_lin_sys=.true.
 X_use_gpu=have_cuda
 if (index(Chi_linalg_mode,"INVERSION")>0) X_use_lin_sys=.false.
 if (index(Chi_linalg_mode,"LIN_SYS")>0  ) X_use_lin_sys=.true.
 if (index(Chi_linalg_mode,"CPU")>0      ) X_use_gpu=.false.
 if (index(Chi_linalg_mode,"GPU")>0      ) X_use_gpu=.true..and.have_cuda
 !
 ! Copy of X dimension for Parallel default environment solver
 !
 X_ng=X%ng
 !
end subroutine
