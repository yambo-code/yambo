!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine scatterBamp(isc)
 !
 ! rhotw(G)=<ib ik_is i_sp| e^[I(RsG-G0).r] | ob ok_os o_sp>   
 !
 !  is(3)=(ib,ik,is,i_sp) --<--:--<-- os(3)=(ob,ok,os,o_sp)
 !                             :
 !                            /:\ iqs=(ig0,iq,qs)
 !                             :
 !
 !                     \:/
 ! REMEMBER that     ->-:->-  =  conjg [-<-:-<-]
 !                                        /:\
 !  iq_is = ik_is-ok_os-Go
 !
 use pars,           ONLY:DP,SP,cZERO,cONE
 use electrons,      ONLY:n_spinor
 use collision,      ONLY:ggwinfo
 use D_lattice,      ONLY:nsym,i_time_rev,spin_sop,idt_index
 use R_lattice,      ONLY:g_rot
 use FFT_m,          ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 !
 implicit none
 !
 type(ggwinfo)::isc
 !
 ! Work Space
 !
 complex(DP) :: rho_tw_rs(fft_size)
 complex(SP) :: WF_symm_i(fft_size,n_spinor)
 complex(SP) :: WF_symm_o(fft_size,n_spinor)
 !
 integer     :: qs,iGo,ifft
 !
 ! <ib ik_is i_sp| 
 call WF_apply_symm(isc%is,WF_symm_i)
 !
 ! | ob ok_os o_sp>   
 call WF_apply_symm(isc%os,WF_symm_o)
 !
 ! \tilde{\rho} in Real Space
 rho_tw_rs(:)=conjg(WF_symm_i(:,1))*WF_symm_o(:,1)
 if (n_spinor==2) rho_tw_rs(:)=rho_tw_rs(:)+conjg(WF_symm_i(:,2))*WF_symm_o(:,2)
 !
#if defined _FFTW
 call fft_3d(rho_tw_rs,fft_dim,+1,fftw_plan)
#else
 call fft_3d(rho_tw_rs,fft_dim,+1)
#endif
 !
 ! e^[I(Rs(G-G0)).r]
 !
 iGo=isc%qs(1)
 qs =isc%qs(3)
 !
 if(qs==idt_index) isc%rhotw(1:isc%ngrho)= rho_tw_rs(fft_g_table(1:isc%ngrho,iGo))
 if(qs/=idt_index) isc%rhotw(1:isc%ngrho)= rho_tw_rs(fft_g_table(g_rot(qs,1:isc%ngrho),iGo))
 !
 ! q=0, G=0 case
 !
 if (all((/isc%qs(2)==1,isc%is(1)==isc%os(1)/))) isc%rhotw(1)=cONe
 if (all((/isc%qs(2)==1,isc%is(1)/=isc%os(1)/))) isc%rhotw(1)=cZERO
 !
end subroutine
