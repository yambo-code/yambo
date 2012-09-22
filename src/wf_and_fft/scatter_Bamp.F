!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
 use pars,           ONLY:DP,SP,cZERO,cONe
 use electrons,      ONLY:n_spinor
 use collision,      ONLY:ggwinfo
 use D_lattice,      ONLY:nsym,i_time_rev,mag_syms,spin_sop,idt_index
 use R_lattice,      ONLY:g_rot
 use wave_func,      ONLY:wf,wf_state
 use FFT_m,          ONLY:fft_size,fft_rot_r,fft_dim,fftw_plan,fft_g_table
 implicit none
 type(ggwinfo)::isc
 !
 ! Work Space
 !
 complex(DP) :: rho_tw_rs(fft_size)
 complex(SP) :: wf_tmp_i(fft_size,n_spinor),wf_tmp_o(fft_size,n_spinor)
 !
 integer :: ib,ik,is,i_sp,i_wf
 integer :: ob,ok,os,o_sp,o_wf
 integer :: qs,iGo,ifft
 logical :: litr,lotr
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !i_sp =1
 !o_sp =1
 ! GPL_INCLUDE_END SPIN_IGNORE
 !
 ! <ib ik_is i_sp| 
 !
 ib=isc%is(1)
 ik=isc%is(2)
 is=isc%is(3)
 i_sp=isc%is(4)
 litr=is>nsym/(i_time_rev+1)
 !
 ! | ob ok_os o_sp>   
 !
 ob=isc%os(1)
 ok=isc%os(2)
 os=isc%os(3)
 o_sp=isc%os(4)
 lotr=os>nsym/(i_time_rev+1)
 !
 ! e^[I(Rs(G-G0)).r]
 !
 iGo=isc%qs(1)
 qs =isc%qs(3)
 !
 ! \tilde{\rho} in Real Space
 !
 i_wf=wf_state(ib,ik,i_sp)
 o_wf=wf_state(ob,ok,o_sp)
 !
 if(is==idt_index) then
   wf_tmp_i(:,1)=wf(:,i_wf)
   wf_tmp_o(:,1)=wf(:,o_wf)
 else
   wf_tmp_i(:,1)=wf(fft_rot_r(is,:),i_wf)
   wf_tmp_o(:,1)=wf(fft_rot_r(os,:),o_wf)
 endif
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 if (n_spinor==2) then
   i_wf=wf_state(ib,ik,2)
   o_wf=wf_state(ob,ok,2)
   if(is==idt_index) then
     wf_tmp_i(:,2)=wf(:,i_wf)
     wf_tmp_o(:,2)=wf(:,o_wf)
   else
     wf_tmp_i(:,2)=wf(fft_rot_r(is,:),i_wf)
     wf_tmp_o(:,2)=wf(fft_rot_r(os,:),o_wf)
   endif
   !
   forall(ifft=1:fft_size) wf_tmp_i(ifft,:)=matmul(spin_sop(:,:,is),wf_tmp_i(ifft,:))
   forall(ifft=1:fft_size) wf_tmp_o(ifft,:)=matmul(spin_sop(:,:,os),wf_tmp_o(ifft,:))
 endif
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
 if(.not.litr) wf_tmp_i=conjg(wf_tmp_i)
 if(     lotr) wf_tmp_o=conjg(wf_tmp_o)
 !
 rho_tw_rs(:)=wf_tmp_i(:,1)*wf_tmp_o(:,1)
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 if (n_spinor==2) rho_tw_rs(:)=rho_tw_rs+wf_tmp_i(:,2)*wf_tmp_o(:,2)
 ! 
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
#if defined _FFTW
 call fft_3d(rho_tw_rs,fft_dim,+1,fftw_plan)
#else
 call fft_3d(rho_tw_rs,fft_dim,+1)
#endif
 !
 !
 if(qs==idt_index) then
   isc%rhotw(1:isc%ngrho)=  rho_tw_rs(fft_g_table(1:isc%ngrho,iGo))
 else
   isc%rhotw(1:isc%ngrho)=  rho_tw_rs(fft_g_table(g_rot(qs,1:isc%ngrho),iGo))
 endif
 !
 ! q=0, G=0 case
 !
 if (all((/isc%qs(2)==1,ib==ob/))) isc%rhotw(1)=cONe
 if (all((/isc%qs(2)==1,ib/=ob/))) isc%rhotw(1)=cZERO
 !
end subroutine
