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
 use pars,           ONLY:DP
 use electrons,      ONLY:n_spinor
 use collision,      ONLY:ggwinfo
 use D_lattice,      ONLY:nsym,i_time_rev,idt_index
 use R_lattice,      ONLY:g_rot
 use wave_func,      ONLY:wf,wf_state
 use FFT_m,          ONLY:fft_size,fft_rot_r,fft_dim,fftw_plan,fft_g_table
 implicit none
 type(ggwinfo)::isc
 !
 ! Work Space
 !
 complex(DP) :: rho_tw_rs(fft_size,n_spinor)
 !
 integer :: ib,ik,is,i_sp,ifft
 integer :: ob,ok,os,o_sp,offt
 integer :: qs,iGo,i_spr,i_spinor
 logical :: litr,lotr
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !i_sp =1
 !o_sp =1
 !i_spr=1
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
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 do i_spr=1,n_spinor
   !
   ! GPL_EXCLUDE_END SPIN_IGNORE
   !
   ifft=wf_state(ib,ik,i_sp+i_spr-1)
   offt=wf_state(ob,ok,o_sp+i_spr-1)
   !                                               
   if(is==idt_index) then
     rho_tw_rs(:,i_spr)=conjg(wf(:,ifft))
   else
     if (litr)      rho_tw_rs(:,i_spr)=wf(fft_rot_r(is,:),ifft)
     if (.not.litr) rho_tw_rs(:,i_spr)=conjg(wf(fft_rot_r(is,:),ifft))
   endif
   !
   if(os==idt_index) then
     rho_tw_rs(:,i_spr)=rho_tw_rs(:,i_spr)*wf(:,offt)
   else
     if (lotr)      rho_tw_rs(:,i_spr)=rho_tw_rs(:,i_spr)*conjg(wf(fft_rot_r(os,:),offt))
     if (.not.lotr) rho_tw_rs(:,i_spr)=rho_tw_rs(:,i_spr)*wf(fft_rot_r(os,:),offt)
   endif
   !
   ! FFT call
   !
#if defined _FFTW
   call fft_3d(rho_tw_rs(:,i_spr),fft_dim,+1,fftw_plan)
#else
   call fft_3d(rho_tw_rs(:,i_spr),fft_dim,+1)
#endif
   !
   ! GPL_EXCLUDE_START SPIN_IGNORE
   !
 enddo
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
 if(qs==idt_index) then
   isc%rhotw(1:isc%ngrho)=  rho_tw_rs(fft_g_table(1:isc%ngrho,iGo),1)
 else
   isc%rhotw(1:isc%ngrho)=  rho_tw_rs(fft_g_table(g_rot(qs,1:isc%ngrho),iGo),1)
 endif
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 if(qs==idt_index) then
   do i_spinor=2,n_spinor,1
     isc%rhotw(1:isc%ngrho)=isc%rhotw(1:isc%ngrho)+rho_tw_rs(fft_g_table(1:isc%ngrho,iGo),i_spinor)
   enddo
 else
   do i_spinor=2,n_spinor,1
     isc%rhotw(1:isc%ngrho)=isc%rhotw(1:isc%ngrho)+&
&                         rho_tw_rs(fft_g_table(g_rot(qs,1:isc%ngrho),iGo),i_spinor)
   enddo
 endif
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 ! 
 ! q=0, G=0 case
 !
 if (all((/isc%qs(2)==1,ib==ob/))) isc%rhotw(1)=(1.,0.)
 if (all((/isc%qs(2)==1,ib/=ob/))) isc%rhotw(1)=(0.,0.)
 !
end subroutine
