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
 subroutine DFT_Vxc(E,Xk)
 !
 ! V_xc
 !
 use pars,          ONLY:SP,schlen,DP,pi,HARTREE
 use electrons,     ONLY:levels,n_spin,n_spinor,n_sp_pol
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table,QP_xc_FUNCTIONAL
 use R_lattice,     ONLY:bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:wf,wf_state
 use xc_functionals,ONLY:V_xc,xc_string,GS_xc_KIND,GS_xc_FUNCTIONAL,el_mag
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 integer                 ::i1,ifft,ifft_sp1,ifft_sp2,ib,ik,i_sp1,i_sp2
#if defined _DOUBLE
 complex(DP)             ::zdotc,zdotu
#else 
 complex(SP)             ::cdotc,cdotu
#endif
 complex(SP) :: c_ws(fft_size,n_spin)
#if defined PJ_SPIN
 real(SP)    :: mod_mag,vxc_plus(fft_size),vxc_minus(fft_size)
 complex(SP) :: V_xc_mat(fft_size,n_spin,n_spin)
#endif
 !
 ! Allocation 
 !
 allocate(V_xc(fft_size,n_spin),el_mag(fft_size,3))
 !
 ! ...LDA (and more) Vxc
 !
 call xc_lda_driver(E,Xk,GS_xc_KIND,GS_xc_FUNCTIONAL,1)
 QP_xc_FUNCTIONAL=trim(xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
 !
 if (n_spin==1) then
   ! 
   do i1=1,QP_n_states
     !
     ib=QP_table(i1,1)
     ik=QP_table(i1,3)
     !
     ifft=wf_state(ib,ik,1)
     c_ws(:,1)=wf(:,ifft)*V_xc(:,1)
     !
#if defined _DOUBLE
     QP_Vxc(i1)=zdotc(fft_size,wf(:,ifft),1,c_ws(:,1),1)
#else 
     QP_Vxc(i1)=cdotc(fft_size,wf(:,ifft),1,c_ws(:,1),1)
#endif
     !
   enddo
   !
 endif
 !
#if defined PJ_SPIN
 !
 if (n_spin==2) then
   !
   ! Magnetization, V_xc + and -
   !
   if (n_sp_pol==1) then
     do ifft=1,fft_size
       mod_mag=sqrt(el_mag(ifft,1)**2+el_mag(ifft,2)**2+el_mag(ifft,3)**2)
       el_mag(ifft,:)=el_mag(ifft,:)/mod_mag
     enddo
   else
     el_mag(:,3)=1.
   endif
   !
   vxc_plus(:) =(V_xc(:,1)+V_xc(:,2))/2.
   vxc_minus(:)=(V_xc(:,1)-V_xc(:,2))/2.
   !
   V_xc_mat=(0.,0.)
   !
   do ifft=1,fft_size 
     !
     ! V_xc_mat ( 1 , 1 ) = up up
     ! V_xc_mat ( 1 , 2 ) = up dn
     ! V_xc_mat ( 2 , 1 ) = dn up
     ! V_xc_mat ( 2 , 2 ) = dn dn
     !
     V_xc_mat(:,1,1)= (vxc_plus(:)+vxc_minus(:)*el_mag(:,3))
     V_xc_mat(:,2,2)= (vxc_plus(:)-vxc_minus(:)*el_mag(:,3))
     !
     if (n_spinor==1) cycle
     !
     V_xc_mat(:,1,2)= vxc_minus(:)*(el_mag(:,1)-(0.,1.)*el_mag(:,2))
     V_xc_mat(:,2,1)= vxc_minus(:)*(el_mag(:,1)+(0.,1.)*el_mag(:,2))
     !
   enddo
   !
   do i1=1,QP_n_states
     !
     ib=QP_table(i1,1)
     ik=QP_table(i1,3)
     !
     if (n_sp_pol==2) then
       !
       i_sp1=QP_table(i1,4)
       ifft_sp1=wf_state(ib,ik,i_sp1)
       !
       c_ws(:,1)=wf(:,ifft_sp1)*V_xc_mat(:,i_sp1,i_sp1)
       !
#if defined _DOUBLE
       QP_Vxc(i1)=zdotc(fft_size,wf(:,ifft_sp1),1,c_ws(:,1),1)
#else 
       QP_Vxc(i1)=cdotc(fft_size,wf(:,ifft_sp1),1,c_ws(:,1),1)
#endif
       !
     else
       !
       ifft_sp1=wf_state(ib,ik,1)
       ifft_sp2=wf_state(ib,ik,2)
       !
       c_ws(:,1)=V_xc_mat(:,1,1)*wf(:,ifft_sp1)+V_xc_mat(:,1,2)*wf(:,ifft_sp2)
       c_ws(:,2)=V_xc_mat(:,2,1)*wf(:,ifft_sp1)+V_xc_mat(:,2,2)*wf(:,ifft_sp2)
       !
#if defined _DOUBLE
       QP_Vxc(i1)=zdotc(fft_size,wf(:,ifft_sp1),1,c_ws(:,1),1)+&
&                 zdotc(fft_size,wf(:,ifft_sp2),1,c_ws(:,2),1)
#else 
       QP_Vxc(i1)=cdotc(fft_size,wf(:,ifft_sp1),1,c_ws(:,1),1)+&
&                 cdotc(fft_size,wf(:,ifft_sp2),1,c_ws(:,2),1)
#endif
       !
     endif
     !
   enddo
   !
 endif
 !
#endif
 !
 ! Clean
 !
 deallocate(V_xc,el_mag)
 !
 end subroutine
