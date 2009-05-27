!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
#if defined _RT
subroutine DFT_Vxc(E,Xk,G_lesser)
#else
subroutine DFT_Vxc(E,Xk)
#endif
 !
 ! V_xc
 !
 use pars,          ONLY:SP,DP,pi
 use electrons,     ONLY:levels,n_spin,n_spinor,n_sp_pol,spin
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table,QP_nk
 use R_lattice,     ONLY:bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:wf,wf_state
 use xc_functionals,ONLY:V_xc,xc_string,el_mag
 use global_XC,     ONLY:WF_kind,WF_xc_functional
#if defined _SC || defined _RT
 use com,           ONLY:error
 use IO_m,          ONLY:REP,VERIFY
 use SC,            ONLY:load_SC_components,V_xc_sc,V_hartree_sc,SC_fft_size, &
 &                       SC_bands
#endif
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
#if defined _RT
 complex(SP),   intent(in) :: G_lesser(SC_bands,SC_bands,QP_nk)
#endif
 !
 ! Work Space
 !
 integer     ::i1,ifft,ifftp,ifft_sp1,ifftp_sp1,ifft_sp2,ib,ibp,ik,i_sp1,io_err
#if defined _DOUBLE
 complex(DP) ::zdotc,zdotu
#else 
 complex(SP) ::cdotc,cdotu
#endif
 complex(SP) ::c_ws(fft_size,n_spin)
 !
 ! Spin support (collinear and not collinear)
 !
 real(SP)    :: mod_mag,vxc_plus(fft_size),vxc_minus(fft_size)
 complex(SP) :: V_xc_mat(fft_size,n_spin,n_spin)
 !
 ! Allocation 
 !
 allocate(V_xc(fft_size,n_spin),el_mag(fft_size,3))
 !
#if defined _SC || defined _RT
 if (WF_KIND< 300 ) then
   !
   ! ...LDA (and more) Vxc
   !
#endif
#if defined _RT
   call xc_lda_driver(E,Xk,WF_KIND,WF_xc_functional,1,G_lesser)
#else
   call xc_lda_driver(E,Xk,WF_KIND,WF_xc_functional,1)
#endif
#if defined _SC || defined _RT
 else
   !
   ! ...SC EXX and EXX+LDA_C
   !
   allocate(V_xc_sc(fft_size,n_spin),V_hartree_sc(fft_size,n_spin))
   SC_fft_size=fft_size
   call load_SC_components('V',COM_=REP,MODE_=VERIFY,io_err_=io_err)
   !
   if (io_err/=0) call error('SC potential FFT size is wrong')
   !
   V_xc=V_xc_sc
   deallocate(V_xc_sc,V_hartree_sc)
 endif
#endif
 !
 if (n_spin==1) then
   ! 
   do i1=1,QP_n_states
     !
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     !
     ! <ifft|V_xc|ifftp>
     !
     ifft =wf_state(ib,ik,1)
     ifftp=wf_state(ibp,ik,1)
     c_ws(:,1)=V_xc(:,1)*wf(:,ifftp)
     !
#if defined _DOUBLE
     QP_Vxc(i1)=zdotc(fft_size,wf(:,ifft),1,c_ws(:,1),1)
#else 
     QP_Vxc(i1)=cdotc(fft_size,wf(:,ifft),1,c_ws(:,1),1)
#endif
     !
   enddo
   !
   deallocate(el_mag)
   return
   !
 endif
 !
 ! GPL_EXCLUDE_START  SPIN_IGNORE
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
   ! Vxc^+=(Vxc_up+Vxc_down)/2
   ! Vxc^+=(Vxc_up-Vxc_down)/2
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
     ibp=QP_table(i1,2)
     ik=QP_table(i1,3)
     !
     if (n_sp_pol==2) then
       !
       i_sp1=spin(QP_table(i1,:))
       ifft_sp1=wf_state(ib,ik,i_sp1)
       ifftp_sp1=wf_state(ibp,ik,i_sp1)
       !
       c_ws(:,1)=wf(:,ifftp_sp1)*V_xc_mat(:,i_sp1,i_sp1)
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
 ! Clean
 !
#if !defined _SC && !defined _RT
 deallocate(V_xc)
#endif
 deallocate(el_mag)
 ! 
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
end subroutine
