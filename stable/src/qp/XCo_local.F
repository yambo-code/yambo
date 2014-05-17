!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine XCo_local(E,Xk)
 !
 ! Local V_xc
 !
 use pars,          ONLY:SP,pi
 use electrons,     ONLY:levels,n_spin,n_spinor,spin
 use QP_m,          ONLY:QP_Vxc,QP_n_states,QP_table
 use R_lattice,     ONLY:bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:wf,wf_state
 use xc_functionals,ONLY:V_xc,xc_string
 use xc_functionals,ONLY:magn
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use wrapper,       ONLY:Vstar_dot_V
#if defined _SC
 use com,           ONLY:error
 use IO_m,          ONLY:REP,VERIFY
 use SC,            ONLY:load_SC_components,V_xc_sc,V_hartree_sc,SC_fft_size,compatible_SC_DB
#endif
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 integer     ::i1,ifft,iwf,iwfp,iwf_sp1,iwfp_sp1,iwf_sp2,iwfp_sp2,ib,ibp,ik,i_sp
 complex(SP) ::c_ws(fft_size,n_spin)
 !
 ! Spin support (collinear and not collinear)
 !
 real(SP)    :: mod_mag,magn_versor(fft_size,3),vxc_plus(fft_size),vxc_minus(fft_size)
 complex(SP) :: V_xc_mat(fft_size,n_spin,n_spin)
 logical     :: l_call_the_driver
 !
 ! Allocation 
 !
 if (.not.allocated(V_xc))   allocate(V_xc(fft_size,n_spin))
 !
 if (n_spin>1.and..not.allocated(magn)) allocate(magn(fft_size,3))
 !
 ! Precompiler options in case a non-DFT potential is required
 !
#if defined _SC
 if (WF_KIND< 300 ) then
   !
   ! ...LDA (and more) Vxc
   !
#endif
   !
   l_call_the_driver=.TRUE.
   !
#if defined _RT
   call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,1)
   l_call_the_driver=.FALSE.
#endif
   if (l_call_the_driver) then
     call XC_potential_driver(E,Xk,WF_KIND,WF_xc_functional,1)
   endif
   !
#if defined _SC
 else
   !
   ! ...SC EXX and EXX+LDA_C
   !
   allocate(V_xc_sc(fft_size,n_spin),V_hartree_sc(fft_size,n_spin))
   SC_fft_size=fft_size
   call load_SC_components('V',COM_=REP,MODE_=VERIFY)
   !
   if (.not.compatible_SC_DB) call error('Incompatible potential database')
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
     ! <iwf|V_xc|iwfp>
     !
     iwf =wf_state(ib,ik,1)
     iwfp=wf_state(ibp,ik,1)
     c_ws(:,1)=V_xc(:,1)*wf(:,iwfp)
     !
     QP_Vxc(i1)=Vstar_dot_V(fft_size,wf(:,iwf),c_ws(:,1))
     !
   enddo
   !
   return
   !
 endif
 !
 if (n_spin==2) then
   !
   V_xc_mat=(0._SP,0._SP)
   !
   ! V_xc_mat ( 1 , 1 ) = up up
   ! V_xc_mat ( 1 , 2 ) = up dn
   ! V_xc_mat ( 2 , 1 ) = dn up
   ! V_xc_mat ( 2 , 2 ) = dn dn
   !
   ! Magnetization, V_xc + and -
   !
   if (n_spinor==1) then
     !
     V_xc_mat(:,1,1)= V_xc(:,1)
     V_xc_mat(:,2,2)= V_xc(:,2)
     !
   else if (n_spinor==2) then
     !
     magn_versor=0.
     do ifft=1,fft_size
       mod_mag=sqrt(magn(ifft,1)**2+magn(ifft,2)**2+magn(ifft,3)**2)
       if(mod_mag==0) magn_versor(ifft,:)=0.
       if(mod_mag/=0) magn_versor(ifft,:)=magn(ifft,:)/mod_mag
     enddo
     !
     ! Vxc^+=(Vxc_up+Vxc_down)/2
     ! Vxc^+=(Vxc_up-Vxc_down)/2
     vxc_plus(:) =(V_xc(:,1)+V_xc(:,2))/2._SP
     vxc_minus(:)=(V_xc(:,1)-V_xc(:,2))/2._SP
     !
     V_xc_mat(:,1,1)= (vxc_plus(:)+vxc_minus(:)*magn_versor(:,3))
     V_xc_mat(:,2,2)= (vxc_plus(:)-vxc_minus(:)*magn_versor(:,3))
     !
     V_xc_mat(:,1,2)= vxc_minus(:)*(magn_versor(:,1)-(0.,1.)*magn_versor(:,2))
     V_xc_mat(:,2,1)= vxc_minus(:)*(magn_versor(:,1)+(0.,1.)*magn_versor(:,2))
     !
   endif
   !
   do i1=1,QP_n_states
     !
     ib =QP_table(i1,1)
     ibp=QP_table(i1,2)
     ik =QP_table(i1,3)
     !
     if (n_spinor==1) then
       !
       i_sp=spin(QP_table(i1,:))
       iwf =wf_state(ib,ik,i_sp)
       iwfp=wf_state(ibp,ik,i_sp)
       !
       c_ws(:,1)=V_xc_mat(:,i_sp,i_sp)*wf(:,iwfp)
       !
       QP_Vxc(i1)=Vstar_dot_V(fft_size,wf(:,iwf),c_ws(:,1))
       !
     else
       !
       iwf_sp1=wf_state(ib,ik,1)
       iwf_sp2=wf_state(ib,ik,2)
       iwfp_sp1=wf_state(ibp,ik,1)
       iwfp_sp2=wf_state(ibp,ik,2)
       !
       c_ws(:,1)=V_xc_mat(:,1,1)*wf(:,iwfp_sp1)+V_xc_mat(:,1,2)*wf(:,iwfp_sp2)
       c_ws(:,2)=V_xc_mat(:,2,1)*wf(:,iwfp_sp1)+V_xc_mat(:,2,2)*wf(:,iwfp_sp2)
       !
       QP_Vxc(i1)=Vstar_dot_V(fft_size,wf(:,iwf_sp1),c_ws(:,1))+&
&                 Vstar_dot_V(fft_size,wf(:,iwf_sp2),c_ws(:,2))
       !
     endif
     !
   enddo
   !
 endif
 !
 ! Clean
 !
#if !defined _SC
 if(n_spin>1) deallocate(magn)
#endif
 ! 
#if !defined _SC
 deallocate(V_xc)
#endif
 !
end subroutine
