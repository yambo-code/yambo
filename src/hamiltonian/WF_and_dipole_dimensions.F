!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine WF_and_dipole_dimensions(X)
 !
 use drivers,      ONLY:l_real_time,l_sc_fock,l_sc_exx,l_sc_coh,l_sc_sex,l_sc_srpa,l_use_Hxc_collisions
 use com,          ONLY:msg
 use X_m,          ONLY:X_t
 use wave_func,    ONLY:wf_ng
 use QP_m,         ONLY:QP_ng_Sx,QP_ng_SH,QP_ng_Vxc,QP_n_G_bands,COHSEX_use_empties
 use R_lattice,    ONLY:qindx_S,G_m_G
 use interfaces,   ONLY:eval_G_minus_G
 use hamiltonian,  ONLY:H_ref_bands,WF_Go_indx,WF_G_max
#if defined _SC
 use SC,           ONLY:SC_X_bands,SC_ng_Sx,SC_X_size,SC_up_W_iters
#endif
 !
 implicit none
 !
 type(X_t)         :: X(2)
 !
 ! Dipoles dimensions
 !===================
 !
 ! In real-time I need X(1)%ib(1)=1 in order to calculate E_kin
 !
 if (l_real_time) then
   X(1)%ib=H_ref_bands
   X(1)%ng=wf_ng
 else
   X(1)%ib=(/1,H_ref_bands(2)/)
   X(1)%ng=QP_ng_Sx
 endif
 !
 ! WF SIZE
 !=========
 !
 WF_G_max=0
 WF_Go_indx=1
 !
 if ( l_sc_fock.or.l_sc_exx.or.l_sc_coh.or.l_sc_sex ) then
   !
   WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   !
   if (l_sc_coh.or.l_sc_sex.or.l_sc_srpa) then
     !
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
     if (l_real_time.and..not.(l_sc_exx.or.l_sc_srpa)) WF_G_max=wf_ng
     !
     ! [07/07/2016] Modified these two lines to avoid the annoying QP_n_G_bands in the input.
     ! At the moment I do not see any reason to use the same bands of the density here.
     !
     QP_n_G_bands(1)=H_ref_bands(1)
     QP_n_G_bands(2)=H_ref_bands(2)
     !
     if (     l_real_time) COHSEX_use_empties=.TRUE.
     !
   else if (l_sc_exx)  then
     WF_G_max=max(QP_ng_Sx,X(1)%ng)
   else if (l_sc_fock)  then
     WF_G_max=QP_ng_Sx
   endif
   !
 endif
 !
 WF_G_max=maxval((/QP_ng_SH,QP_ng_Vxc,WF_G_max/))
 !
 ! SC shadow variables
 !====================
 !
#if defined _SC
 SC_ng_Sx  =QP_ng_Sx
 SC_X_bands=X(2)%ib
 SC_X_size =X(2)%ng
 !
 if(SC_up_W_iters/=0) then
   call msg('nr','[SC] Updating W each                   :',SC_up_W_iters)
   !
   if(l_use_Hxc_collisions) then
     SC_up_W_iters=0
     call warning(' Collisions not compatible with W updating, turned off ')
   endif
   !
   if(.not.(l_sc_coh.and.l_sc_sex)) then
     SC_up_W_iters=0
     call warning(' Dipoles update implemented only for COHSEX self-energy ')
   endif
   !
 endif    
#endif
 !
end subroutine
