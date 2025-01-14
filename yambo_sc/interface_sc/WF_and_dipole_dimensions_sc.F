!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine WF_and_dipole_dimensions_sc(X,Xk)
 !
 use drivers,      ONLY:l_real_time,l_sc_fock,l_sc_exx,l_sc_coh,l_sc_sex,l_sc_srpa,&
&                       l_sc_hartree,l_use_Hxc_collisions
 use com,          ONLY:msg
 use X_m,          ONLY:X_t
 use wave_func,    ONLY:wf_ng
 use QP_m,         ONLY:QP_ng_Sx,QP_ng_SH,QP_ng_Vxc,QP_n_G_bands,COHSEX_use_empties
 use R_lattice,    ONLY:qindx_S,G_m_G,bz_samp,qindx_S_max_Go
 use interfaces,   ONLY:eval_G_minus_G
 use hamiltonian,  ONLY:H_ref_bands,WF_Go_indx,WF_G_max,l_sc_XC_is_local
 use SC,           ONLY:SC_X_bands,SC_ng_Sx,SC_X_size,SC_up_W_iters
 !
 implicit none
 !
 type(X_t)         :: X(2)
 type(bz_samp)     :: Xk
 !
call WF_and_dipole_dimensions(X,Xk)
 !
 if ( l_sc_fock.or.l_sc_exx.or.l_sc_coh.or.l_sc_sex ) then
   !
   if(allocated(qindx_S)) then
     WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   elseif(qindx_S_max_Go/=-1) then
     WF_Go_indx=qindx_S_max_Go 
   else 
     call error('Max Go not defined run setup again')
   endif
   !
   if (l_sc_coh.or.l_sc_sex.or.l_sc_srpa) then
     !
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     !
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
 if (l_sc_hartree    ) WF_G_max=maxval((/QP_ng_SH, WF_G_max/))
 if (l_sc_XC_is_local) WF_G_max=maxval((/QP_ng_Vxc,WF_G_max/))
 !
 ! SC shadow variables
 !====================
 !
 SC_ng_Sx  =QP_ng_Sx
 SC_X_bands=X(2)%ib
 SC_X_size =X(2)%ng
 !
 if(SC_up_W_iters/=0) then
   call msg('nr','[SC] Updating W each                   ',SC_up_W_iters)
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
 !
end subroutine
