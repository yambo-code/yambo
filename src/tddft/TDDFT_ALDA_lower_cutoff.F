!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine TDDFT_ALDA_lower_cutoff(ALDA_cut_scheme)
 !
 ! This subroutine performes a double FFT of F_xc
 ! F_xc(r) --> F_xc(G_l) --> F_xc(r)
 !
 ! Before performing the FFT bach to r space, it can lower the cutoff of F_xc(G)
 ! There are  three options
 ! a) "lower_GmGp" --> F_xc(G_l) with G_l=G-G' and a maximum cutoff is applied to G and G'
 ! b) "lower_Gmax" --> a cutoff is applied directly to G_l
 ! c) "full_grid"  --> no cutoff is applied (for debugging purposes)
 !
 ! The three options can be controlled from input. Notice that full grid 
 ! means Ng = fft_size, where Ng is the number of G-vectors is reciprocal space
 ! This means Ng >> ng_closed because F_xc is not at all smooth
 !
 ! The main goal of this subrotuine is to allow comparison between a
 ! TDDFT run in G-space and one in eh-space
 !
 use pars,          ONLY:DP,SP,cZERO
 use TDDFT,         ONLY:FXC_n_g_corr
 use BS,            ONLY:BS_n_g_fxc,l_BS_magnons
 use com,           ONLY:msg
 use FFT_m,         ONLY:fft_size,fftw_plan,fft_dim,fft_g_table
 use electrons,     ONLY:n_spin,n_spinor
 use R_lattice,     ONLY:G_m_G
 use xc_functionals,ONLY:F_xc_mat
 !
 implicit none
 !
 character(10) :: ALDA_cut_scheme
 ! 
 ! Work Space
 !
 integer     :: i1,i2,ig_fxc,isp1,isp2,isp3,isp4,n_holes
 complex(DP) :: F_xc_DP(fft_size),F_xc_DP_reduced(fft_size)
 logical     :: l_lower_GmGp, l_lower_Gmax, l_use_fullgd
 !
 if(trim(ALDA_cut_scheme)=="none") return
 !
 l_lower_GmGp=trim(ALDA_cut_scheme)=="lower_GmGp"
 l_lower_Gmax=trim(ALDA_cut_scheme)=="lower_Gmax"
 l_use_fullgd=trim(ALDA_cut_scheme)=="full_grid"
 !
 if (l_lower_GmGp) then
   BS_n_g_fxc=maxval(G_m_G(:FXC_n_g_corr,:FXC_n_g_corr))
   call PARSER_close_G(BS_n_g_fxc,'tRL')
   call msg('s',"[TDDFT] lowering the cutoff of F_xc(G-G') for G and G' to ",FXC_n_G_corr)
 endif
 !
 if (l_lower_Gmax) call msg('s',"[TDDFT] lowering the cutoff of F_xc(G) to ",BS_n_g_fxc)
 !
 do isp1=1,n_spin
 do isp2=1,n_spin
   do isp3=1,n_spin
   do isp4=1,n_spin
     !
     if (n_spinor==1) then
       if (l_BS_magnons) then
         if(isp1==isp2) cycle
         if(isp3==isp4) cycle
       else
         if(isp1/=isp2) cycle
         if(isp3/=isp4) cycle
       endif
     endif
     !
     F_xc_DP=cmplx(F_xc_mat(:,isp1,isp2,isp3,isp4),kind=DP)
     !
     ! FFT call: BACKWARD (R->G)
     !
#if defined _FFTW
     fftw_plan=0
     call fft_3d(F_xc_DP,fft_dim,-1,fftw_plan)
#else
     call fft_3d(F_xc_DP,fft_dim,-1)
#endif
     !
     ! Lower cutoff
     !
     ! PROCEDURE 1:
     !! I need FXC_n_g_corr up to wf_ng to arrive up to ng_closed
     !! This is consistent with what done in G-space TDDFT
     ! DEBUG <
     if (l_lower_GmGp) then
       F_xc_DP_reduced=cZERO
       do i1=1,FXC_n_g_corr
         do i2=1,FXC_n_g_corr
           ig_fxc=fft_g_table( G_m_G(i1,i2),1)
           if(abs(F_xc_DP_reduced(ig_fxc))>0._SP) cycle
           !write(*,*) i1,i2,G_m_G(i1,i2)
           F_xc_DP_reduced(ig_fxc)=F_xc_DP(ig_fxc)/real(fft_size,DP)
         enddo
       enddo
       !
       n_holes=0
       do i1=1,BS_n_g_fxc
         ig_fxc=fft_g_table(i1,1)
         if(abs(F_xc_DP_reduced(ig_fxc))>0._SP) cycle
         n_holes=n_holes+1
       enddo
     endif
     ! DEBUG >
     !
     ! PROCEDURE 2: I need FXC_n_g_corr up to ng_closed
     if (l_lower_Gmax) then
       n_holes=0
       F_xc_DP_reduced=cZERO
       do i1=1,BS_n_g_fxc
         ig_fxc=fft_g_table(i1,1)
         F_xc_DP_reduced(ig_fxc)=F_xc_DP(ig_fxc)/real(fft_size,DP)
       enddo
     endif
     !
     ! DEBUG <
     ! PROCEDURE 3: no lowering here
     ! In both previous procedures I can use up to ng_closed vectors max
     ! However Fxc may need more G vecs, up to G-G' with both running till n_g_closed in principle.
     ! With this direct procedure all g-vectors are used
     if (l_use_fullgd) F_xc_DP_reduced=F_xc_DP/real(fft_size,DP)
     ! DEBUG >
     !
     ! FFT call: FORWARD (G->R)
     !
#if defined _FFTW
     call dfftw_destroy_plan(fftw_plan)
     fftw_plan = 0
     call fft_3d(F_xc_DP_reduced,fft_dim,1,fftw_plan)
#else
     call fft_3d(F_xc_DP_reduced,fft_dim,1)
#endif
     !
     F_xc_mat(:,isp1,isp2,isp3,isp4) = real(F_xc_DP_reduced,SP)
     !
   enddo
   enddo
 enddo
 enddo
 !
 if (l_lower_GmGp) call msg('s','[TDDFT] while lowering the cutoff F_xc is not defined on n_points in the last shells ',n_holes)
 !
 ! DEBUG <
 ! write(*,*) "F_xc= ",F_xc_matt(1:10,1,1,1,1)
 ! DEBUG >
 !
end subroutine
