!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team
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
subroutine SC_WF_and_dipole_dimensions(X)
 !
 use drivers,      ONLY:l_real_time,l_sc_hf,l_sc_exx,l_sc_cohsex
 use X_m,          ONLY:X_t,Eval_P_and_P2_only
 use wave_func,    ONLY:wf_ng
 use QP_m,         ONLY:QP_ng_Sx,QP_n_G_bands,COHSEX_use_empties
 use R_lattice,    ONLY:qindx_S,G_m_G
 use SC,           ONLY:SC_bands,WF_Go_indx,WF_G_max,SC_X_bands,SC_ng_Sx,SC_X_size
 implicit none
 !
 type(X_t)         :: X(2)
 integer, external :: eval_G_minus_G
 !
 ! Dipoles dimensions
 !===================
 !
 X(1)%ib(2)=SC_bands
 if (l_real_time) then
   X(1)%ng=wf_ng
   Eval_P_and_P2_only=.true.
 else
   X(1)%ng=QP_ng_Sx
 endif
 !
 ! WF SIZE
 !=========
 !
 WF_G_max=0
 WF_Go_indx=1
 !
 if ( l_sc_hf.or.l_sc_exx.or.l_sc_cohsex ) then
   WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   if (l_sc_cohsex) then
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
     QP_n_G_bands(2)=SC_bands
     !
     if (.not.l_real_time) COHSEX_use_empties=.TRUE. !<- DEBUG. Related to
                                                     !   problems in defining the COLLISIONS
     if (     l_real_time) COHSEX_use_empties=.TRUE.
     !
   else if (l_sc_exx)  then
     WF_G_max=max(QP_ng_Sx,X(1)%ng)
   endif
 endif
 !
 ! SC shadow variables
 !====================
 !
 SC_ng_Sx  =QP_ng_Sx
 SC_X_bands=X(2)%ib
 SC_X_size =X(2)%ng
 !
end subroutine
