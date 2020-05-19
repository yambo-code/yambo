!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV DS
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
subroutine excitons_read(k,Xk,en,Xen,iq,what)
 !
 use pars,          ONLY:pi
 use R_lattice,     ONLY:bz_samp
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_reset
 use QP_CTL_m,      ONLY:QP_apply
 use electrons,     ONLY:levels,n_spin,n_sp_pol,spin,E_SOC_nbands
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY,io_disconnect
 use BS_solvers,    ONLY:BSS_eh_table,BSS_write_eig_2_db,io_BSS_diago,BS_mat,BSS_n_eig
 use BS,            ONLY:BS_H_dim,BS_K_dim,BS_bands,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr,BSqpts
 use YPPm,          ONLY:BS_R_left,BS_R_right,BS_E,BS_E_SOC_corr,BS_R_PL,BS_R_kerr,Xbsk   
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk,k
 type(levels)  ::Xen,en
 integer, intent(in) :: iq
 character(*)        :: what
 !
 ! Work Space
 !
 !
 !... I/0
 !
 integer           ::io_err,io_ID,i_sec
 integer, external ::io_BS,io_E_SOC_map
 !
 integer                  :: neh
 logical                  :: l_check,l_eigenval,l_eigenvec,l_clean
 !
 l_check   =index(what,"check")        >0
 l_eigenval=index(what,"eigenvalues")  >0
 l_eigenvec=index(what,"eigenvectors") >0
 l_clean   =index(what,"clean")        >0
 !
 if(l_check) then
   !
   ! GPL_EXCLUDE_START
   !
   ! E SOC perturbative
   ! 
   if(n_spin==1) then
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
     io_err=io_E_SOC_map(en,k,io_ID)
   endif
   !
   ! GPL_EXCLUDE_END
   !
   YAMBO_ALLOC(BSqpts,(3,iq))
   !
   ! X type. Also used in PARALLEL setup
   !
   Xbsk%whoami=2
   !
   ! Here I read information from the BSS_diago database
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,Xbsk)
   !
   if (io_err/=0) call error(' ndb.BS_diago not found')
   !
   ! Apply quasi-particle correction if presents
   ! 
   call QP_apply(BS_bands,Xen,Xk,'G',msg_fmt='s')
   !
   if (BS_cpl_K_exchange)                   BS_cpl_mode='x'
   if (BS_cpl_K_corr)                       BS_cpl_mode='c'
   if (BS_cpl_K_exchange.and.BS_cpl_K_corr) BS_cpl_mode='xc'
   !
   BS_H_dim=BS_K_dim
   if (BS_K_coupling) BS_H_dim=2*BS_K_dim
   !
 endif
 !
 if(l_eigenval) then
   !
   ! Allocation of energies and residuals, then also eigenvector
   !
   YAMBO_ALLOC(BS_R_right,(BSS_n_eig))
   YAMBO_ALLOC(BS_R_left,(BSS_n_eig))
   YAMBO_ALLOC(BS_E,(BSS_n_eig))
   if(E_SOC_nbands/=0) then
     YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BSS_n_eig))
   endif
   !
   !
   do i_sec=1,2
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/i_sec/),ID=io_ID)
     io_err=io_BSS_diago(iq,io_ID,Xbsk,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
     if (io_err/=0) call error(' ndb.BSS_diago, SEC '//trim(intc(i_sec))//', not compatible with ypp input')
   enddo
   !
   call io_disconnect(ID=io_ID)
   !
 endif
 !
 if(l_eigenvec) then
   ! 
   if(.not.BSS_write_eig_2_db) &
&    call error(' Diagonalization database does not contain excitonic eigenstates.'//&
&               ' Re-run yambo using the WRbsWF flag in the input file.')
   !
   ! Adjust the user provided indexes to the average ones.
   !
   ! Loading tables and eigenvectors
   !
   YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
   YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/3/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,Xbsk,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_kerr,BS_R_PL)
   if (io_err/=0) call error(' ndb.BSS_diago, SEC 3, not compatible with ypp input')
   !
   ! Create the anti-resonant part of the eh_table
   !
   if (BS_K_coupling) then
     do neh=BS_K_dim+1,BS_H_dim
       !
       BSS_eh_table(neh,:)=BSS_eh_table(neh-BS_K_dim,:)
       !
       ! invert conduction <-> valence order
       !
       BSS_eh_table(neh,2)=BSS_eh_table(neh-BS_K_dim,3)
       BSS_eh_table(neh,3)=BSS_eh_table(neh-BS_K_dim,2)
       !
     enddo
   endif
   !
 endif
 !
 if(l_clean) then
   !         
   ! ! CLEAN
   ! !
   YAMBO_FREE(BS_R_left)
   YAMBO_FREE(BS_R_right)
   YAMBO_FREE(BS_E)
   YAMBO_FREE(BSS_eh_table)
   YAMBO_FREE(BS_mat)
   YAMBO_FREE(BSqpts)
   if(associated(BS_E_SOC_corr)) then
     YAMBO_FREE_P(BS_E_SOC_corr)
   endif
   call X_reset(Xbsk)
   !
 endif
 !
end subroutine
