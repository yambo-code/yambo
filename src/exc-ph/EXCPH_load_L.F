!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
! --- Theory ---
! (5) Figure out rotations/symmetry of exciton eigenvectors and EPC m.e.
! - FP
!
subroutine EXCPH_load_L(iq_bse,X,what,Ltype)
 !
 use pars,           ONLY:lchlen
 use BS_solvers,     ONLY:io_BSS_diago,BSS_n_eig,BSS_free,BSS_write_eig_2_db,BSS_n_eig
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message
 use R_lattice,      ONLY:bz_samp
 use BS,             ONLY:BS_H_dim,L_kind,BS_res_ares_n_mat,BS_K_dim
 use EXCPH,          ONLY:BS_E,BS_R_left,BS_R_right,EXCPH_states,EXCPH_sum,EXCPH_kind,Lout_path,L_kind_in,L_kind_out
 use IO_int,         ONLY:io_control
 use stderr,         ONLY:intc
 use X_m,            ONLY:X_t
 use com,            ONLY:jobstr,msg
 !
#include<memory.h>
 ! 
 type(X_t)     ::X
 type(bz_samp) ::k,q
 integer, intent(in) :: iq_bse     
 character(*)        :: what,Ltype
 !
 ! Work space
 !
 integer :: ID_BS,io_err
 character(lchlen) :: jobstr_save
 logical :: l_read_residuals
 !
 if(index(Ltype,'Lout')>0.and.Lout_path/='none') then
   jobstr_save=jobstr
   jobstr     =trim(Lout_path)
 endif
 !
 l_read_residuals=.FALSE.
 if(index(what,'residuals')>0) l_read_residuals=.TRUE.
 !
 if(index(what,'check')>0) then
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID_BS)
   io_err=io_BSS_diago(iq_bse,1,ID_BS,X)
   if(io_err/=0) call error(" Error checking "//trim(Ltype)//" for q = "//trim(intc(iq_bse)))
   !
   if (BS_res_ares_n_mat/=1) call error("Excitonic Gkkp: anti-resonant part not implemented")
   !
   if(.not.BSS_write_eig_2_db) call error("BSS diago DB does not contain the eigenstates")
   !
   BS_H_dim=BS_K_dim(1)
   !
   if(EXCPH_states(2)>BSS_n_eig)  &
&     call error("Number of exc. states in the excphon coupling must         <= number of BSE eigenstates")
   !
   if(EXCPH_sum(2)>BS_H_dim)      &
&     call error("Number of virtual exc. states in the excphon coupling must <= size of the exc. Hamiltonian")
   !
   if(iq_bse==1) then
     call msg('rs','Found '//trim(Ltype)//' with dimension: '//trim(intc(BSS_n_eig))//' x '//trim(intc(BS_H_dim)))
     if(index(Ltype,'Lout')>0) then
       L_kind_out=L_kind
       call msg('rs','L kind out: '//trim(L_kind_out))
     elseif(index(Ltype,'Lin')>0) then
       L_kind_in =L_kind
       call msg('rs','L kind  in: '//trim(L_kind_in))
     endif
   endif
   !
 elseif(index(what,'eigenvalues')>0) then
   !
   ! Read eigenvalues and optionally residuals
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2/),ID=ID_BS)
   if(l_read_residuals) then
      io_err=io_BSS_diago(iq_bse,1,ID_BS,X,bsE=BS_E,bsRr=BS_R_right,bsRl=BS_R_left)
   else
      io_err=io_BSS_diago(iq_bse,1,ID_BS,X,bsE=BS_E)
   endif
   if(io_err/=0) call error(" Error loading "//trim(Ltype)//" for q = "//trim(intc(iq_bse)))
   !
 elseif(index(what,'load')>0) then
   !      
   ! Read BSE eigenvectors and eigenvalues and optionally residuals
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2,3/),ID=ID_BS)
   if(l_read_residuals) then
      io_err=io_BSS_diago(iq_bse,1,ID_BS,X,bsE=BS_E,bsRr=BS_R_right,bsRl=BS_R_left)
   else
      io_err=io_BSS_diago(iq_bse,1,ID_BS,X,bsE=BS_E)
   endif
   if(io_err/=0) call error(" Error loading "//trim(Ltype)//" for q = "//trim(intc(iq_bse)))
   !
 else
   call error(" Wrong call to the EXCPH_load_L function! ")
 endif
 !
 if(index(Ltype,'Lout')>0.and.Lout_path/='none') then
   jobstr     =jobstr_save
 endif
 !
end subroutine EXCPH_load_L
