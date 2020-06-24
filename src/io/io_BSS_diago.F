!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
integer function io_BSS_diago(iq,ID,X_static,bsE,bsRl,bsRr,BsE_corr,bsR_kerr,bsR_pl)
 !
 use pars,          ONLY:SP,lchlen,schlen,IP
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_K_dim,BS_H_dim,BSqpts
 use BS_solvers,    ONLY:BSS_q0,BSS_write_eig_2_db,BSS_eh_table,BS_mat,variables_BS,BSS_n_eig
 use X_m,           ONLY:X_t
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,io_header
 use electrons,     ONLY:n_sp_pol,E_SOC_nbands
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 type(X_t),intent(inout)        :: X_static
 integer                        :: iq,ID
 complex(SP)        , optional  :: bsE(BSS_n_eig),bsRl(BSS_n_eig),bsRr(BSS_n_eig)
 real(SP)   ,pointer, optional  :: BsE_corr(:,:),bsR_pl(:,:)
 complex(SP),pointer, optional  :: bsR_kerr(:)
 !
 ! Work Space
 !
 character(schlen):: db_name
 character(lchlen):: local_description(100)
 !
 call timing('io_BSS_diago',OPR='start')
 !
 db_name='BS_diago_Q'//trim(intc(iq))
 !
 io_BSS_diago=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_BSS_diago/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS ideentifier I only need to
   ! check the K QP corrections
   !
   io_BSS_diago=io_header(ID,XC_KIND="K_E force")
   !
   io_BSS_diago=variables_BS(ID,iq,local_description,.FALSE.,X=X_static)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
   !
   call io_elemental(ID,VAR=&
&       " BSS|Eigenstates included    :",L0=BSS_write_eig_2_db,CHECK=.true.,OP=(/"=="/))
   if(iq==1) then
     call io_elemental(ID,VAR=&
&       "    |Field direction         :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
     if (allocated(BSqpts)) BSqpts(:,iq)=BSS_q0
   endif
   call io_elemental(ID,VAR=&
&       "    |Eigenstates computed    :",I0=BSS_n_eig,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ! header return
   !
   if (io_BSS_diago/=0) goto 1
   !
   io_BSS_diago=io_status(ID)
   if (io_BSS_diago/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (present(bsE)) then
     !
     call io_bulk(ID,'BS_Energies',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsE)
     !
   endif
   !
   if (present(bsE_corr)) then
     if (associated(bsE_corr)) then
       if(E_SOC_nbands/=0) then
         call io_bulk(ID,'BS_Energies_SOC_corr',VAR_SZ=(/2/n_sp_pol,BSS_n_eig/))
         call io_bulk(ID,R2=BsE_corr)
       endif
     endif
   endif
   !
   if (present(bsRl)) then
     call io_bulk(ID,'BS_left_Residuals',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsRl)
   endif
   !
   if (present(bsRr)) then
     call io_bulk(ID,'BS_right_Residuals',VAR_SZ=(/2,BSS_n_eig/))
     call io_bulk(ID,C1=bsRr)
   endif
   !
   ! GPL_EXCLUDE_START
   !
   if(present(bsR_kerr)) then
     if(associated(bsR_kerr)) then
       call io_bulk(ID,'BS_KERR_Residuals',VAR_SZ=(/2,BSS_n_eig/))
       call io_bulk(ID,C1=bsR_kerr)
     endif
   endif
   !
   if(present(bsR_pl)) then
     if(associated(bsR_pl)) then
       call io_bulk(ID,'BS_PL_Residuals',VAR_SZ=(/2,BSS_n_eig/))
       call io_bulk(ID,R2=bsR_pl)
     endif
   endif
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 if (any((/io_sec(ID,:)==3/))) then
   ! 
   ! If not switched on skip this section
   !
   if (.not.BSS_write_eig_2_db) then
     io_BSS_diago=-1
     goto 1
   endif
   !
   ! BS_eh_table
   !
   call io_bulk(ID,'BS_TABLE',VAR_SZ=(/BS_K_dim,3+n_sp_pol-1/))
   call io_bulk(ID,I2=BSS_eh_table(:BS_K_dim,:))
   !
   ! Eigenstates (resonant only matrix)
   !
   call io_bulk(ID,'BS_EIGENSTATES',VAR_SZ=(/2,BS_H_dim,BSS_n_eig/))
   call io_bulk(ID,C2=BS_mat,IPOS=(/1,1,1/))
   !
 endif
 !
1 call io_disconnect(ID)
 !
 call timing('io_BSS_diago',OPR='stop')
 !
end function
