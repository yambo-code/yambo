!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine load_SC_components(what,E,COM_,MODE_,ik,i_sp_pol,n_bands,kind,&
&                             xc_functional,perturbation)
 !
 use drivers,     ONLY:l_sc_run
 use SC,          ONLY:compatible_SC_DB,SC_bands,SC_kind,SC_neq_kind,&
&                      SC_xc_functional,SC_perturbation,found_SC_DB
 use hamiltonian, ONLY:H_nl_sc,H_rotation,H_rotation_ref
 use electrons,   ONLY:levels,E_reset,n_sp_pol
 use R_lattice,   ONLY:nkibz
 use IO_m,        ONLY:io_control,OP_RD_CL,RD_CL_IF_END,OP_RD,RD_CL,&
&                      DUMP,OP_RD_CL,NONE,IO_NO_DATABASE
#include<memory.h>
 !
 character(*)           :: what
 type(levels), optional :: E
 integer,      optional :: COM_
 integer,      optional :: MODE_
 integer,      optional :: ik,i_sp_pol
 integer,      optional :: n_bands
 integer,      optional :: kind
 integer,      optional :: xc_functional
 integer,      optional :: perturbation
 ! 
 ! Work Space
 !
 type(levels)      :: E_dummy
 integer           :: ID,ip,ip_sp_pol,io_err
 integer, external :: io_SC_components
 !
 compatible_SC_DB=.false.
 !
 if (l_sc_run) then
   found_SC_DB=.false.
   return
 endif
 !
 call E_reset(E_dummy)
 ! 
 if (what=="WF") then
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
 else if (what=="Hsc") then
   if (ik> 0.or. i_sp_pol> 0) call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
   if (ik==0.and.i_sp_pol==0) call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
 else if (what=="E".or.what=="V") then
   call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
 else if (what=="check_WF_DB") then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 endif
 !
 select case (what)
   !
   case("E")
     io_err=io_SC_components('E',E,ID)
     !
   case("V")
     io_err=io_SC_components('V',E_dummy,ID)
     !
   case("check_WF_DB")
     io_err=io_SC_components('WF',E_dummy,ID)
     !
   case("Hsc")
     io_err=io_SC_components('V',E_dummy,ID)
     if (io_err==0.and.ik>0.and.i_sp_pol>0) then
       if (ik==1.and.i_sp_pol==1) then
         YAMBO_ALLOC(H_nl_sc,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)))
       endif
       call io_control(ACTION=RD_CL,SEC=(/ip+1+(ip_sp_pol-1)*nkibz/),ID=ID)
       io_err=io_SC_components('V',E_dummy,ID)
     endif
     !
   case("WF")
     io_err=io_SC_components('WF',E_dummy,ID)
     if (io_err==0) then
       YAMBO_ALLOC(H_rotation,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),nkibz,n_sp_pol))
       if (trim(SC_neq_kind)=="matsubara") &
       &  YAMBO_ALLOC(H_rotation_ref,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),nkibz,n_sp_pol))
       do ip_sp_pol=1,n_sp_pol
         do ip=1,nkibz
           call io_control(ACTION=RD_CL_IF_END,SEC=(/ip+1+(ip_sp_pol-1)*nkibz/),ID=ID)
           io_err=io_SC_components('WF',E_dummy,ID)
         enddo
       enddo
     endif
     !
   case("Hsc_clean")
     if (allocated(H_nl_sc)) then
       YAMBO_FREE(H_nl_sc)
     endif
     return
     !
   case("WF_clean")
     if (allocated(H_rotation)) then
       YAMBO_FREE(H_rotation)
     endif
     if (allocated(H_rotation_ref)) then
       YAMBO_FREE(H_rotation_ref)
     endif
     return
     ! 
 end select
 !
 ! Define compatible_SC_DB/found_SC_DB flags
 !
 found_SC_DB      = .not.(io_err==IO_NO_DATABASE)
 compatible_SC_DB = io_err == 0
 !
 if (what=="check_WF_DB")  compatible_SC_DB = n_bands<= SC_bands(2)
 !
 if (io_err==0.and.present(kind)) then
   kind=SC_kind
   xc_functional=SC_xc_functional
   perturbation=SC_perturbation
 endif
 !
end subroutine
