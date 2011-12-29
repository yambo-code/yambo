!
! Copyright (C) 2000-2012 A. Marini, and M. Gruening and the YAMBO team 
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
subroutine load_SC_components(what,E,COM_,MODE_,ik,n_bands,kind,&
&                             xc_functional,perturbation)
 !
 use drivers,   ONLY:l_sc_run
 use SC,        ONLY:compatible_SC_DB,SC_R,SC_bands,H_nl_sc,SC_kind,&
&                    SC_xc_functional,SC_perturbation,found_SC_DB
 use electrons, ONLY:levels,E_reset,n_sp_pol
 use R_lattice, ONLY:nkibz
 use memory_m,  ONLY:mem_est
 use IO_m,      ONLY:io_control,OP_RD_CL,RD_CL_IF_END,OP_RD,RD_CL,&
&                    DUMP,OP_RD_CL,NONE,IO_NO_DATABASE
 implicit none
 !
 character(*)           :: what
 type(levels), optional :: E
 integer,      optional :: COM_
 integer,      optional :: MODE_
 integer,      optional :: ik
 integer,      optional :: n_bands
 integer,      optional :: kind
 integer,      optional :: xc_functional
 integer,      optional :: perturbation
 ! 
 ! Work Space
 !
 type(levels)      :: E_dummy
 integer           :: ID,ip,io_err
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
   if (ik> 0) call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
   if (ik==0) call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
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
     if (io_err==0.and.ik>0) then
       if (ik==1) then
         allocate(H_nl_sc(SC_bands(2),SC_bands(2),n_sp_pol))
         call mem_est("H_nl_sc",(/size(H_nl_sc)/))
       endif
       do ip=1,ik
         if (ip< ik) call io_control(ACTION=RD_CL_IF_END,SEC=(/ip+1/),ID=ID)
         if (ip==ik) call io_control(ACTION=RD_CL,SEC=(/ip+1/),ID=ID)
         io_err=io_SC_components('V',E_dummy,ID)
       enddo
     endif
     !
   case("WF")
     io_err=io_SC_components('WF',E_dummy,ID)
     if (io_err==0) then
       allocate(SC_R(SC_bands(2),SC_bands(2),nkibz,n_sp_pol))
       call mem_est("SC_R",(/size(SC_R)/))
       do ip=1,nkibz
         call io_control(ACTION=RD_CL_IF_END,SEC=(/ip+1/),ID=ID)
         io_err=io_SC_components('WF',E_dummy,ID)
       enddo
     endif
     !
   case("Hsc_clean")
     if (allocated(H_nl_sc)) then
       deallocate(H_nl_sc)
       call mem_est("H_nl_sc")
       return
     endif
     !
   case("WF_clean")
     if (allocated(SC_R)) then
       deallocate(SC_R)
       call mem_est("SC_R")
       return
     endif
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
