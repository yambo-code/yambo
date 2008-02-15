!
! Copyright (C) 2000-2005 M. Gruening and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine load_SC_components(what,E,COM_,MODE_)
 !
 use pars,      ONLY:SP
 use drivers,   ONLY:l_sc_run
 use SC,        ONLY:E_of_WFo,using_SC_basis,SC_R,SC_bands
 use electrons, ONLY:levels,n_sp_pol
 use R_lattice, ONLY:nkibz
 use memory_m,  ONLY:mem_est
 use IO_m,      ONLY:io_control,OP_RD_CL,RD_CL_IF_END,OP_RD
 implicit none
 !
 character(*)           :: what
 integer                :: COM_,MODE_
 type(levels), optional :: E
 ! 
 ! Work Space
 !
 type(levels) :: E_dummy
 integer      :: ID,ik,io_err
 integer, external :: io_SC_components
 !
 if (l_sc_run) using_SC_basis=.false.
 if (l_sc_run) return
 ! 
 ! Energies
 !----------
 !
 ! keep backup of bare energies to be used in
 ! optical oscillators
 !
 if (what/="WF") then
   call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
 else
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=MODE_,ID=ID)
 endif
 !
 select case (what)
   !
   case("E")
     allocate(E_of_WFo(E%nb,E%nk,n_sp_pol))
     call mem_est("E_of_WFo",(/size(E_of_WFo)/),(/SP/))
     E_of_WFo=E%E
     io_err=io_SC_components('E',E,ID)
     if (io_err/=0) then
       deallocate(E_of_WFo)
       call mem_est("E_of_WFo")
     endif
     !
   case("V")
     io_err=io_SC_components('V',E_dummy,ID)
     !
   case("WF")
     io_err=io_SC_components('WF',E_dummy,ID)
     if (io_err==0) then
       allocate(SC_R(SC_bands,SC_bands,nkibz))
       call mem_est("SC_R",(/size(SC_R)/))
       do ik=1,nkibz
         call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID)
         io_err=io_SC_components('WF',E_dummy,ID)
       enddo
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
 using_SC_basis=io_err==0
 !
end subroutine
