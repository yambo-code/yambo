!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine io_RT_components_header(ID,what,loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
 !
 use pars,           ONLY:IP,LP,SP,schlen
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:io_header,io_status,frag_RT,read_is_on,&
&                         def_variable_elemental,io_variable_elemental,&
&                         def_variable_bulk,io_variable_bulk
 use real_time,      ONLY:q_weight_RT,RT_carriers,RT_obj_alloc
 use RT_control,     ONLY:SAVE_G_history
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer, intent(in)      :: ID
 logical, intent(in)      :: loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF
 integer, intent(out)     :: io_RT_components
 !
 integer, external     :: variables_RT_static
 !
 character(schlen)     :: what_
 !
 if ( .not. loading_carr_before_neq ) then
   !
   what_=what
   if (l_OBS) then
     what_='OBSERVABLES'
   else if (.not.any((/l_carriers,l_THETA,l_G,l_REF/))) then
     what_='carriers'
   endif
   !
   io_RT_components=io_header(ID,IMPOSE_SN=(.not.l_carriers),CUTOFF=.true.,T_EL=.true.,FRAG=frag_RT)
   if (io_RT_components/=0) return
   !
   io_RT_components=variables_RT_static(ID,trim(what_))
   if (io_RT_components/=0) return
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 if (l_THETA) then
   call def_variable_bulk(ID,"Q_weight",3,(/nqbz/),SP)
   call  io_variable_bulk(ID,3,R1=q_weight_RT(:,1,1))
 endif
 !
 ! Here I dump all RELEVANT RT_carriers% components
 ! that make the RT_carriers databases independent
 !
 if (l_carriers) then
   call def_variable_elemental(ID,"RT_carriers_dimensions",4,IP+100,0) 
   call  io_variable_elemental(ID,I0=RT_carriers%nk)
   call  io_variable_elemental(ID,I1=RT_carriers%nb)
   call  io_variable_elemental(ID,I0=RT_carriers%nstates)
   !call  io_variable_elemental(ID,"",0,0)
   !
   if (read_is_on(ID)) call RT_obj_alloc(RT_carriers)
   !
   call def_variable_bulk(ID,"RT_kpt",4,(/RT_carriers%nk,3/),SP)
   call  io_variable_bulk(ID,4,R2=RT_carriers%kpt)
   call def_variable_bulk(ID,"RT_k_weight",5,(/RT_carriers%nk/),SP)
   call  io_variable_bulk(ID,5,R1=RT_carriers%k_weight)
   call def_variable_bulk(ID,"RT_table",6,(/RT_carriers%nstates,4/),IP+100)
   call  io_variable_bulk(ID,6,I2=RT_carriers%table)
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_G) then
   call def_variable_elemental(ID,"Save_G_history",1,LP+100,0) 
   call  io_variable_elemental(ID,L0=SAVE_G_history)
   !call io_variable_elemental(ID,"",0,0)
 endif
 !
 io_RT_components=io_status(ID)
 !
end subroutine io_RT_components_header
