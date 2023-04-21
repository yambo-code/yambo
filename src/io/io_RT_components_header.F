!
!        Copyright (C) 2000-2022 the YAMBO team
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
#if defined _RT
subroutine io_RT_components_header(ID,what,loading_carr_before_neq,io_RT_components)
 !
 use pars,           ONLY:IP,SP,LP,IP_YIO,LP_YIO
 use R_lattice,      ONLY:nqbz
 use IO_int,         ONLY:io_header,def_variable_elemental,io_variable_elemental,&
&                         def_variable_bulk,io_variable_bulk
 use IO_m,           ONLY:io_status,frag_RT,read_is_on
 use RT_lifetimes,   ONLY:q_weight_RT
 use real_time,      ONLY:RT_carriers,RT_obj_alloc
 use RT_control,     ONLY:RT_do_IO,RT_current_DB_identifier,RT_DB_name,SAVE_G_history
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer, intent(in)      :: ID
 logical, intent(in)      :: loading_carr_before_neq
 integer, intent(out)     :: io_RT_components
 !
 integer, external     :: variables_RT_static
 !
 if ( .not. loading_carr_before_neq ) then
   !
   io_RT_components=io_header(ID,IMPOSE_SN=(.not.RT_do_IO("carriers")),CUTOFF=.true.,TEMP=.true.,FRAG=frag_RT)
   if (io_RT_components/=0) return
   !
   io_RT_components=variables_RT_static(ID,trim(RT_DB_name(RT_current_DB_identifier)),RT_do_IO("carriers"))
   if (io_RT_components/=0) return
   !
 endif
 !
 if (RT_do_IO("THETA")) then
   call def_variable_bulk(ID,"Q_weight",3,(/nqbz/),SP)
   call  io_variable_bulk(ID,3,R1=q_weight_RT(:,1,1))
 endif
 !
 ! Here I dump all RELEVANT RT_carriers% components
 ! that make the RT_carriers databases independent
 !
 if (RT_do_IO("carriers")) then
   call def_variable_elemental(ID,"RT_carriers_dimensions",4,IP_YIO,0) 
   call  io_variable_elemental(ID,I0=RT_carriers%nk)
   call  io_variable_elemental(ID,I1=RT_carriers%nb)
   call  io_variable_elemental(ID,I0=RT_carriers%nstates)
   !
   if (read_is_on(ID)) call RT_obj_alloc(RT_carriers)
   !
   call def_variable_bulk(ID,"RT_kpt",4,(/RT_carriers%nk,3/),SP)
   call  io_variable_bulk(ID,4,R2=RT_carriers%kpt)
   call def_variable_bulk(ID,"RT_k_weight",5,(/RT_carriers%nk/),SP)
   call  io_variable_bulk(ID,5,R1=RT_carriers%k_weight)
   call def_variable_bulk(ID,"RT_table",6,(/RT_carriers%nstates,4/),IP_YIO)
   call  io_variable_bulk(ID,6,I2=RT_carriers%table)
   !
 endif
 !
 if (RT_do_IO("ANY_G")) then
   call def_variable_elemental(ID,"Save_G_history",1,LP_YIO,0) 
   call  io_variable_elemental(ID,L0=SAVE_G_history)
 endif
 !
 io_RT_components=io_status(ID)
 !
end subroutine io_RT_components_header
#endif
