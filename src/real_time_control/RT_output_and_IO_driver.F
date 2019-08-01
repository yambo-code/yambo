!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_output_and_IO_driver(what,E,k,OBSERVABLES)
 !
 use pars,           ONLY:SP
 use RT_control,     ONLY:RT_output
 use units,          ONLY:AUT2FS
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:set_real_printed_length
 use IO_m,           ONLY:IO_and_Messaging_switch
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use real_time,      ONLY:eval_energy,eval_entropy,l_RT_update_Energies,l_NE_with_fields,NE_time, &
&                         l_RT_induced_field,l_RT_impose_N_dN,l_RT_impose_E,RT_is_dynamically_dephased,NE_i_time
 use RT_control,     ONLY:RT_do_it
 !
 implicit none
 !
 character(*)           :: what
 type(levels)           :: E
 type(bz_samp)          :: k
 real(SP), intent(inout):: OBSERVABLES(25)
 !
 ! Workspace
 !
 real(SP) :: NCarr(3),TFit(2),EfFit(2),Ener(5),Entr(3),TEntr(3),EnEntr(3),BGR(4)
 !
 select case(trim(what))
   !
 case('open')
   !
   OBSERVABLES=0._SP
   !
   call RT_output(what="open curr pol")
   !
   !
 case('write')
   !
   !
   call set_real_printed_length()
   call IO_and_Messaging_switch("-fragments_IO_log")
   call RT_databases_IO(E)
   call IO_and_Messaging_switch("+fragments_IO_log")
   call set_real_printed_length(f_length=10,g_length=10)
   !
   ! OUTPUT(s)
   !###########
   !
   if (.not.RT_do_it('OUT')) return
   !
   call RT_output('curr pol',TIME=NE_time*AUT2FS)
   !
   !
   ! Field(s)
   !==========
   if (l_NE_with_fields) then
     call RT_output('external_field', TIME=NE_time*AUT2FS)
     if(l_RT_induced_field) then
       call RT_output('total_field'  ,TIME=NE_time*AUT2FS)
       call RT_output('induced_field',TIME=NE_time*AUT2FS)
     endif
   endif
   !
 end select
 !
end subroutine RT_output_and_IO_driver
