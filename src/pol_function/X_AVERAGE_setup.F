!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine X_AVERAGE_setup(what,i_counter,q0)
 !
 use pars,          ONLY:SP,rZERO
 use vec_operate,   ONLY:v_norm
 use R_lattice,     ONLY:q0_def_norm
 use parser_m,      ONLY:parser
 use X_m,           ONLY:OPTICAL_do_average,n_OPTICAL_dir_to_eval,OPTICAL_averaged_dirs_string
 !
 implicit none
 !
 character(*)           :: what
 integer                :: i_counter
 real(SP),intent(inout) :: q0(3)
 !
 ! Work Space
 !
 logical :: is_there_an_average_to_do
 integer :: ic
 real(SP), SAVE :: q0_save(3)
 !
 if (what=="INIT") then
   !
   OPTICAL_averaged_dirs_string="none"
   !
   call parser("OptDipAverage",is_there_an_average_to_do)
   !
   ! The following is not really needed since X_irredux does again the same operation
   ! However it is kept for back compatibility of old databases, since I've removed
   ! this operation from INIT.F
   !
   q0(:)=q0/v_norm(q0)*q0_def_norm
   q0_save=q0
   !
   if (.not.is_there_an_average_to_do) then
     n_OPTICAL_dir_to_eval=1
     return
   endif
   !
   n_OPTICAL_dir_to_eval=0
   OPTICAL_averaged_dirs_string=""
   do ic=1,3
     if (abs(q0(ic))>0.) then
       if (ic==1) OPTICAL_averaged_dirs_string=trim(OPTICAL_averaged_dirs_string)//'X'
       if (ic==2) OPTICAL_averaged_dirs_string=trim(OPTICAL_averaged_dirs_string)//'Y'
       if (ic==3) OPTICAL_averaged_dirs_string=trim(OPTICAL_averaged_dirs_string)//'Z'
       n_OPTICAL_dir_to_eval=n_OPTICAL_dir_to_eval+1
       OPTICAL_do_average(n_OPTICAL_dir_to_eval)=ic
     endif
   enddo
   !
   return
   !
 endif
 !
 if (what=="Q-LOOP") then
   n_OPTICAL_dir_to_eval=1
   if (i_counter>1) return
   n_OPTICAL_dir_to_eval=max(count(OPTICAL_do_average/=0),1)
   return
 endif
 !
 if (what=="DIR-LOOP") then
   q0=rZERO
   q0( OPTICAL_do_average(i_counter) )=1._SP
   return
 endif
 !
 if (what=="CLOSE") then
   q0=q0_save
   return
 endif
 !
end subroutine X_AVERAGE_setup
