!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
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
subroutine RT_driver(en,k)
 !
 use YPP,            ONLY:l_fix_syms,l_RT_optics,Use_FFT,l_no_remove_DC,&
&                         RT_P_t,RT_J_t,current_ext_t,A_vecpot_vel_t
 use R_lattice,      ONLY:bz_samp
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 call section("*",'RT Post/Pre processing')
 !
 if (l_fix_syms) then
   !
   ! SYMMERIES removal
   !-------------------
   !
   call RT_fix_symmetries(en,k)
   return
 endif
 !
 ! DATABASES
 !-----------
 !
 call RT_databases(en)
 !
 if (.not.allocated(RT_P_t)) return
 !
 if (l_RT_optics) then
   !
   ! CORE Logicals
   !---------------
   ! 
   call parser('UseFFT',Use_FFT)
   call parser('NoRmDC',l_no_remove_DC)
   !
   ! OPTICS
   !--------
   !
   call RT_optics(en,k)
   !
 endif
 !
 ! CLEAN
 !
 deallocate(RT_P_t,RT_J_t,current_ext_t,A_vecpot_vel_t)
 call mem_est("RT_P_t RT_J_t Current_ext_t A_vecpot_vel_t")
 !
end subroutine
