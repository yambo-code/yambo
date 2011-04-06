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
 use YPP,            ONLY:l_fix_syms,l_RT_X_one,Use_FFT,RT_P_t,RT_J_t,l_RT_X_two,&
&                         l_effective_X,N_probes,N_pumps,RT_init_X_one,RT_databases
 use R_lattice,      ONLY:bz_samp
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use com,            ONLY:error
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 logical       :: IO_error
 !
 if (l_fix_syms) then
   !
   call section("*",'RT Pre Processing')
   !
   ! SYMMERIES removal
   !-------------------
   !
   call RT_fix_symmetries(en,k)
   return
 endif
 !
 call section("*",'RT Post Processing')
 !
 call RT_paths(en)
 !
 !
 if (N_pumps==0.and.N_probes==1) then
   !
   ! Database
   !
   call RT_databases(en)
   !
   !  Initialization 
   !
   call RT_init_X_one(dump_and_write=.TRUE.)
   !
   if (l_RT_X_one) then
     !
     ! CORE Logicals
     !---------------
     ! 
     call parser('UseFFT',Use_FFT)
     !
     ! OPTICS
     !--------
     !
     call RT_X_one(en)
     !
   elseif (l_RT_X_two) then
     !
     call RT_X_two(en)
     !
   endif
   !
 else if ( (N_pumps==1.and.N_probes>=1).or.(N_pumps==0.and.N_probes>=1)) then
   !
   call RT_X_effective(en)
   !
 endif
 !
 ! CLEAN
 !
 if (allocated(RT_P_t)) deallocate(RT_P_t)
 if (allocated(RT_J_t)) deallocate(RT_J_t)
 call mem_est("RT_P_t RT_J_t")
 !
end subroutine
