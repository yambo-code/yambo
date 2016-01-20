!
! Copyright (C) 2000-2016 C. Attaccalite and the YAMBO team 
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
module WF_distribute
 !
 logical     :: Distributed_Memory
 !
 interface
   !
   subroutine WF_states_setup(k,Xw,b_to_load,k_to_load,qp_to_load,cg_npts,&
&                             bg_npts,q_to_load,qpoint,par_loop,report)
     !
     use R_lattice,       ONLY:bz_samp
     use frequency,       ONLY:w_samp
     !
     implicit none
     !
     type(bz_samp), optional       :: k
     type(w_samp),  optional       :: Xw
     logical, intent(in), optional :: par_loop,report
     integer, intent(in), optional :: k_to_load(2),b_to_load(2),qp_to_load(2)
     integer, intent(in), optional :: qpoint,cg_npts,bg_npts(*),q_to_load(2)
     !
   end subroutine 
   !
 end interface
 !
end module
