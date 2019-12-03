! 
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS, AM
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
subroutine generate_circuit(USER_k,CIRCUIT_k,n_steps)
 !
 use pars,           ONLY:SP,schlen
 use R_lattice,      ONLY:bz_samp,bz_samp_reset,k_the_nearest
 use com,            ONLY:msg,error,warning
 use vec_operate,    ONLY:c2a,v_norm
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)  :: USER_k
 integer      , intent(in)  :: n_steps
 type(bz_samp), intent(out) :: CIRCUIT_k
 !
 ! Work space
 !
 real(SP)              :: max_dist,v(3)
 real(SP), allocatable :: distances(:)
 integer               :: ik,ic,i1
 integer,  allocatable :: int_distances(:)
 !
 ! Generate a circuit
 !====================
 !
 allocate(distances(USER_k%nbz-1),int_distances(USER_k%nbz-1))
 !
 do ik=1,USER_k%nbz-1
    v=USER_k%pt(ik,:)-USER_k%pt(ik+1,:)
    call c2a(v_in=v,mode="ki2c")
    distances(ik)=v_norm(v)
 enddo
 !
 max_dist=maxval(distances)
 !
 do ik=1,USER_k%nbz-1
   int_distances(ik)=maxval((/nint(n_steps*distances(ik)/max_dist),1/))
 enddo
 !
 CIRCUIT_k%nbz =sum(int_distances)+1
 CIRCUIT_k%nibz=CIRCUIT_k%nbz
 !
 call msg("s",'Number of Q/K-points in the circuit :',CIRCUIT_k%nbz)
 !
 YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
 YAMBO_ALLOC(CIRCUIT_k%weights,(CIRCUIT_k%nbz))
 !
 CIRCUIT_k%weights=1._SP/real(CIRCUIT_k%nbz,SP)
 !
 ic=1
 do ik=1,USER_k%nbz-1
   v=(USER_k%pt(ik+1,:)-USER_k%pt(ik,:))/int_distances(ik)
   do i1=1,int_distances(ik)
     CIRCUIT_k%pt(ic,:)= USER_k%pt(ik,:)+(i1-1._SP)*v
     ic=ic+1
   enddo
 enddo
 CIRCUIT_k%pt(CIRCUIT_k%nbz,:)=USER_k%pt(USER_k%nbz,:)
 !
 deallocate(distances,int_distances)
 !
end subroutine generate_circuit
