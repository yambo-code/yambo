! 
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, CA, AM
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
subroutine k_circuit_made_of_user_points( )
 !
 use pars,           ONLY:SP,schlen
 use com,            ONLY:msg
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use YPPm,           ONLY:BANDS_steps,USER_k,CIRCUIT_k,coo_in,coo_out,K_transform
 !
#include<memory.h>
 !
 ! Work space
 !
 character(schlen)     :: coo_out_
 real(SP)              :: max_dist,v(3),real_ctl
 real(SP), allocatable :: distances(:)
 integer               :: ik,ic,i1
 integer,  allocatable :: int_distances(:)
 !
 ! Read the kpts from input (extrema from the path)
 ! 
 USER_k%nbz=1
 !
 kgrid_main_loop: do while(real_ctl/=999._SP)
   YAMBO_FREE(USER_k%pt)
   YAMBO_ALLOC(USER_k%pt,(USER_k%nbz,3))
   USER_k%pt(USER_k%nbz,:)=(/0._SP,0._SP,999._SP/)
   call parser('BANDS_kpts',USER_k%pt)
   real_ctl=USER_k%pt(USER_k%nbz,3)
   if (real_ctl/=999._SP) USER_k%nbz=USER_k%nbz+1
 enddo kgrid_main_loop
 !
 USER_k%nbz =USER_k%nbz-1
 USER_k%nibz=USER_k%nbz
 !
 if (USER_k%nbz==0) return
 !
 coo_out_=coo_out
 coo_out ="iku"
 do ik=1,USER_k%nbz
   call K_transform(USER_k%pt(ik,:),coo_in)
 enddo
 coo_out =coo_out_
 !
 ! Generate the circuit
 !======================
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
   int_distances(ik)=maxval((/nint(BANDS_steps*distances(ik)/max_dist),1/))
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
end subroutine k_circuit_made_of_user_points
