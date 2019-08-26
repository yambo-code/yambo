!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
subroutine SLK_setup( idriver, ncpu_in_use, ORTHO, POOL, COMM, INDEX_comm, slk_error)
 !
 use SLK_m,          ONLY:n_pools,ORTHO_group,POOL_group,SLK_POOL_init
 use parallel_m,     ONLY:MPI_comm
 use linear_algebra, ONLY:INV,DIAGO,LIN_SYS,MAT_MUL
 use com,            ONLY:msg
 !
 IMPLICIT NONE
 !
 integer,           intent(in)    :: idriver
 integer,           intent(in)    :: ncpu_in_use
 type(MPI_comm),    intent(in)    :: COMM , INDEX_comm
 type(ORTHO_group), intent(inout) :: ORTHO 
 type(POOL_group) , intent(inout) :: POOL 
 integer,           intent(inout) :: slk_error 
 !
 ! Work Space
 !
 character(20) :: ch
 !
 slk_error=0
 !
 ! # of POOLS
 !
 n_pools = INDEX_comm%n_CPU
 !
 ! Init pools
 !
 call SLK_POOL_init(POOL,COMM)
 !
 ! Init the scalapack grid
 !
 call SLK_ORTHO_init( ORTHO, POOL, min(POOL%n_CPU,ncpu_in_use) )
 ! 
 if (ORTHO%grid(1)<=1) then 
   slk_error=-1
   return
 endif
 !
 if (idriver==INV)     ch="Inversion"
 if (idriver==DIAGO)   ch="Digonalization"
 if (idriver==LIN_SYS) ch="LinearSystem"
 if (idriver==MAT_MUL) ch="MatrixMult."
 !
 call msg("lr" ,'[SLK-'//trim(ch)//']  Pools        :',n_pools)
 call msg("lrn",'[SLK-'//trim(ch)//']  BLACS grid   :',ORTHO%grid)
 !
end subroutine SLK_setup
