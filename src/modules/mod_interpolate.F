!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
module interpolate
 !
 use pars,       ONLY:SP,schlen
 !
 ! Interpolation according to PRB 38 p2721
 ! Code copied from BolzTraP 
 !
 implicit none
 !
 integer               :: nwave
 real(SP), allocatable :: int_sop(:,:,:)
 real(SP), pointer     :: lattice_vectors(:,:)
 real(SP), parameter   :: lpfac = 5.0_SP  ! Use 5 times more waves than nkibz
 real(SP), allocatable :: engre(:,:)
 ! 
 type interp_coeff
   character(schlen)     :: interp_name
   real(SP), allocatable :: engre(:,:)
 end type interp_coeff
 !
 interface
   !
   subroutine eval_interpolation_coeff(RVALUES,CVALUES,k)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     implicit none
     type(bz_samp), intent(in) :: k
     real(SP),      optional, intent(in) :: RVALUES(:,:)
     complex(SP),   optional, intent(in) :: CVALUES(:,:)
     !
   end subroutine eval_interpolation_coeff
   !
 end interface
 !
 contains
   !
   SUBROUTINE STERN1(G,nsym,symop,nst,stg)
     !.... GENERATE STAR OF G
     ! note: this is the only place symop is used
     use pars,           ONLY:SP
     USE vec_operate,    ONLY:v_is_zero
     IMPLICIT NONE
     !
     INTEGER,INTENT(IN)     :: nsym
     REAL(SP),INTENT(IN)    :: g(3),symop(3,3,nsym) ! input vector and symmetry operations
     REAL(SP),INTENT(OUT)   :: stg(3,nsym)          ! star vectors - maximum number is nsym
     INTEGER,INTENT(OUT)    :: nst                  ! number of vectors in star
     !
     INTEGER          :: I,M
     REAL(SP)          :: try(1:3)
     LOGICAL          :: add
     !
     nst=1
     stg(1:3,1)=g(1:3)
     !
     DO I=1,nsym
       try(1:3)=MATMUL(symop(1:3,1:3,i),G(1:3))
       add=.TRUE.
       DO m=1,nst
! could use a hash table to do this with integer compares instead of math operations
         if(v_is_zero(try(:)-stg(:,m))) add=.FALSE.
       ENDDO
     IF(add) THEN
       nst=nst+1                                                      
       stg(1:3,nst)=try(1:3)
     ENDIF
   ENDDO
 END SUBROUTINE STERN1
 !
end module interpolate
