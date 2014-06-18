!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): PM
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
subroutine QED_polarization_vectors(k,pol_sum)
!
 use pars,                ONLY:SP
 !
 !I/O
 !
 implicit none
 real(SP), intent(in)     :: k(3)                        !photon momentum
 real(SP), intent(out)    :: pol_sum(3,3)                !sum over the polarisation indeces of e*_(ic,i_pol,q)e_(jc,i_pol_q)
 !integer, intent(in)     :: pol_type                    !1 for circular polarisation, 2 for linear
 !Workspace
 !
 integer :: i, j
 !complex(SP)             :: e_r(:), e_l(:)              
 real(SP)                 :: e_1(3), e_2(3)
 real(SP)                 :: cos_a, sin_a, cos_t, sin_t  !rotation parameters
 !
 !The option to choose the polarisation type (linear or circular is commented, for now)
 !pol_type will be user defined
 !
 !if(pol_type /= 1 .and. pol_type /= 2) then  
 !  print*, 'Polarisation type not existant. Must be either linear or circular polarisation.'
 !else
 if (k(1)==0.0 .and. k(2)==0.0 .and. k(3) == 0.0) then
   cos_a = 1.0
   sin_a = 0.0
   cos_t = 1.0
   sin_t = 0.0
 else if(k(1)==0.0 .and. k(2)==0.0) then
   cos_a = 1.0
   sin_a = 0.0
   cos_t = k(3)/sqrt(k(1)**2 + k(2)**2 + k(3)**2) 
   sin_t = sqrt(k(1)**2 + k(2)**2)/sqrt(k(1)**2 + k(2)**2 + k(3)**2)
 else
   cos_a = k(1)/sqrt(k(1)**2 + k(2)**2)
   sin_a = k(2)/sqrt(k(1)**2 + k(2)**2)
   cos_t = k(3)/sqrt(k(1)**2 + k(2)**2 + k(3)**2) 
   sin_t = sqrt(k(1)**2 + k(2)**2)/sqrt(k(1)**2 + k(2)**2 + k(3)**2)
 end if
 !
 !Will work only with linear polarisation for now
 ! 
 !if(pol_type == 1) then
 !   ALLOCATE(e_r(3), e_l(3), A(2,3,3))
 ! 
 !   e_r(1) = dcmplx(cos_a*cos_t,sin_a)
 !   e_r(2) = dcmplx(sin_a*cos_t,-cos_a)
 !   e_r(3) = dcmplx(-sin_t,0)
 !
 !   e_l(1) = dcmplx(cos_a*cos_t,-sin_a)
 !   e_l(2) = dcmplx(sin_a*cos_t,cos_a)
 !   e_l(3) = dcmplx(-sin_t)
 !
 !   e_r = e_r/sqrt(2.0)
 !   e_l = e_l/sqrt(2.0)
 !   
 !   do i = 1, 3
 !     do j = i, 3
 !       A(1,i,j) = CONJG(e_l(i))*e_l(j)
 !       A(1,j,i) = CONJG(A(1,i,j))
 !       A(2,i,j) = A(1,j,i)
 !       A(2,j,i) = A(1,i,j)
 !     end do
 !   end do
 !        
 !   DEALLOCATE(e_r, e_l, A)
 !
 ! else if(pol_type == 2) then
 !  ALLOCATE(e_1(3), e_2(3), B(2,3,3))
 !
 e_1(1) = cos_a*cos_t
 e_1(2) = sin_a*cos_t
 e_1(3) = -sin_t
 !
 e_2(1) = -sin_a
 e_2(2) = cos_a
 e_2(3) = 0.0
 ! 
 pol_sum = 0.
 !
 do i = 1, 3
  do j = i, 3
   pol_sum(i,j) = e_1(i)*e_1(j) + e_2(i)*e_2(j)
   pol_sum(j,i) = pol_sum(i,j)
  end do
 end do
 !
 ! end if
 !end if
 !
end subroutine QED_polarization_vectors
