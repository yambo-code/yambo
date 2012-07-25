!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine Nearest_level_interpolation(Energy,E_reference,Nb,Nk,ib_ref,ik_ref,SINGLE_ARRAY)
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:sort
 !
 implicit none
 !
 real(SP),           intent(in) :: Energy,E_reference(Nb,Nk)
 integer,            intent(in) :: Nb,Nk
 logical,            intent(in) :: SINGLE_ARRAY
 integer,            intent(out):: ib_ref,ik_ref
 !
 ! Work Space
 !
 integer  :: E_indx(Nb*Nk),ik,ib,i1,E_table(Nk*Nb,2)
 real(SP) :: E_single_array(Nb*Nk),E_diff,E_diff_tresh
 !
 i1=0
 do ik=1,Nk
   do ib=1,Nb
     i1=i1+1
     E_single_array(i1)=E_reference(ib,ik)
     E_table(i1,:)= (/ib,ik/)
   enddo
 enddo
 !
 call sort(arrin=E_single_array,indx=E_indx)
 !
 E_diff_tresh=1000.
 !
 do i1=1,Nk*Nb
   !
   E_diff=abs( Energy-E_single_array(i1)+E_single_array(1) )
   !
   if (E_diff<E_diff_tresh) then
     !
     if (SINGLE_ARRAY) then
       ib_ref= i1
       ik_ref= i1
     else
       ib_ref=E_table( E_indx(i1), 1)
       ik_ref=E_table( E_indx(i1), 2)
     endif
     !
     E_diff_tresh=E_diff
     !
   endif
   !
 enddo
 !
 !write (*,*) Energy,E_single_array(ib_ref)+E_single_array(1)
 !
end subroutine Nearest_level_interpolation
