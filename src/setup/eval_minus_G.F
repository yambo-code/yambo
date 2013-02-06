!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine eval_minus_G()
 !
 ! Output: minus_G(ng_closed)
 !
 ! G_{minus_G(ig)} = -G_ig
 !
 use pars,         ONLY:IP
 use memory_m,     ONLY:mem_est
 use D_lattice,    ONLY:inv_index
 use vec_operate,  ONLY:v_is_zero
 use R_lattice,    ONLY:ng_closed,g_vec,g_rot,minus_G
 use zeros,        ONLY:G_iku_zero
 use com,          ONLY:msg
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: ig,igp,ng1,ng2
 logical  :: found(ng_closed)
 !
 if (allocated(minus_G)) then
   deallocate(minus_G)
   call mem_est("minus_G")
 endif
 allocate(minus_G(ng_closed))
 call mem_est("minus_G",(/ng_closed/),(/IP/))
 !
 minus_G=0
 !
 if (inv_index>0) then
   do ig=1,ng_closed
     minus_G(ig)=g_rot(inv_index,ig)
   enddo
 else
   !
   minus_G(1)=1
   !
   found=.false. 
   if (ng_closed>1) call msg('s','Inversion map for the R lattice')
   !
   do ig=2,ng_closed
      if (found(ig)) cycle  
     !
      do igp=ig+1,ng_closed
       !
         if  (found(igp)) cycle 
         if (v_is_zero(g_vec(ig,:)+g_vec(igp,:),zero_=G_iku_zero)) then
            minus_G(ig)=igp
            minus_G(igp)=ig
            found(igp)=.true.
            exit
         endif
       !
     enddo
     !
   enddo
   !
 endif
 !
 !
end subroutine
