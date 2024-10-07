!
!        Copyright (C) 2000-2017 the LUMEN team
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_test_collisions(k,q) 
 !
 ! Test-collisions built on the fly from V/W 
 !                   VS
 ! collisions stored on disk (yambo_nl -e -v sex/f)
 !
 use pars,           ONLY:cZERO,SP,rZERO
 use R_lattice,      ONLY:nXkibz,bz_samp,qindx_S
 use collision_ext,  ONLY:HXC_COLL_element,HXC_collisions
 use nl_optics,      ONLY:NL_bands
 use com,            ONLY:msg
 !
 implicit none
 ! 
 type(bz_samp), intent(in)  :: k,q
 !
 integer :: i_kmq,i_qp,ib,ibp,i_coll,i_coll_mem
 integer :: i_k,i_n,i_m,i_spin
 integer :: ic1
 complex(SP) :: COLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nXkibz)
 complex(SP) :: COLL_OSCLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nXkibz)
 ! 
 real(SP) :: max_diff,diff
 !
 call msg('s','Collisions Test COLL vs OSCLL')
 call msg('s','')
 !
 max_diff=rZERO
 !
 do i_coll=1,HXC_collisions%N
   !
   i_k       =HXC_collisions%state(i_coll,3)
   i_n       =HXC_collisions%state(i_coll,1)
   i_m       =HXC_collisions%state(i_coll,2)
   i_spin    =HXC_collisions%state(i_coll,4)
   i_coll_mem=HXC_collisions%PAR_map(i_coll)
   !
   ! Spin not supported for the moment
   !
   call OSCLL_compose_collision(k,q,i_n,i_m,i_k,COLL_OSCLL) 
   !
   ic1=1
   !
   COLL=cZERO
   !
   do i_kmq=1,nXkibz
     !
     do ibp=NL_bands(1),NL_bands(2)
       do ib=NL_bands(1),NL_bands(2)
         if (HXC_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq)=="t") then
           COLL(ib,ibp,i_kmq)=HXC_COLL_element(i_coll_mem)%v_c(ic1)
           ic1=ic1+1
         endif
         !
         diff=abs(COLL(ib,ibp,i_kmq)-COLL_OSCLL(ib,ibp,i_kmq))
         write(*,'(6i5,2e14.6)') i_k,i_m,i_n,i_kmq,ib,ibp,abs(COLL(ib,ibp,i_kmq)),abs(COLL_OSCLL(ib,ibp,i_kmq))
         if(max_diff<=diff) max_diff=diff
         !
       enddo
     enddo
     !
   enddo
   !
 enddo 
 !
 call msg('s','')
 call msg('s','Max error in collisions : ',max_diff)
 call msg('s','')
 !
end subroutine NL_test_collisions
