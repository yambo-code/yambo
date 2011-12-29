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
subroutine RT_generalized_KBA(E,Gkkp,L,ik,ikp,L_kind)
 !
 ! L^{><}_{kp,nm}(t,t')=(spin_occ*delta_{nu}+i G^<_{n u k}(t')) Gkkp(u v) G^<_{v m p}(t')
 !
 !                      ----- G_left --------------------------           --- G_right ---
 !
 ! L^{<>}_{kp,nm}(t,t')=G^<_{n u k}(t') Gkkp(u,v) (spin_occ*delta_{vm}+i G^<_{v m p}(t'))
 !
 !                      ----- G_left ---           ----- G_right ------------------------
 !
 use electrons,      ONLY:levels,spin_occ
 use pars,           ONLY:SP,cI
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:G_lesser,MEM_index
 use wrapper,        ONLY:M_by_M
 implicit none
 !
 type(levels),intent(in):: E
 complex(SP)   :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)   :: L(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 integer       :: ik,ikp
 character(2)  :: L_kind
 !
 ! Work Space
 !
 integer               :: i_n
 complex(SP)           :: G_g(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_l(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_by_Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 if (L_kind=="><") then
   !
   !  G_greater @ ik
   !
   G_g(:,:)=cI*G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,MEM_index(it_now)) 
   forall (i_n=1:SC_bands(2)-SC_bands(1)+1) G_g(i_n,i_n)=G_g(i_n,i_n)+spin_occ
   !
   !  G_lesser @ ikp
   !
   G_l(:,:)=G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ikp,MEM_index(it_now)) 
   !
   call M_by_M('n','n',SC_bands(2)-SC_bands(1)+1,G_g,Gkkp,G_by_Gkkp(:,:))
   call M_by_M('n','n',SC_bands(2)-SC_bands(1)+1,G_by_Gkkp(:,:),G_l,L(:,:))
   !
 else
   !
   !  G_greater @ ikp
   !
   G_g(:,:)=cI*G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ikp,MEM_index(it_now)) 
   forall (i_n=1:SC_bands(2)-SC_bands(1)+1) G_g(i_n,i_n)=G_g(i_n,i_n)+spin_occ
   !
   !  G_lesser @ ik
   !
   G_l(:,:)=G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,MEM_index(it_now)) 
   !
   call M_by_M('n','n',SC_bands(2)-SC_bands(1)+1,G_l,Gkkp,G_by_Gkkp(:,:))
   call M_by_M('n','n',SC_bands(2)-SC_bands(1)+1,G_by_Gkkp(:,:),G_g(:,:),L(:,:))
   !
 endif
 !
end subroutine RT_generalized_KBA
