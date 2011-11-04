!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
subroutine RT_generalized_KBA(E,Gkkp,L,ik,ikp,L_kind,i_t_range,MEM_steps)
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
 use real_time,      ONLY:G_lesser,NE_MEM_steps,MEM_index
 use wrapper,        ONLY:M_by_M
 implicit none
 !
 type(levels),intent(in):: E
 complex(SP)   :: Gkkp(SC_bands(2),SC_bands(2))
 complex(SP)   :: L(SC_bands(2),SC_bands(2),NE_MEM_steps)
 integer       :: ik,ikp,MEM_steps,i_t_range(2)
 character(2)  :: L_kind
 !
 ! Work Space
 !
 integer               :: i_t,i_n,i_t_mem
 complex(SP)           :: G_g(SC_bands(2),SC_bands(2))
 complex(SP)           :: G_by_Gkkp(SC_bands(2),SC_bands(2))
 !
 do i_t=i_t_range(1),i_t_range(2)
   !
   i_t_mem=i_t-it_now+MEM_steps
   !
   if (L_kind=="><") then
     !
     !  i G_greater @ ik
     !
     G_g(:,:)=cI*G_lesser(:,:,ik,MEM_index(i_t)) 
     forall (i_n=1:SC_bands(2)) G_g(i_n,i_n)=G_g(i_n,i_n)+spin_occ
     !
     call M_by_M('n','n',SC_bands(2),G_g,Gkkp,G_by_Gkkp(:,:))
     call M_by_M('n','n',SC_bands(2),G_by_Gkkp(:,:),G_lesser(:,:,ikp,MEM_index(i_t)),L(:,:,i_t_mem))
     !
   else
     !
     !  i G_greater @ ikp
     !
     G_g(:,:)=cI*G_lesser(:,:,ikp,MEM_index(i_t)) 
     forall (i_n=1:SC_bands(2)) G_g(i_n,i_n)=G_g(i_n,i_n)+spin_occ
     !
     call M_by_M('n','n',SC_bands(2),G_lesser(:,:,ik,MEM_index(i_t)),Gkkp,G_by_Gkkp(:,:))
     call M_by_M('n','n',SC_bands(2),G_by_Gkkp(:,:),G_g(:,:),L(:,:,i_t_mem))
     !
   endif
   !
 enddo
 !
end subroutine RT_generalized_KBA
