!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Collision(ik,it,en)
 !
 use pars,           ONLY:SP,schlen
 use SC,             ONLY:SC_potential,SC_bands
 use real_time,      ONLY:G_lesser,Gf_diagonal,Sigma_s,I1_lesser,I2_lesser, &
&                         Thermal_steps 
 use electrons,      ONLY:levels
 implicit none 
 type(levels), intent(in) :: en
 integer,      intent(in) :: ik,it
 !
 !
 I1_lesser(:,:)=(0._SP,0._SP)
 I2_lesser(:,:)=(0._SP,0._SP)
 !
#if defined _DOUBLE
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s,&
&             SC_bands,G_lesser(:,:,ik),SC_bands,(1._SP,0._SP),I1_lesser(:,:),SC_bands)
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_lesser(:,:,ik),&
&             SC_bands,Sigma_s,SC_bands,(1._SP,0._SP),I2_lesser(:,:),SC_bands)
#else
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s,&
&             SC_bands,G_lesser(:,:,ik),SC_bands,(1._SP,0._SP),I1_lesser(:,:),SC_bands)
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_lesser(:,:,ik),&
&             SC_bands,Sigma_s,SC_bands,(1._SP,0._SP),I2_lesser(:,:),SC_bands)
#endif
end subroutine RT_Collision

