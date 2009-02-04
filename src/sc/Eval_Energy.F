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
subroutine Eval_Energy(en,k)
 !
 use pars,           ONLY:SP,schlen
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_potential,SC_bands
 use real_time,      ONLY:G_lesser,G_l_old,RT_step,Ekin,Ec,Ex
 use electrons,      ONLY:levels
 implicit none 
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer       :: i1
 !
 Ekin=(0.0,0.0) 
 !
 ! In principle it is a real quantity but to be sure that
 ! everything it is ok I calculate also the immaginary part
 !
 do i1=1,SC_bands
   Ekin=Ekin-(0.0,1.0)*sum(G_lesser(i1,i1,:)*en%E(i1,:,1)*k%weights(:))
 enddo
 !
 Ec=(0.0,0.0)
 !
 do i1=1,SC_bands 
   Ec=Ec+sum((G_lesser(i1,i1,:)-G_l_old(i1,i1,:))*k%weights(:))/RT_step
 enddo
 !
 Ec=0.5*(Ec-Ekin)  
 !
end subroutine Eval_Energy

