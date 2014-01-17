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
subroutine RT_impose_N(k)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:spin_occ,nel
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_Nk_ibz,RT_ibz_coo
 use real_time,      ONLY:MEM_index,G_MEM_steps,G_lesser,RIM_ibz_co
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer          :: ib,ik,ik_rim,i_cycle
 real(SP)         :: f_occ,k_weight,No,dN,alpha
 !
 No = real(SC_bands(1))
 dN = 0._SP
 alpha=1.
 !
 do i_cycle=1,2
   do ik=1,k%nibz
     k_weight=k%weights(ik)/real(RT_Nk_ibz(ik))
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       do ib=SC_bands(1),SC_bands(2)
         f_occ=-cI*G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))
         if (abs(f_occ-spin_occ)>1.E-5) then
           if (i_cycle==2) then
             f_occ=f_occ*alpha
             G_lesser(ib,ib,ik_rim,MEM_index(it_now,G_MEM_steps))=cI*f_occ
           endif
           dN=dN+k_weight*f_occ
         else
           No=No+k_weight*f_occ
         endif
       enddo
     enddo
   enddo
   if (i_cycle==1.and.dN>1.E-5) then
     alpha=(nel-No)/dN
   endif
 enddo
 !
end subroutine RT_impose_N
