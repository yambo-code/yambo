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
subroutine Eval_Energy(en,k,Ko,Vh,Trace_Gf)
 !
 use pars,           ONLY:SP,DP,schlen
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc
 use real_time,      ONLY:G_lesser,RT_step,Ho,rho
 use electrons,      ONLY:levels
 use wave_func,      ONLY:wf
 implicit none 
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(out):: Ko,Vh,Trace_Gf
 !
 ! Work Space
 !
 integer       :: ik,i1
 complex(DP)   :: zdotu
 complex(SP)   :: cdotu
 complex(SP)   :: TmpMatrix(SC_bands,SC_bands)
 !
 TmpMatrix=(0._SP,0._SP)
 !
 ! Ko is the non-interaction kinetc
 !
 ! Ko = - i Tr {Ho G^<}  
 !
 Ko=(0._SP,0._SP)
 !
 do ik=1,en%nk
   !      
#if defined _DOUBLE
   Ko=Ko-(0._SP,1._SP)*zdotu(SC_bands*SC_bands,G_lesser(:,:,ik),1,Ho(:,:,ik),1)*k%weights(ik)
#else
   Ko=Ko-(0._SP,1._SP)*cdotu(SC_bands*SC_bands,G_lesser(:,:,ik),1,Ho(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !
 ! Vh is the mean value of the Hartree Potential
 !
 Vh=(0._SP,0._SP)
 !
 do ik=1,en%nk
   !       
   call V_to_H(ik,V_hartree_sc,TmpMatrix,wf)
   !
#if defined _DOUBLE
   Vh=Vh-(0._SP,1._SP)*zdotu(SC_bands*SC_bands,G_lesser(:,:,ik),1,TmpMatrix,1)*k%weights(ik)
#else
   Vh=Vh-(0._SP,1._SP)*cdotu(SC_bands*SC_bands,G_lesser(:,:,ik),1,TmpMatrix,1)*k%weights(ik)
#endif
   !
 enddo
 !
 Trace_Gf=(0._SP,0._SP)
 !
 do i1=1,SC_bands
   Trace_Gf=Trace_Gf-(0._SP,1._SP)*sum(G_lesser(i1,i1,:)*k%weights(:))
 enddo
 !
end subroutine Eval_Energy

