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
subroutine RT_generalized_KBA(E,L,ik,ikp,Time,L_kind)
 !
 use electrons,      ONLY:levels,spin_occ
 use pars,           ONLY:SP,cI
 use wrapper,        ONLY:M_by_M
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:G_lesser,NE_MEM_steps,RT_step,MEM_index
 implicit none
 !
 type(levels),intent(in):: E
 complex(SP)   :: L(SC_bands(2),SC_bands(2),NE_MEM_steps)
 integer       :: ik,ikp
 character(2)  :: L_kind
 real(SP)      :: Time
 !
 ! Work Space
 !
 integer               :: it,ib,ibp,MEM_steps,it_mem
 real(SP)              :: Time_difference
 complex(SP)           :: G_tmp(SC_bands(2),SC_bands(2)),G_by_G(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP), external :: RT_G_retarded
 !
 MEM_steps = NE_MEM_steps
 if (it_now-NE_MEM_steps+1<=0) MEM_steps=it_now
 !
 if (L_kind=="><") then
   !
   ! L^{><}_{kp,nm}(t,t')=-i G^r_nk(t-t')(spin_occ*delta_{nm}+i G^<_{n n' k}(t')) G^<_{n' m p}(t') [G^r_mp(t-t')]^*
   !
   ! t'=T-T_mem, T => t-t'=T_mem,0
   !
   do it=it_now-MEM_steps+1,it_now
     !
     it_mem=it-it_now+MEM_steps
     !
     ! G_greater@ik
     !
     G_tmp(:,:)=cI*G_lesser(:,:,ik,MEM_index(it)) 
     forall (ib=1:SC_bands(2)) G_tmp(ib,ib)=G_tmp(ib,ib)+spin_occ
     !
     ! G_greater@ik x G_lesser@ikp
     !
     call M_by_M('n','n',SC_bands(2),G_tmp,G_lesser(:,:,ikp,MEM_index(it)),G_by_G(:,:,it_mem))
     !
   enddo
 endif
 !
 if (L_kind=="<>") then
   !
   ! L^{<>}_{kp,nm}(t,t')=-i G^r_nk(t-t') G^<_{n n' k}(t') (spin_occ*delta_{n'm}+i G^<_{n' m p}(t')) [G^r_mp(t-t')]^*
   !
   do it=it_now-MEM_steps+1,it_now
     !
     it_mem=it-it_now+MEM_steps
     !
     ! G_greater@ikp
     !
     G_tmp(:,:)=cI*G_lesser(:,:,ikp,MEM_index(it)) 
     forall (ib=1:SC_bands(2)) G_tmp(ib,ib)=G_tmp(ib,ib)+spin_occ
     !
     ! G_lesser@ik x G_greater@ikp
     !
     call M_by_M('n','n',SC_bands(2),G_lesser(:,:,ik,MEM_index(it)),G_tmp,G_by_G(:,:,it_mem))
     !
   enddo
 endif
 !
 L(:,:,:)=(0.,0.)
 !
 do it=1,MEM_steps
   !
   ! t'=T-T_mem, T => t-t'=T_mem,0
   !
   Time_difference=(MEM_steps-it+1)*RT_step
   !
   ! L=-i Gr G_greater G_lesser (Gr)^*
   !
   do ib=1,SC_bands(2)
     do ibp=1,SC_bands(2)
       L(ib,ibp,it)=-cI*RT_G_retarded(E,ib,ik,Time_difference,0.)*G_by_G(ib,ibp,it)*&
&                 conjg(RT_G_retarded(E,ibp,ikp,Time_difference,0.))
     enddo
   enddo
   !
 enddo
 !
end subroutine RT_generalized_KBA
