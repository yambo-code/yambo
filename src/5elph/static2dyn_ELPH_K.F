!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine static2dyn_ELPH_K(E,k,q,frequency,M)
 !
 ! This routine accumulates in M(:,:) the total Dynamic elph 
 ! contribution to the DBSE kernel given the residual part only.
 !
 ! Note that this version is compatible only with elph_use_q_grid=.TRUE.
 !
 use pars,          ONLY:SP
 use com,           ONLY:error
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use BS,            ONLY:BS_K_dim,BS_eh_table
 use ELPH,          ONLY:EXCPH_mat,elph_use_q_grid
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 complex(SP)   ::frequency,M(BS_K_dim,BS_K_dim)
 !
 ! Work Space ...
 !
 complex(SP)  ::E_diff(2)
 real(SP)     ::W_(2)
 integer      ::ii1,ii2,ic1,ic2,iv1,iv2,ik1_bz,ik2_bz,ik1_ibz,ik2_ibz
 integer, external::eval_ELPH_K
 !
 ! Residual only ELPH Kernel
 !
 if (eval_ELPH_K(E,k,q,.TRUE.,(0.,0.))<=0) return
 !
 if (.not.elph_use_q_grid) call error('Static 2 Dynamic transformation only using the Q grid')
 !
 do ii1=1,BS_K_dim
   ik1_bz =BS_eh_table(ii1,1)
   ik1_ibz=k%sstar(ik1_bz,1)
   iv1    =BS_eh_table(ii1,2)
   ic1    =BS_eh_table(ii1,3)
   do ii2=1,BS_K_dim
     ik2_bz =BS_eh_table(ii2,1)
     ik2_ibz=k%sstar(ik2_bz,1)
     iv2    =BS_eh_table(ii2,2)
     ic2    =BS_eh_table(ii2,3)
     !
     W_=0.
     if (associated(E%W)) W_=(/E%W(ic1,ik1_ibz,1)-E%W(iv2,ik2_ibz,1),&
&                              E%W(ic2,ik2_ibz,1)-E%W(iv1,ik1_ibz,1)/)
     !
     E_diff(1) = frequency-(E%E(ic1,ik1_ibz,1)-E%E(iv2,ik2_ibz,1)+(0.,1.)*W_(1))
     E_diff(2) = frequency-(E%E(ic2,ik2_ibz,1)-E%E(iv1,ik1_ibz,1)+(0.,1.)*W_(2))
     !
     M(ii1,ii2)=M(ii1,ii2)+EXCPH_mat(ii1,ii2)*(1./E_diff(1)+1./E_diff(2))
     !
   enddo 
 enddo 
 !
end subroutine
