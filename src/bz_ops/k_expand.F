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
subroutine k_expand(k)
 !
 ! Outputs:
 ! 
 !   k%star k%sstar k%nstar k%weights k%nbz
 !
 !Given the kpoints and the simmetry operations
 !this sub. calculates weights (and rangs of the point
 !groups) as well the simm. operations contained
 !in each star:
 !
 !k%star(ik,i) = is | R_is ik =!= ik  with i=1,k%nstar(ik)
 !
 !k%nstar(ik)   = # of k-points in the Star of ik
 !k%weights(ik) = k%nstar(ik) / (Sum_ik nk(ik) )  = Num. of symm / rank(ko) / Nk^BZ
 !
 !
 !2-dimension example:
 !
 !-------------------
 !|        |        |
 !|   X1   |   X2   |
 !|        |        |
 !-------------------
 !|        |        |
 !|   X3   |   ko   |
 !|        |        |
 !-------------------
 !
 !In this case we have 8 symmetries and we obtain
 !from the IRREDUCIBLE ko other 3 REDUCIBLE k-points i
 !(X1, X2, X3)
 !
 !rang(ko) = 2  (I plus inversion that sends X2 < - > X3)
 !k%nstar(ko) = Num. of symm / rank(ko) = 8/2 = 4 (points in the star)
 !
 ! N.B
 ! rang(ik) = # of symmetries | R_is ik = ik
 !
 use pars,           ONLY:SP,IP
 use memory_m,       ONLY:mem_est
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:b,rl_sop,bz_samp
 use zeros,          ONLY:k_rlu_zero
 !
 implicit none
 type(bz_samp)::k
 !
 ! Work Space
 !
 integer         :: i1,i2,is
 real(SP)        :: v(3),kstar(nsym,3)
 logical         :: k_found
 !
 if (associated(k%sstar)) return
 !
 allocate(k%star(k%nibz,nsym),k%nstar(k%nibz),k%weights(k%nibz))
 call mem_est( trim(k%description)//'-star' ,(/k%nibz*(nsym+2)+k%nbz*2/),(/IP/))
 !
 k%nstar=0
 k%star=0
 do i1=1,k%nibz
   do is=1,nsym
     v=matmul(rl_sop(:,:,is),k%pt(i1,:))
     call k2bz(v_in=v)
     call c2a(v_in=v,v_out=kstar(is,:),mode='ki2a')
     k_found=.false.
     do i2=1,k%nstar(i1)
       if(rlu_v_is_zero(kstar(is,:)-kstar(k%star(i1,i2),:),zero_=k_rlu_zero)) k_found=.true.
       if (k_found) exit
     enddo
     if (.not.k_found) k%nstar(i1)=k%nstar(i1)+1
     if (.not.k_found) k%star(i1,k%nstar(i1))=is
   enddo
 enddo
 !
 k%weights(:)=real(k%nstar(:))/real(sum(k%nstar))
 k%weights(:)=k%weights(:)/sum(k%weights)
 k%nbz=sum(k%nstar(:))
 !
 allocate(k%sstar(k%nbz,2))
 i2=0
 do i1=1,k%nibz
   do is=1,k%nstar(i1)
     i2=i2+1
     k%sstar(i2,:)=(/i1,k%star(i1,is)/)
   enddo
 enddo
 !
end subroutine
