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
 integer function X_ehg(iq,X,Xen,Xk,minmax_ehe)
!==============================================
 use pars,          ONLY:SP
 use X_m,           ONLY:X_t,X_poles,X_poles_tab
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_spin,spin_occ
 use frequency,     ONLY:rg_index_bg
 implicit none
 type(levels)::Xen 
 type(bz_samp)::Xk
 type(X_t):: X
 integer :: iq
 real(SP):: minmax_ehe(2)
!ws
 integer :: i1,i2,ik,ikp,iv,ic,Nb,Nt,is,i_pole
 real(SP):: edf,ff
 real(SP), allocatable :: poles(:)

 Nb=Xen%nbm-X%ib(1)+1
 Nt=X%ib(2)-Xen%nbf

 allocate(poles(Xk%nbz*Nb*Nt*n_spin))
!
!Note that in metals not all the elements are defined
!
 poles=0._SP 

 X_ehg=0
 do i1=1,Xk%nbz
   do iv=X%ib(1),Xen%nbm
     do ic=Xen%nbf+1,X%ib(2)
       do is=1,n_spin
         i2=qindx_X(iabs(iq),i1,1)
         ik=Xk%sstar(i1,1) 
         ikp=Xk%sstar(i2,1)
         edf=Xen%E(ic,ik,is)-Xen%E(iv,ikp,is)
         ff=Xen%f(iv,ikp,is)*(spin_occ-Xen%f(ic,ik,is))
         if (ff<epsilon(1.)) cycle
         if (edf<1.E-5.and.&
&            any((/ic/=Xen%bf,iv/=Xen%bf,ik/=Xen%kf,is/=Xen%sf/)) ) cycle
         if (any((/abs(edf)<X%ehe(1),abs(edf)>X%ehe(2).and.X%ehe(2)>0./))) cycle
         X_ehg=X_ehg+1
         poles(X_ehg)=edf
         i_pole=X_ehg
         if (allocated(rg_index_bg)) i_pole=rg_index_bg(X_ehg)
         if (iq>0) X_poles_tab(i_pole,:)=(/i1,iv,ic,is/)
       enddo
     enddo
   enddo
 enddo

 minmax_ehe=(/max(minval(poles(:X_ehg))-0.1,0.),maxval(poles(:X_ehg))+0.1/)

 if (iq<0) then
   if (.not.allocated(X_poles)) then
     allocate(X_poles(X_ehg))
     X_poles=0.
   endif
   X_poles=X_poles+poles(:X_ehg)
 endif

 deallocate(poles)

 end function
