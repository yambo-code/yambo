!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team 
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
integer function X_eh_setup(iq,X,Xen,Xk,minmax_ehe)
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:X_t,X_poles,X_poles_tab
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use frequency,     ONLY:rg_index_bg
 implicit none
 type(levels)::Xen 
 type(bz_samp)::Xk
 type(X_t):: X
 integer :: iq
 real(SP):: minmax_ehe(2)
 !
 ! Work Space
 !
 integer :: i1,i2,ik,ikp,iv,ic,Nb,Nt,i_sp,i_pole
 real(SP):: E_eh,f_eh
 real(SP), allocatable :: poles(:)
 !
 Nb=Xen%nbm-X%ib(1)+1
 Nt=X%ib(2)-Xen%nbf
 !
 allocate(poles(Xk%nbz*Nb*Nt*n_sp_pol))
 !
 ! Note that in metals not all the elements are defined
 !
 poles=0._SP 
 !
 X_eh_setup=0
 do i1=1,Xk%nbz
   do iv=X%ib(1),Xen%nbm
     do ic=Xen%nbf+1,X%ib(2)
       do i_sp=1,n_sp_pol
         !
         i2=qindx_X(iabs(iq),i1,1)
         ik=Xk%sstar(i1,1) 
         ikp=Xk%sstar(i2,1)
         !
         E_eh=Xen%E(ic,ik,i_sp)-Xen%E(iv,ikp,i_sp)
         !
         ! Note that all possible E_eh signs are accepted. Negative
         ! transitions energies appear at finite temperature.
         !
         ! The way to distinguish between resonant and anti-reonant transitions
         ! is to check fv(1-fc) factor that comes from the t>0 ordering
         ! of the G's function. In this way, however, E_eh can be negative as
         ! shown below
         !
         ! n(E) ----     
         !          *    
         !          4*  3
         !            -.   
         !              *
         !          1   2-._______
         !          -Eeh-
         !
         !   1,2 = v , 3,4 = c
         !
         !   both transitions 1->3 , 2->4 are resonant but 2->4 has negative energy
         !
         f_eh=Xen%f(iv,ikp,i_sp)*(spin_occ-Xen%f(ic,ik,i_sp))
         if (abs(f_eh)<epsilon(1.)) cycle
         !
         ! Drude "transition"
         !
         if (abs(E_eh)<1.E-5 .and. ik==ikp) then
           if(any((/ic/=Xen%bf,iv/=Xen%bf,ik/=Xen%kf,i_sp/=Xen%sf/)) ) cycle
           if(real(X%Wd)==0..and.aimag(X%Wd)==0.) cycle
         endif
         !
         if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0./))) cycle
         !
         X_eh_setup=X_eh_setup+1
         poles(X_eh_setup)=E_eh
         i_pole=X_eh_setup
         if (allocated(rg_index_bg)) i_pole=rg_index_bg(X_eh_setup)
         if (iq>0) X_poles_tab(i_pole,:)=(/i1,iv,ic,i_sp/)
         !
       enddo
     enddo
   enddo
 enddo
 !
 minmax_ehe=(/max(minval(poles(:X_eh_setup))-0.1_SP,0._SP),maxval(poles(:X_eh_setup))+0.1_SP/)
 !
 if (iq<0) then
   if (.not.allocated(X_poles)) then
     allocate(X_poles(X_eh_setup))
     X_poles=0.
   endif
   X_poles=X_poles+poles(:X_eh_setup)
 endif
 !
 deallocate(poles)
 !
end function
