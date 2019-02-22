!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use X_m,           ONLY:X_t,X_poles,X_Ein_poles,X_poles_tab,l_X_terminator,X_term_E
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use frequency,     ONLY:ordered_grid_index
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,PAR_IND_VAL_BANDS_X
 !
#include<memory.h>
 !
 type(levels)  ::Xen 
 type(bz_samp) ::Xk
 type(X_t)     ::X
 integer       ::iq
 real(SP)      ::minmax_ehe(2)
 !
 ! Work Space
 !
 integer :: i1,i2,ik,ikp,iv,ic,Nv,Nc,i_sp,i_pole,ic_min,Nc_XTerm
 logical :: MPI_logicals_on,l_XTerm_vv
 real(SP):: E_eh,E_in,f_eh,local_tresh
 real(SP), allocatable :: poles(:),Ein_poles(:)
 !
 Nv=Xen%nbm-X%ib(1)+1
 Nc=X%ib(2)-Xen%nbf
 !
 Nc_XTerm=0
 if (l_X_terminator) Nc_XTerm=Nv
 !
 YAMBO_ALLOC(poles,(Xk%nbz*Nv*(Nc+Nc_XTerm)*n_sp_pol))
 YAMBO_ALLOC(Ein_poles,(Xk%nbz*Nv*(Nc+Nc_XTerm)*n_sp_pol))
 !
 MPI_logicals_on=allocated(PAR_IND_Xk_bz%element_1D)
 !
 ! Note that in metals not all the elements are defined
 !
 poles=0._SP 
 Ein_poles=0._SP
 !
 !
 X_eh_setup=0
 local_tresh=epsilon(1._SP)
 !
 do i1=1,Xk%nbz
   !
   if (MPI_logicals_on) then
     if (.not.PAR_IND_Xk_bz%element_1D(i1)) cycle 
   endif
   !
   do iv=X%ib(1),X%ib(1)+Nv-1
     !
     if (MPI_logicals_on) then
       if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle 
     endif
     !
     ! setting terminator bnd range
     ic_min = X%ib(2)-Nc+1
     if (l_X_terminator) ic_min=X%ib(1) 
     !
     do  ic=ic_min,X%ib(2)
       do i_sp=1,n_sp_pol
         !
         if (MPI_logicals_on) then
           if (.not.PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic)) cycle 
         endif
         !
         i2=qindx_X(iabs(iq),i1,1)
         ik=Xk%sstar(i1,1) 
         ikp=Xk%sstar(i2,1)
         !
         ! terminator handling
         if (ic>=X%ib(1).and.ic<=Xen%nbm.and.l_X_terminator) then
           l_XTerm_vv=.TRUE.
           E_eh=X_term_E-Xen%E(iv,ikp,i_sp)       
         else
           l_XTerm_vv=.FALSE.
           E_eh=Xen%E(ic,ik,i_sp)-Xen%E(iv,ikp,i_sp)
         endif 
         !
         E_in=Xen%E(iv,ikp,i_sp)
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
         if (.not.l_XTerm_vv) then
           f_eh=Xen%f(iv,ikp,i_sp)*(spin_occ-Xen%f(ic,ik,i_sp))/spin_occ
         else
           f_eh=Xen%f(iv,ikp,i_sp)*Xen%f(ic,ik,i_sp)/spin_occ
         endif
         !
         if (abs(f_eh)<local_tresh)  cycle  
         ! When iv--> iv transitions are counted, f_eh=0. 
         ! Therefore  if (abs(f_eh)<local_tresh) cycle hold only 
         ! when the terminator  is off 
         !                                                              
         if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0._SP/))) cycle
         !
         X_eh_setup=X_eh_setup+1
         poles(X_eh_setup)=E_eh
         Ein_poles(X_eh_setup)=E_in
         !
         i_pole=X_eh_setup
         if (allocated(ordered_grid_index)) i_pole=ordered_grid_index(X_eh_setup)
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
     YAMBO_ALLOC(X_poles,(X_eh_setup))
     X_poles=0._SP
   endif
   X_poles=X_poles+poles(:X_eh_setup)
   !
   if (.not.allocated(X_Ein_poles)) then
     YAMBO_ALLOC(X_Ein_poles,(X_eh_setup))
     X_Ein_poles=0._SP
   endif
   X_Ein_poles=X_Ein_poles+Ein_poles(:X_eh_setup)
 endif
 !
 YAMBO_FREE(poles)
 YAMBO_FREE(Ein_poles)
 !
end function
