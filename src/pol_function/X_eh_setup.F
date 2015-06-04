!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use com,           ONLY:warning
 use X_m,           ONLY:X_t,X_poles,X_poles_tab,use_X_RIM,use_X_Drude
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use frequency,     ONLY:ordered_grid_index
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,PAR_IND_VAL_BANDS_X
 !
 implicit none
 !
 type(levels)  ::Xen 
 type(bz_samp) ::Xk
 type(X_t)     ::X
 integer       ::iq
 real(SP)      ::minmax_ehe(2)
 !
 ! Work Space
 !
 integer :: i1,i2,ik,ikp,iv,ic,Nv,Nc,i_sp,i_pole
 logical :: MPI_logicals_on,call_warning
 real(SP):: E_eh,f_eh
 real(SP), allocatable :: poles(:)
 !
 Nv=Xen%nbm-X%ib(1)+1
 Nc=X%ib(2)-Xen%nbf
 !
 allocate(poles(Xk%nbz*Nv*Nc*n_sp_pol))
 !
 MPI_logicals_on=associated(PAR_IND_Xk_bz%element_1D)
 !
 call_warning=.false.
 !
 ! Note that in metals not all the elements are defined
 !
 poles=0._SP 
 !
 X_eh_setup=0
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
     do ic=X%ib(2)-Nc+1,X%ib(2)
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
         if (abs(f_eh)<epsilon(1._SP)) cycle
         !
         ! Low energy "transition". 
         ! If I add a Drude term just the poles inside K_f are considered
         !
         if (abs(E_eh)<1.E-5_SP) then
           if ( .not.(use_X_RIM.or.use_X_drude) ) call_warning=.true.
           if ( any((/ic/=Xen%bf,iv/=Xen%bf,ik/=Xen%kf,i_sp/=Xen%sf/)) .and. use_X_drude ) cycle
         endif
         !
         if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0./))) cycle
         !
         X_eh_setup=X_eh_setup+1
         poles(X_eh_setup)=E_eh
         i_pole=X_eh_setup
         if (allocated(ordered_grid_index)) i_pole=ordered_grid_index(X_eh_setup)
         if (iq>0) X_poles_tab(i_pole,:)=(/i1,iv,ic,i_sp/)
         !
       enddo
     enddo
   enddo
 enddo
 !
 if(call_warning.and.iq>0) call warning ('Low energy transitions may be not properly described')
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
