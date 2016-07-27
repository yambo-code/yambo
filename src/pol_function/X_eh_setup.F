!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use X_m,           ONLY:X_t,X_poles,X_Ein_poles,X_poles_tab, l_X_terminator, X_term_E_offset
 use R_lattice,     ONLY:qindx_X,bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin_occ
 use frequency,     ONLY:ordered_grid_index
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,PAR_IND_VAL_BANDS_X
 !
 !XXXXXXX
 use wave_func,      ONLY:WF
 use parallel_m,    ONLY: myid
 !XXXXXX
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
 integer :: i1,i2,ik,ikp,iv,ic,Nv,Nc,i_sp,i_pole, ic_min, Nc_X
 logical :: MPI_logicals_on,l_XTerm_vv
 real(SP):: E_eh,E_in,f_eh,local_tresh
 real(SP), allocatable :: poles(:),Ein_poles(:)
 !
 WRITE(33, *)  "X_eh_setup", l_X_terminator
 !
 Nv=Xen%nbm-X%ib(1)+1
 Nc=X%ib(2)-Xen%nbf
 !XXXXXXXX
 Nc_X=0._SP 
 if (l_X_terminator) Nc_X=Nv
 !XXXXXX
 allocate(poles(Xk%nbz*Nv*(Nc+Nc_X)*n_sp_pol))
 allocate(Ein_poles(Xk%nbz*Nv*(Nc+Nc_X)*n_sp_pol))
 !
 MPI_logicals_on=associated(PAR_IND_Xk_bz%element_1D)
 !
 ! Note that in metals not all the elements are defined
 !
 poles=0._SP 
 Ein_poles=0._SP
 !
!XXXX         
 WRITE(500+myid,*) "SHAPE WF%index", SHAPE(WF%index)
 DO i_sp = lbound(WF%index,3), ubound(WF%index,3)
 WRITE(500+myid,"(a)") "sono qui i_sp "
 DO ik   = lbound(WF%index,2), ubound(WF%index,2)
 WRITE(500+myid,"(a)") "sono qui ik"
 DO ic   = lbound(WF%index,1), ubound(WF%index,1)
 WRITE(500+myid,"(a)") "sono qui i_c "
    WRITE(500+myid,"(a,3i5,i5,l)") "ic,ik,isp, WF%index(ic,ik,i_sp), WF%state(ic,ik,i_sp) ", ic, ik, i_sp, WF%index(ic,ik,i_sp), WF%state(ic,ik,i_sp)
 ENDDO
 ENDDO
 ENDDO
 WRITE(500+myid,*)
!XXXX         
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
     !XXXXXXXXXXXXXXXX
     if (l_X_terminator) then
      ic_min=X%ib(1) 
      else
      ic_min = X%ib(2)-Nc+1
     endif
     do  ic=ic_min, X%ib(2)
    !XXXXXXXXXX
     ! WRITE(201, "(a, 2i5)") "ic_min=", ic_min, ic
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
         ! XXXXXXXX    have i to change 12, ik and ipp when the terminator is turned on?
         if (ic >= X%ib(1) .and. ic <= Xen%nbm .and. l_X_terminator) then
           l_XTerm_vv=.TRUE.
           E_eh= X_term_E_offset - Xen%E(iv,ikp,i_sp)       
         else
           l_XTerm_vv=.FALSE.
           E_eh= Xen%E(ic,ik,i_sp)-Xen%E(iv,ikp,i_sp)
         endif 
         ! XXXXX  
         WRITE(850,"(a,2i5,2f12.6)" ) "iv, ic, E_eh, X_term_E_offset",iv, ic, E_eh, X_term_E_offset
          E_in=Xen%E(iv,ikp,i_sp)
         ! XXXXXX
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
         !XXXXXXXXXXXXX 
         !WRITE(850,"(a,2i5,f12.6)" ) "ic, iv,  f_eh", ic, iv, f_eh
         if (abs(f_eh)<local_tresh)  cycle  ! When iv--> iv transitions are counted, f_eh=0. 
         !                                                             Therefore  if (abs(f_eh)<local_tresh) cycle hold only 
         !                                                             when the terminator  is off 
         !                                                              
         ! XXXXXXXXX 
         !
          if (any((/abs(E_eh)<X%ehe(1),abs(E_eh)>X%ehe(2).and.X%ehe(2)>0._SP/))) cycle
         !
         !XXXXX
         ! WRITE(850,"(a,  2f12.6)" ) "X%ehe(1), X%ehe(2)", X%ehe(1), X%ehe(2)
         ! Skip these poles because dipoles are not defined in this case
         if (abs(E_eh)<1.E-5 .and. abs(iq)==1) then
         !WRITE(850,"(a,  f12.6)" ) "Xen%bf",Xen%bf 
           if(any((/ic/=Xen%bf,iv/=Xen%bf,ikp/=Xen%kf,ik/=Xen%kf,i_sp/=Xen%sf/)) ) cycle
         !XXXXXXXXXXXXX 
           if(real(X%Wd)==0..and.aimag(X%Wd)==0.) cycle
         endif
         !
         X_eh_setup=X_eh_setup+1
         poles(X_eh_setup)=E_eh
         Ein_poles(X_eh_setup)=E_in
         !
         i_pole=X_eh_setup
         if (allocated(ordered_grid_index)) i_pole=ordered_grid_index(X_eh_setup)
         if (iq>0) X_poles_tab(i_pole,:)=(/i1,iv,ic,i_sp/)
         !
!XXXX         
         !WRITE(500,"(a,3i5,i5)") "ic,ik,isp, ind", ic, ik, i_sp, WF%index(ic,ik,i_sp)
         !WRITE(600,"(a,3i5,l)") "ic,ik,isp, log", ic, ik, i_sp, WF%state(ic,ik,i_sp)
!XXXX         
       enddo
     enddo
   enddo
 enddo
 !
 minmax_ehe=(/max(minval(poles(:X_eh_setup))-0.1_SP,0._SP),maxval(poles(:X_eh_setup))+0.1_SP/)
 !
  WRITE(301, "(a, 2f12.6)") "minmax_ehe(1), minmax-ehe(2)", minmax_ehe(1), minmax_ehe(2) 
 !
 if (iq<0) then
   if (.not.allocated(X_poles)) then
     allocate(X_poles(X_eh_setup))
     X_poles=0._SP
   endif
   X_poles=X_poles+poles(:X_eh_setup)
   !
   if (.not.allocated(X_Ein_poles)) then
     allocate(X_Ein_poles(X_eh_setup))
     X_Ein_poles=0._SP
   endif
   X_Ein_poles=X_Ein_poles+Ein_poles(:X_eh_setup)
 endif
 !
 deallocate(poles,Ein_poles)
! WRITE(850,"(a)" ) "fine_X-eh_setup"
 !
end function
