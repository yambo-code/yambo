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
 subroutine K_dump_to_o_file(iq,mode)
!====================================
 use pars,  ONLY:HARTREE,pi,SP
 use BS,    ONLY:BSS_description,BSS_of_title,BSS_file_name,&
&                BSS_n_descs,BS_K_coupling
 use X_m,   ONLY:X_epsilon,alpha_dim,eps_2_alpha
 use com,   ONLY:msg,of_open_close
 implicit none
 integer    :: mode,iq
!ws
 character(5)      :: extension 
 integer           :: i1
 real(SP)          :: od(7)
 complex(SP)       :: alpha
!
!Haydock
 if (iabs(mode)==1)  extension='-bh'
!
!Diagonalization
 if (iabs(mode)==2)  extension='-bd'

 if (mode<0) then
   if (trim(alpha_dim)/='adim') then
     write (BSS_file_name,'(a,i3.3,a)') 'alpha_q',iq,trim(extension)
   else
     write (BSS_file_name,'(a,i3.3,a)') 'eps_q',iq,trim(extension)
   endif
   call of_open_close(BSS_file_name,'ot')
 else
   call of_open_close(BSS_file_name,'Ot')
 endif 

 if (iabs(mode)==1)  extension='o -bh'
 if (iabs(mode)==2)  extension='o -bd'
 do i1=1,BSS_n_descs
   call msg(extension,"",trim(BSS_description(i1)),INDENT=0)
 enddo
!
 if (mode>0) then
   call msg(extension,'#')
   if (mode==1) call msg(extension,'#',BSS_of_title,INDENT=0,USE_TABS=.TRUE.)
   if (mode==2) call msg(extension,'#',BSS_of_title(:5),INDENT=0,USE_TABS=.TRUE.)
   call msg(extension,'#')
   do i1=1,size(X_epsilon(1,:))
     !
     od(1)=real(X_epsilon(1,i1))*HARTREE
     od(2)=aimag(X_epsilon(2,i1))
     od(3)=real(X_epsilon(2,i1))
     od(4)=aimag(X_epsilon(3,i1))
     od(5)=real(X_epsilon(3,i1))
     !
     if (trim(alpha_dim)/='adim') then
       !
       ! POLARIZABILITY
       !----------------
       !
       ! This relation is exact
       ! 
       ! alpha = -eps_2_alpha X(1,1) / |q|^2      (1)
       !       =  (1 -eps_M^-1) eps_2_alpha/4/pi  (2)
       !
       ! and BOTH need the antiresonant part of X ( or eps_M(w) )
       !
       ! In the limit of eps_2_alpha (or the volume) -> \infty we get
       !
       ! eps_M^-1 \sim 1 - 4 pi /|q|^2 X(1,1)
       !
       ! so that
       !
       ! alpha=(epsilon_M-1) eps_2_alpha/4/pi (3)
       !
       ! this does not need the antiresonant part of X. 
       ! Now when using the BS kernel includes the coupling I use (2) (instead
       ! of (1) like in O_driver.F)
       !
       if (BS_K_coupling)      alpha=1.-1./X_epsilon(2,i1)  ! (2)
       if (.not.BS_K_coupling) alpha=X_epsilon(2,i1)-1.     ! (3)
       !
       od(2:3)=(/aimag(alpha),real(alpha)-1./)*eps_2_alpha/4./pi
       !
       alpha=X_epsilon(3,i1)-1.
       od(4:5)=(/aimag(alpha),real(alpha)-1./)*eps_2_alpha/4./pi
       !
     endif
     !
     if (mode==1) then
       od(6)=aimag(X_epsilon(4,i1))
       od(7)=real(X_epsilon(4,i1))
       if (trim(alpha_dim)/='adim') then
         od(6)=od(6)*eps_2_alpha/4./pi
         od(7)=(od(7)-1.)*eps_2_alpha/4./pi
       endif
       call msg(extension,'',od,INDENT=-2,USE_TABS=.TRUE.)
     else if (mode==2) then
       call msg(extension,'',od(:5),INDENT=-2,USE_TABS=.TRUE.)
     endif

   enddo
 endif
 call of_open_close(BSS_file_name)
 end subroutine
!
