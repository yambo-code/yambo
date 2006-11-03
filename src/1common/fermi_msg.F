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
 subroutine fermi_msg(E,mode)
!============================
 use pars,          ONLY:SP,lchlen,HARTREE
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol,spin_occ,n_spinor
 implicit none
 type(levels)::E
 integer     ::mode
! ws
 integer :: i1
 real(SP):: r1
 real(SP),parameter:: tresh=epsilon(1._SP)
 character(lchlen) :: lch
!
! Evaluate Metallic/Filled bands
!--------------------------------
 E%nbm=0
 E%nbf=0
 do i1=1,E%nb
   if (n_spinor==1) r1=sum(E%f(i1,:,:))/2.
   if (n_spinor==2) r1=sum(E%f(i1,:,:))
   if (r1<=tresh) cycle
   if (abs(r1-real(E%nk,SP))<=tresh) then
     E%nbf=i1
     cycle
   endif
   E%nbm=i1
 enddo
 if (E%nbm==0) E%nbm=E%nbf
!
 if (mode<=1) return
 i1=1
 if (E%nbf==0) i1=0
 if (E%nbf/=E%nbm) then
   E%Efermi(2:)=0.
   call msg('r','States summary             : Full        Metallic    Empty')
   write (lch,'(29x,3(i4.4,a,i4.4,3x))') i1,'-',E%nbf,&
&                                       E%nbf+1,'-',E%nbm,E%nbm+1,'-',E%nb
   call msg('r',trim(lch))
 else
   call msg('r','States summary             : Full        Empty')
   write (lch,'(29x,3(i4.4,a,i4.4,3x))') i1,'-',E%nbf,E%nbm+1,'-',E%nb
   call msg('r',trim(lch))
!
! E%Efermi(1) = Fermi Level
! E%Efermi(2) = VB max (with resepect to Efermi(1) )
! E%Efermi(3) = CB min (with resepect to Efermi(1) )
!
   E%Efermi(2:)=E%Efermi(1)+(/maxval(E%E(E%nbf,:,:)),minval(E%E(E%nbf+1,:,:))/)
!
   if (n_sp_pol==2) then
     call msg('r','Indirect Gap [ev][min-max][up]:',(/&
&             ( minval(E%E(E%nbf+1,:,2))-maxval(E%E(E%nbf,:,2)) )*HARTREE,&
&             ( maxval(E%E(E%nbf+1,:,2))-minval(E%E(E%nbf,:,2)) )*HARTREE /))
     call msg('r','                          [dn]:',(/&
&             ( minval(E%E(E%nbf+1,:,1))-maxval(E%E(E%nbf,:,1)) )*HARTREE,&
&             ( maxval(E%E(E%nbf+1,:,1))-minval(E%E(E%nbf,:,1)) )*HARTREE /))
   else
     call msg('r','Indirect Gap  [ev][min-max]:',(/&
&             ( minval(E%E(E%nbf+1,:,1))-maxval(E%E(E%nbf,:,1)) )*HARTREE,&
&             ( maxval(E%E(E%nbf+1,:,1))-minval(E%E(E%nbf,:,1)) )*HARTREE /))
   endif
 endif

 end subroutine

