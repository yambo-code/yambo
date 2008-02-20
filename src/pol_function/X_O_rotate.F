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
subroutine X_O_rotate(X_bands)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:nXkibz
 use SC,             ONLY:SC_bands,SC_R,load_SC_components,H_nl_sc,H_rotate
 use X_m,            ONLY:X_q0grad
 use IO_m,           ONLY:NONE,DUMP
 use memory_m,       ONLY:mem_est
 implicit none
 !
 ! Work Space
 !
 integer     :: X_bands,ik,i_cart,i1,i2,i3,i4,i_ter
 complex(SP) :: grad_WS_n(X_bands,X_bands),grad_WS_nm1(X_bands,X_bands)
 !
 ! First I read the rotation matrices
 !                  -----------------
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP,ik=0)
 !
 ! and the header of the H_sc
 !
 call load_SC_components('Hsc',COM_=NONE,MODE_=DUMP,ik=0)
 !
 do ik=1,nXkibz
   !
   call load_SC_components('Hsc',COM_=NONE,MODE_=DUMP,ik=ik)
   !
   call H_rotate(SC_R(:,:,ik),H_nl_sc,SC_bands,1)
   !
   do i_cart=1,3
     !
     grad_WS_nm1(:,:)=X_q0grad(i_cart,:X_bands,:X_bands,ik,1)
     !
     do i_ter=1,100
       !
       grad_WS_n(:,:)=X_q0grad(i_cart,:X_bands,:X_bands,ik,1)

!     !
!     X_q0grad(i_cart,:X_bands,:X_bands,ik,1)=(0.,0.)
!     !
!     ! X_q0grad (I1,I2) = i <I2|r|I1> = \sum_i3 i4
!     !                    <I2|i3> i <i3|r|i4> <i4|I1> =
!     !                    \sum_i3 i4 SC_R(i3,I2)^*  X_q0grad (i4,i3) SC_R(i4,I1)
!     !
!     do I1=1,X_bands
!       do I2=1,X_bands
!         do i3=1,X_bands
!           do i4=1,X_bands
!             X_q0grad(i_cart,I1,I2,ik,1)=X_q0grad(i_cart,I1,I2,ik,1)+&
!&            conjg(SC_R(i3,I2,ik))*grad_WS(i4,i3)*SC_R(i4,I1,ik)
!           enddo
!         enddo
!       enddo
!     enddo
     !
     enddo ! i_ter
     !
   enddo
   !
 enddo
 !
 call load_SC_components('WF_clean', COM_=NONE,MODE_=DUMP,ik=0)
 call load_SC_components('Hsc_clean',COM_=NONE,MODE_=DUMP,ik=0)
 !
 stop
 !
end subroutine
