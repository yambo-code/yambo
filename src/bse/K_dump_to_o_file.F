!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine K_dump_to_o_file(iq,mode)
 !
 use pars,      ONLY:pi,SP
 use units,     ONLY:HARTREE
 use BS,        ONLY:BSS_description,BSS_of_title,BSS_file_name,&
&                    BSS_n_descs,BS_K_coupling,BS_K_is_ALDA,BSS_n_freqs
 use X_m,       ONLY:X_epsilon,eval_alpha,eps_2_alpha,O_eels
 use com,       ONLY:msg,of_open_close
 implicit none
 integer    :: mode,iq
 !
 ! Work Space
 !
 character(10)     :: extension,ch
 character(1)      :: ordering
 integer           :: i1,n_od
 real(SP)          :: od(7)
 complex(SP)       :: eels(3,BSS_n_freqs),alpha(3,BSS_n_freqs)
 logical           :: eval_eels
 !
 ! Haydock
 !
 if (iabs(mode)==1)  extension='-bh'
 !
 ! Diagonalization
 !
 if (iabs(mode)==2)  extension='-bd'
 !
 ! Inversion
 !
 if (iabs(mode)==3)  extension='-bi'
 !
 ! Inversion+ELPH
 !
 if (iabs(mode)==4) extension='-bi_elph'
 !
 ! ALDA
 !
 if (BS_K_is_ALDA) then
   ch=extension
   extension=trim(ch)//'_alda'
 endif
 !
 ordering='r'
 if (BS_K_coupling) ordering='c'
 eval_eels= O_eels(X_epsilon(1,:),BSS_n_freqs,.TRUE.,ordering) 
 !
 if (mode<0) then
   write (BSS_file_name(1),'(a,i3.3,a)') 'eps_q',iq,trim(extension)
   write (BSS_file_name(2),'(a,i3.3,a)') 'alpha_q',iq,trim(extension)
   write (BSS_file_name(3),'(a,i3.3,a)') 'eel_q',iq,trim(extension)
   !
   call of_open_close(BSS_file_name(1),'ot')
   if (eval_eels) then 
     call of_open_close(BSS_file_name(3),'ot')
     if (eval_alpha) call of_open_close(BSS_file_name(2),'ot')
   endif
 else
   call of_open_close(BSS_file_name(1),'Ot')
   if (eval_eels) then 
     call of_open_close(BSS_file_name(3),'Ot')
     if (eval_alpha) call of_open_close(BSS_file_name(2),'Ot')
   endif
 endif
 !
 do i1=1,BSS_n_descs
   call msg("o eps alpha","#",trim(BSS_description(i1)),INDENT=0)
 enddo
 call msg("o eps alpha",'#')
 !
 if (mode>0) then
   if (mode==1) call msg("o eps alpha",'#',BSS_of_title,INDENT=0,USE_TABS=.TRUE.)
   if (mode/=1) call msg("o eps alpha",'#',BSS_of_title(:5),INDENT=0,USE_TABS=.TRUE.)
   call msg("o eps alpha",'#')
   !
   ! ABSORPTION
   !------------
   !
   do i1=1,BSS_n_freqs
     !
     ! X_epsilon: 2 (int) 3 (non int) 4 (n-1 Haydock)
     !
     od(1:5)=(/real(X_epsilon(1,i1))*HARTREE,aimag(X_epsilon(2,i1)),real(X_epsilon(2,i1)),&
&              aimag(X_epsilon(3,i1)),real(X_epsilon(3,i1))/)
     !
     ! Hayodock n-1 iteration
     !
     n_od=5
     if (mode==1) n_od=7
     if (mode==1) od(6:7)=(/aimag(X_epsilon(4,i1)),real(X_epsilon(4,i1))/)
     !
     call msg("o eps",'',od(:n_od),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   ! EELS & POLARIZABILTY
   !---------------------
   !
   if (eval_eels) then
     !
     eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.TRUE.,ordering,X_epsilon(2,:),eels(1,:)) 
     eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.TRUE.,ordering,X_epsilon(3,:),eels(2,:)) 
     if (mode==1) eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.TRUE.,ordering,X_epsilon(4,:),eels(3,:)) 
     !
     alpha(1,:)=(1.+eels(1,:))/4./pi*eps_2_alpha
     alpha(2,:)=(1.+eels(2,:))/4./pi*eps_2_alpha
     if (mode==1) alpha(3,:)=(1.+eels(3,:))/4./pi*eps_2_alpha
     !
     do i1=1,BSS_n_freqs
       !
       ! EELS
       !
       od(1:5)=(/real(X_epsilon(1,i1))*HARTREE,aimag(eels(1,i1)),real(eels(1,i1)),&
&                aimag(eels(2,i1)),real(eels(2,i1))/)
       n_od=5
       if (mode==1) n_od=7
       if (mode==1) od(6:7)=(/aimag(eels(3,i1)),real(eels(3,i1))/)
       call msg("o eel",'',od(:n_od),INDENT=-2,USE_TABS=.TRUE.)
       !
       ! ALPHA
       !
       od(1:5)=(/real(X_epsilon(1,i1))*HARTREE,aimag(alpha(1,i1)),real(alpha(1,i1)),&
&                aimag(alpha(2,i1)),real(alpha(2,i1))/)
       n_od=5
       if (mode==1) n_od=7
       if (mode==1) od(6:7)=(/aimag(alpha(3,i1)),real(alpha(3,i1))/)
       call msg("o alpha",'',od(:n_od),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
   endif
   !
 endif
 !
 call of_open_close(BSS_file_name(1))
 call of_open_close(BSS_file_name(2))
 call of_open_close(BSS_file_name(3))
 !
end subroutine
