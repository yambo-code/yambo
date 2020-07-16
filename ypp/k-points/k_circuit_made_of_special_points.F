!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine k_circuit_made_of_special_points( PATH, n_points )
 !
 use pars,                ONLY:SP,schlen
 use R_lattice,           ONLY:bz_samp_reset,b
 use D_lattice,           ONLY:lattice,a
 use com,                 ONLY:msg,of_open_close
 use vec_operate,         ONLY:c2a,v_norm
 use YPPm,                ONLY:K_transform,l_high_sym_pts,&
&                              SPECIAL_k,n_SPECIAL_k_MAX,CIRCUIT_k,SPECIAL_k_label,&
&                              n_SPECIAL_k_MAX,CIRCUIT_k_label,CIRCUIT_k_norm,CIRCUIT_made_of_SPECIAL_k
 use stderr,              ONLY:STRING_split,STRING_same,intc
 !
#include<memory.h>
 !
 character(*) :: PATH
 integer      :: n_points
 !
 ! Work Space
 !
 type :: USER_k
   character(schlen) :: string
   character(schlen) :: label
   integer           :: n_operations
   integer           :: iK_special(n_SPECIAL_k_MAX)
   real(SP)          :: iK_prefactor(n_SPECIAL_k_MAX)
 end type USER_k
 !
 integer, parameter :: MAX_n_lines=10
 type(USER_k)       :: Uk(MAX_n_lines)
 real(SP)           :: k_l(3),k_lp1(3),K_norm
 integer            :: i_Uk,i_sp,ik,i_plus,i_minus,n_Uk,&
&                      n_SPACE_strings,n_P_strings,n_M_strings,n_X_strings
 logical            :: minus_prefactor
 character(schlen)  :: SPACE_string(MAX_n_lines),dumb_ch,k_f_name(2)
 character(schlen)  :: P_string(MAX_n_lines),M_string(MAX_n_lines),X_string(MAX_n_lines),k_label
 !
 if (len_trim(PATH)==0) return
 !
 call bz_samp_reset(CIRCUIT_k)
 !
 call STRING_split(PATH,SPACE_string,n_non_empty_strings=n_SPACE_strings)
 !
 n_Uk=0
 !
 do i_sp=1,n_SPACE_strings
   !
   ! +
   call STRING_split(SPACE_string(i_sp),P_string,space="+",n_non_empty_strings=n_P_strings)
   !
   do i_plus=1,n_P_strings
     !
     ! -
     call STRING_split(P_string(i_plus),M_string,space="-",n_non_empty_strings=n_M_strings)
     !
     do i_minus=1,n_M_strings
       !
       dumb_ch=P_string(i_plus)
       minus_prefactor=i_minus>1.or.dumb_ch(1:1)=="-"
       !
       call STRING_split(M_string(i_minus),X_string,space="x",n_non_empty_strings=n_X_strings)
       !
       if  (n_X_strings==1) ik=which_K(X_string(1))
       if  (n_X_strings==2) ik=which_K(X_string(2))
       !
       if (ik/=0) then
         if (i_plus==1.and.i_minus==1) then
           n_Uk=n_Uk+1
           Uk(n_Uk)%n_operations=0
           Uk(n_Uk)%string=SPACE_string(i_sp)
         endif
         Uk(n_Uk)%n_operations=Uk(n_Uk)%n_operations+1
         Uk(n_Uk)%iK_special(Uk(n_Uk)%n_operations)=ik
         if  (n_X_strings==1) then
           Uk(n_Uk)%iK_prefactor(Uk(n_Uk)%n_operations)=1.
         else
           read(X_string(1),*) Uk(n_Uk)%iK_prefactor(Uk(n_Uk)%n_operations)
           if (minus_prefactor) Uk(n_Uk)%iK_prefactor(Uk(n_Uk)%n_operations)=&
&                              -Uk(n_Uk)%iK_prefactor(Uk(n_Uk)%n_operations)
         endif
       endif
       !
     enddo
     !
   enddo
 enddo
 !
 CIRCUIT_made_of_SPECIAL_k=n_Uk>0
 !
 if (n_Uk==0) return
 !
 if (n_points==0) then
   call msg('s','Found '//trim(intc(n_Uk))//' user special points')
   CIRCUIT_k%nbz = n_Uk
   YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
   allocate(CIRCUIT_k_label(CIRCUIT_k%nbz))
   do i_Uk=1,n_Uk
     CIRCUIT_k_label(i_Uk)=Uk(i_Uk)%string
     CIRCUIT_k%pt(i_Uk,:)=GIMME_K_path(i_Uk)
   enddo
   return
 endif
 !
 if (n_Uk>1) then
   call msg('s','Found '//trim(intc(n_Uk-1))//' segments with  '//trim(intc(n_points))//' points each')
   do i_Uk=1,n_Uk-1
     dumb_ch='Segment #'//trim(intc(i_Uk))//': '//trim(Uk(i_Uk)%string)//' to '//trim(Uk(i_Uk+1)%string)
     call msg('s',trim(dumb_ch))
   enddo
 endif
 !
 do ik=1,SPECIAL_k%nbz
   call c2a(v_in=SPECIAL_k%pt(ik,:),mode='ki2c')
 enddo
 !
 CIRCUIT_k%nbz =n_points* ( n_Uk -1 )+1
 YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
 allocate(CIRCUIT_k_norm(CIRCUIT_k%nbz))
 allocate(CIRCUIT_k_label(CIRCUIT_k%nbz))
 CIRCUIT_k%nbz=0
 CIRCUIT_k_norm=0._SP
 K_norm=0.
 CIRCUIT_k_label=" "
 !
 if (n_Uk>1) then
   do i_Uk=1,n_Uk-1
     !
     k_l  =GIMME_K_path(i_Uk)
     k_lp1=GIMME_K_path(i_Uk+1)
     !
     do ik=1,n_points
       CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
       if (ik==1) CIRCUIT_k_label(CIRCUIT_k%nbz)=Uk(i_Uk)%string
       CIRCUIT_k%pt(CIRCUIT_k%nbz,:3)=k_l+(k_lp1-k_l)/n_points*(ik-1)
       if (CIRCUIT_k%nbz>1) K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
       CIRCUIT_k_norm(CIRCUIT_k%nbz)= K_norm
     enddo
   enddo
 endif
 !
 CIRCUIT_k%nbz= CIRCUIT_k%nbz+1
 CIRCUIT_k%nibz=CIRCUIT_k%nbz
 CIRCUIT_k%pt(CIRCUIT_k%nbz,:)=GIMME_K_path(n_Uk)
 CIRCUIT_k_label(CIRCUIT_k%nbz)=Uk(n_Uk)%string
 if (n_Uk>1) then
   K_norm=K_norm+v_norm(  CIRCUIT_k%pt(CIRCUIT_k%nbz,:3) -  CIRCUIT_k%pt(CIRCUIT_k%nbz-1,:3) )
   CIRCUIT_k_norm(CIRCUIT_k%nbz)= K_norm
 endif
 !
 do ik=1,CIRCUIT_k%nbz
   call c2a(v_in=CIRCUIT_k%pt(ik,:),mode='kc2i')
 enddo
 do ik=1,SPECIAL_k%nbz
   call c2a(v_in=SPECIAL_k%pt(ik,:),mode='kc2i')
 enddo
 !
 YAMBO_ALLOC(CIRCUIT_k%weights,(CIRCUIT_k%nbz))
 CIRCUIT_k%weights=1._SP/real(CIRCUIT_k%nbz,SP)
 !
 ! Print
 !=======
 if (l_high_sym_pts) then
   !
   k_f_name(1)="path_points"
   k_f_name(2)="PWSCF_points"
   call of_open_close(k_f_name(1),'ot')
   call of_open_close(k_f_name(2),'ot')
   call msg('o  path',"#",(/"c1    ","c2    ","c3    ","Norm  "/),INDENT=0,USE_TABS=.true.)
   call msg('o PWSCF',"#",(/"c1    ","c2    ","c3    ","Weight"/),INDENT=0,USE_TABS=.true.)
   call msg('o path PWSCF',"#"," ",INDENT=0)
   !
   do ik=1,CIRCUIT_k%nbz
     !
     call K_transform(CIRCUIT_k%pt(ik,:),'iku')
     !
     k_label=" "
     if (len_trim(CIRCUIT_k_label(ik))>0) k_label= ' ['// trim(CIRCUIT_k_label(ik))//']'
     !
     call msg('o  path',"",(/CIRCUIT_k%pt(ik,:),CIRCUIT_k_norm(ik)/),INDENT=0,USE_TABS=.true.,after_msg=k_label)
     call msg('o PWSCF',"",(/CIRCUIT_k%pt(ik,:),1._SP/),INDENT=0,USE_TABS=.true.,after_msg=k_label)
     !
   enddo
   !
   YAMBO_FREE(SPECIAL_k%pt)
   YAMBO_FREE(CIRCUIT_k%pt)
   !
   call of_open_close(k_f_name(1))
   call of_open_close(k_f_name(2))
   !
 endif
 !
 contains
   !
   function GIMME_K_path(i_point)
     integer  :: i_point,i_op
     real(SP) :: GIMME_K_path(3)
     GIMME_K_path=0.
     do i_op=1,Uk(i_point)%n_operations
       GIMME_K_path=GIMME_K_path+ Uk(i_point)%iK_prefactor(i_op)*SPECIAL_k%pt(Uk(i_point)%iK_special(i_op),:)
     enddo
   end function
   !
   integer function which_K(label)
     character(*) :: label
     which_K=0
     do ik=1,SPECIAL_k%nbz
       if ( STRING_same(trim(label),trim(SPECIAL_k_label(ik))) ) which_K=ik
     enddo
   end function
   !
end subroutine k_circuit_made_of_special_points
