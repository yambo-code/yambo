!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine G_shells_finder()
 !
 ! Output: g_rot(nsym,ng_closed)
 !
 ! R_is G_ig = G_{g_rot(is,ig)}
 !
 use pars,         ONLY:SP
 use LIVE_t,       ONLY:live_timing
 use D_lattice,    ONLY:nsym,inv_index
 use R_lattice,    ONLY:n_g_shells,ng_in_shell,ng_closed,&
&                       g_vec,g_rot,E_of_shell,rl_sop,minus_G
 use vec_operate,  ONLY:iku_v_norm,v_is_zero,sort,degeneration_finder
 use IO_m,         ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY
 use zeros,        ONLY:G_iku_zero
 use interfaces,   ONLY:PARALLEL_index
 use parallel_m,   ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer :: i_1,i_2,ig_1,ig_2,i_s,i_m,n_g_shells_no_holes
 integer :: G_mod_indx(ng_closed),ng_in_shell_TMP(ng_closed),first_G_in_shell(ng_closed)
 real(SP):: G_mod(ng_closed),g_rotated(3),E_of_shell_TMP(ng_closed)
 type(PP_indexes) :: px
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: io_GROT
 !
 ! I/O (read)
 !============
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=io_GROT(ID)
 !
 if (io_err==0) then
   if(inv_index<0) then
     YAMBO_FREE(ng_in_shell)
     YAMBO_FREE(E_of_shell)
     YAMBO_FREE(g_rot)
     YAMBO_FREE(minus_G)
     call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),MODE=VERIFY,ID=ID)
     io_err=io_GROT(ID)
   endif
   return
 else
   if (allocated(g_rot)) then
     YAMBO_FREE(ng_in_shell)
     YAMBO_FREE(E_of_shell)
     YAMBO_FREE(g_rot)
     YAMBO_FREE(minus_G)
   endif
 endif
 !
 ! Re-ordering by increasing module
 !==================================
 !
 do ig_1=1,ng_closed
   G_mod(ig_1)=iku_v_norm(g_vec(ig_1,:))
 enddo
 call sort(G_mod,indx=G_mod_indx)
 !
 ! Shells build-up by finding equal module G_vectors
 ! ==================================================
 !
 call degeneration_finder(G_mod,ng_closed,first_G_in_shell,ng_in_shell_TMP,n_g_shells,&
&                        1.E-5_SP,Include_single_values=.TRUE.)
 !
 ! Init
 !
 YAMBO_ALLOC(g_rot,(nsym,ng_closed))
 !
 g_rot=0
 if (myid==0) g_rot(:,1)=1 ! Gamma point
 E_of_shell_TMP=0._SP
 !
 ! Parallel Setup
 !================
 !
 call PP_indexes_reset(px)
 !
 call PARALLEL_index(px,(/n_g_shells/))
 !
 if (ng_closed>1) call live_timing('Shells finder',px%n_of_elements(myid+1))
 !==========================================================================
 !
 do i_m=2,n_g_shells
   !
   if (.not.px%element_1D(i_m)) cycle
   !
#if defined _OPENMP
!$omp parallel do default(shared), private(i_1,ig_1,i_s,g_rotated,i_2,ig_2)
#endif
   do i_1=first_G_in_shell(i_m),first_G_in_shell(i_m)+ng_in_shell_TMP(i_m)-1
     !
     ig_1=G_mod_indx(i_1)
     !
     if (i_1==first_G_in_shell(i_m)) E_of_shell_TMP(i_m)=iku_v_norm(g_vec(ig_1,:))**2/2._SP
     !
     do i_s=1,nsym
       !
       g_rotated=matmul(rl_sop(:,:,i_s),g_vec(ig_1,:))
       !
       do i_2=first_G_in_shell(i_m),first_G_in_shell(i_m)+ng_in_shell_TMP(i_m)-1
         !
         ig_2=G_mod_indx(i_2)
         !
         if (v_is_zero(g_rotated-g_vec(ig_2,:),zero_=G_iku_zero)) then
           !
           g_rot(i_s,ig_1)=ig_2
           !
           exit
           !
         endif
         !
       enddo
       !
     enddo
     !
   enddo
#if defined _OPENMP
!$omp end parallel do
#endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 if (ng_closed>1) call live_timing(steps=1)
 !
 call PP_redux_wait(g_rot)
 call PP_redux_wait(E_of_shell_TMP)
 !
 ! Redefine ng_in_shell_TMP witg actual position of the last element of the
 ! degenerate group
 !
 do i_m=2,n_g_shells
   ng_in_shell_TMP(i_m)=first_G_in_shell(i_m)+ng_in_shell_TMP(i_m)-1
 enddo
 !
 ! Check for SHELLS holes
 !
 n_g_shells_no_holes=n_g_shells
 !
 do i_m=n_g_shells,2,-1
   !
   if ( all(g_rot(:,:ng_in_shell_TMP(i_m))/=0) ) then
     n_g_shells_no_holes=i_m
     exit 
   endif
   !
 enddo
 n_g_shells=n_g_shells_no_holes
 !
 ! Shells Allocation & Transfer 
 !
 YAMBO_ALLOC(ng_in_shell,(n_g_shells))
 YAMBO_ALLOC(E_of_shell,(n_g_shells))
 !
 ng_in_shell=ng_in_shell_TMP(:n_g_shells)
 ng_closed=ng_in_shell(n_g_shells)
 !
 ! Andrea [12/10/2011]: when the symmetries are removed using ypp
 ! or when, for any reason, both the spatial inversion and the time-reversal
 ! are not introduced the coupling part of the BSE (and maybe also other parts
 ! of the code) must access an ad-hoc table of the -G vectors evaluated in eval_minus_G.
 ! However if the inversion symmetry is not used here the eval_minus_G routine
 ! will fail as the shells are not closed anymore with respect to the inversion.
 !
 if (inv_index<0) then
   YAMBO_ALLOC(minus_G,(ng_closed))
   call eval_minus_G()
   do while (  count(minus_G==0)>0 ) 
     n_g_shells=n_g_shells-1 
     ng_closed=ng_in_shell(n_g_shells)
     call eval_minus_G()
   enddo
   ng_in_shell(:n_g_shells)=ng_in_shell_TMP(:n_g_shells)
 endif
 !
 E_of_shell(:n_g_shells)=E_of_shell_TMP(:n_g_shells)
 !
 !I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
 io_err=io_GROT(ID)
 !
end subroutine
