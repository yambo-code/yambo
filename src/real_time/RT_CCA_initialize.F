!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine RT_CCA_initialize(E)
 !
 use pars,           ONLY:IP,SP
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:Gr,Gr_indexes,Gr_matrix,Gr_T_steps,Gr_T_window,l_G_retarded_DB,&
&                         Gr_mat_dim,Gr_peak_pos,Gr_E_ranges
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:nkibz
 use par_proc_m,     ONLY:pp_indexes,pp_indexes_reset,myid,pp_redux_wait
 use par_indexes_m,  ONLY:par_indexes
 use parser_m,       ONLY:parser
 use timing,         ONLY:live_timing
 use electrons,      ONLY:levels
 use IO_m,           ONLY:OP_WR_CL,OP_RD_CL,io_control,REP,VERIFY
 implicit none
 !
 type(levels) :: E
 !
 ! Work Space
 !
 integer :: i_n,i_k,i_m,i_p,i_np,i_kp,i_mp,i_pp,i_c,i1,i2,ID,io_err,n_blocks
 real(SP):: Gr_T_step,GAMMA_(2),&
&           Gr_energies(((SC_bands(2)-SC_bands(1))*nkibz)**2,2),E_(2),E_center,&
&           Gr_integrand(Gr_T_steps),Gr_integral_min_val(2)
 type(pp_indexes)    :: px
 logical             :: l_do_not_cut
 integer,allocatable :: Gr_local_indexes(:,:)
 integer,    external:: io_G_retarded
 real(SP),   external:: Laplace_transform
 !
 call parser('DO_NOT_CUT',l_do_not_cut)
 !
 do i_c=1,2
   if (i_c==2) then
     allocate(Gr_indexes(SC_bands(1):SC_bands(2),nkibz,SC_bands(1):SC_bands(2),nkibz))
     call mem_est("Gr_indexes",(/size(Gr_indexes)/),(/IP/))
     allocate(Gr_local_indexes(Gr_mat_dim,4))
   endif
   Gr_mat_dim=0
   do i_n=SC_bands(1),SC_bands(2)
     do i_k=1,nkibz
       do i_m=SC_bands(1),SC_bands(2)
         p_loop: do i_p=1,nkibz
           !
           E_(1)=E%E(i_n,i_k,1)
           E_(2)=E%E(i_m,i_p,1)
           !
           if (local_condition(i_n,i_k,i_m,i_p)) then
             do i1=1,Gr_mat_dim
               if (abs(Gr_energies(i1,1)-E_(1))<1.E-5.and.&
&                  abs(Gr_energies(i1,2)-E_(2))<1.E-5) then
                 if (i_c==2) then
                   Gr_indexes(i_n,i_k,i_m,i_p)=i1
                   Gr_indexes(i_m,i_p,i_n,i_k)=i1
                 endif
                 cycle p_loop 
               endif
             enddo
             !
             Gr_mat_dim=Gr_mat_dim+1
             !
             Gr_energies(Gr_mat_dim,1)=E_(1)
             Gr_energies(Gr_mat_dim,2)=E_(2)
             !
             if (i_c==2) then
               Gr_local_indexes(Gr_mat_dim,:)=(/i_n,i_k,i_m,i_p/)
               Gr_indexes(i_n,i_k,i_m,i_p)=Gr_mat_dim
               Gr_indexes(i_m,i_p,i_n,i_k)=Gr_mat_dim
             endif
           endif
         enddo p_loop
       enddo
     enddo
   enddo
 enddo
 !
 ! Allocation
 !============
 allocate(Gr_matrix(Gr_mat_dim,Gr_mat_dim))
 call mem_est("Gr_matrix",(/size(Gr_matrix)/))
 Gr_matrix=0._SP
 !
 ! I/O (read)
 !============
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),MODE=VERIFY,COM=REP,ID=ID)
 io_err=io_G_retarded(ID,"G_retarded_matrix")
 !
 if (io_err==0) then
   if (l_G_retarded_DB) then
     deallocate(Gr,Gr_peak_pos,Gr_E_ranges)
     call mem_est("G_retarded G_peak_pos G_E_ranges")
   endif
   return
 endif
 !
 n_blocks=(Gr_mat_dim*Gr_mat_dim-Gr_mat_dim)/2+Gr_mat_dim
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/n_blocks/))
 !
 call live_timing('G retarded integrals',px%n_of_elements(myid+1))
 !
 if (l_G_retarded_DB) Gr_T_step=Gr_T_window/Gr_T_steps
 !
 n_blocks=0
 !
 Gr_integral_min_val=0.
 !
 do i1=1,Gr_mat_dim
   !
   i_np=Gr_local_indexes(i1,1)
   i_kp=Gr_local_indexes(i1,2)
   i_m =Gr_local_indexes(i1,3)
   i_p =Gr_local_indexes(i1,4)
   !
   do i2=1,i1
     !
     i_mp=Gr_local_indexes(i2,1)
     i_pp=Gr_local_indexes(i2,2)
     i_n =Gr_local_indexes(i2,3)
     i_k =Gr_local_indexes(i2,4)
     !
     n_blocks=n_blocks+1
     if (.not.px%element_1D(n_blocks)) cycle
     !
     if (l_G_retarded_DB) then
       !
       Gr_integrand(:)=real( Gr(i_kp,i_np,:)*Gr(i_p,i_m,:)*&
&                            conjg(Gr(i_pp,i_mp,:))*conjg(Gr(i_k,i_n,:)) )
       !
       E_center=Gr_peak_pos(i_pp,i_mp)+Gr_peak_pos(i_k,i_n)-Gr_peak_pos(i_kp,i_np)-Gr_peak_pos(i_p,i_m)
       !
       if (abs(E_center)<1.E-5) goto 1
       !
       GAMMA_(1) = Gr_E_ranges(i_pp,i_mp,1)+Gr_E_ranges(i_k,i_n,1)+Gr_E_ranges(i_kp,i_np,2)+Gr_E_ranges(i_p,i_m,2)
       GAMMA_(2) = Gr_E_ranges(i_pp,i_mp,2)+Gr_E_ranges(i_k,i_n,2)+Gr_E_ranges(i_kp,i_np,1)+Gr_E_ranges(i_p,i_m,1)
       !
       if (.not.l_do_not_cut) then
         if (E_center> GAMMA_(2)) goto 1
         if (E_center<-GAMMA_(1)) goto 1
       endif
       !
       Gr_matrix(i1,i2)=Laplace_transform(Gr_integrand,Gr_T_steps,Gr_T_step,E_center)
       !
     else
       !
       E_center=E%E(i_np,i_kp,1)-E%E(i_n,i_k,1)+E%E(i_m,i_p,1)-E%E(i_mp,i_pp,1)
       !
       GAMMA_ = abs(E%W(i_np,i_kp,1))+abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pp,1))+abs(E%W(i_n,i_k,1))
       !
       if (.not.l_do_not_cut) then
         if (E_center> GAMMA_(1)) goto 1
         if (E_center<-GAMMA_(1)) goto 1
       endif
       !
       Gr_matrix(i1,i2)=GAMMA_(1)/(E_center**2+GAMMA_(1)**2)
       !
     endif
     !
     if (E_center>Gr_integral_min_val(1)) then
       Gr_integral_min_val(1)=E_center
       Gr_integral_min_val(2)=min( Gr_integral_min_val(2), Gr_matrix(i1,i2))
     endif
     !
     !DEBUG>
     !write (100,*)  ((E%E(i_n,i_k,1)-E%E(i_np,i_kp,1))-(E%E(i_m,i_p,1)-E%E(i_mp,i_pp,1)))*27.21,Gr_matrix(i1,i2)
     !DEBUG<
     !
1    call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 do i1=1,Gr_mat_dim
   call pp_redux_wait(Gr_matrix(:,i1))
 enddo
 do i1=1,Gr_mat_dim
   do i2=1,i1
     Gr_matrix(i1,i2)=Gr_matrix(i1,i2)+Gr_integral_min_val(2)
     Gr_matrix(i2,i1)=Gr_matrix(i1,i2)
   enddo
 enddo
 !
 if (l_G_retarded_DB) then
   deallocate(Gr,Gr_peak_pos,Gr_E_ranges)
   call mem_est("G_retarded G_peak_pos G_E_ranges")
 endif
 deallocate(Gr_local_indexes)
 !
 call live_timing()
 !
 ! I/O (write)
 !=============
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),COM=REP,ID=ID)
 io_err=io_G_retarded(ID,"G_retarded_matrix")
 !
 contains
   !
   logical function local_condition(i_n,i_k,i_m,i_p)
     !
     integer i_k,i_n,i_p,i_m
     !
     local_condition=.FALSE.
     !
     if (i_k<i_p) then
       local_condition=.TRUE.
     else if (i_k==i_p.and.i_n<=i_m) then
       local_condition=.TRUE.
     endif
     !
   end function
   !
end subroutine RT_CCA_initialize
