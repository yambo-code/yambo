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
 use real_time,      ONLY:Gr,Gr_indexes,Gr_matrix,Gr_T_steps,Gr_T_window,l_G_retarded_db,&
&                         Gr_mat_dim
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:nkibz
 use timing,         ONLY:live_timing
 use electrons,      ONLY:levels
 use IO_m,           ONLY:OP_WR_CL,OP_RD_CL,io_control,REP,VERIFY
 implicit none
 !
 type(levels) :: E
 !
 ! Work Space
 !
 integer :: i_n,i_k,i_m,i_p,i_np,i_kp,i_mp,i_pp,i_c,i1,i2,ID,io_err
 real(SP):: Gr_integrand(Gr_T_steps),Gr_T_step,DELTA(2),GAMMA_
 integer,  external :: io_G_retarded
 real(SP), external :: RIntegrate_fixed_step
 !
 do i_c=1,2
   if (i_c==2) then
     allocate(Gr_indexes(SC_bands(1):SC_bands(2),nkibz,SC_bands(1):SC_bands(2),nkibz))
     call mem_est("Gr_indexes",(/size(Gr_indexes)/),(/IP/))
   endif
   Gr_mat_dim=0
   do i_n=SC_bands(1),SC_bands(2)
     do i_k=1,nkibz
       do i_m=SC_bands(1),SC_bands(2)
         do i_p=1,nkibz
           if (local_condition(i_k,i_n,i_p,i_m)) then
             Gr_mat_dim=Gr_mat_dim+1
             if (i_c==2) then
               Gr_indexes(i_n,i_k,i_m,i_p)=Gr_mat_dim
               Gr_indexes(i_m,i_p,i_n,i_k)=Gr_mat_dim
             endif
           endif
         enddo
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
   if (l_G_retarded_db) then
     deallocate(Gr)
     call mem_est("G_retarded")
   endif
   return
 endif
 !
 call live_timing('G retarded integrals',(SC_bands(2)-SC_bands(2)+1)*nkibz)
 !
 if (l_G_retarded_db) Gr_T_step=Gr_T_window/Gr_T_steps
 !
 do i_np=SC_bands(1),SC_bands(2)
   do i_kp=1,nkibz
     do i_m=SC_bands(1),SC_bands(2)
       do i_p=1,nkibz
         !
         if (.not.local_condition(i_kp,i_np,i_p,i_m)) cycle
         !
         do i_mp=SC_bands(1),SC_bands(2)
           do i_pp=1,nkibz
             do i_n=SC_bands(1),SC_bands(2)
               do i_k=1,nkibz
                 !
                 if (.not.local_condition(i_pp,i_mp,i_k,i_n)) cycle
                 !
                 i1=Gr_indexes(i_np,i_kp, i_m, i_p)
                 i2=Gr_indexes(i_mp,i_pp, i_n, i_k)
                 !
                 if (i1>i2) cycle
                 !
                 if (l_G_retarded_db) then
                   !
                   Gr_integrand(:)=real( Gr(i_kp,i_np,:)*Gr(i_p,i_m,:)*&
&                                  conjg(Gr(i_pp,i_mp,:))*conjg(Gr(i_k,i_n,:)) )
                   Gr_matrix(i1,i2)=RIntegrate_fixed_step(Gr_integrand,Gr_T_steps,Gr_T_step)
                   !
                 else
                   !
                   DELTA(1) = E%E(i_np,i_kp,1)-E%E(i_n,i_k,1)
                   DELTA(2) = E%E(i_m,i_p,1)-E%E(i_mp,i_pp,1)
                   !
                   GAMMA_ = abs(E%W(i_np,i_kp,1))+abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pp,1))+abs(E%W(i_n,i_k,1))
                   !
                   Gr_matrix(i1,i2)=GAMMA_/((DELTA(1)+DELTA(2))**2+GAMMA_**2)
                   !
                 endif
                 !
               enddo
             enddo
           enddo
         enddo
       enddo
     enddo
     call live_timing(steps=1)
   enddo
 enddo
 !
 do i1=1,Gr_mat_dim
   do i2=i1+1,Gr_mat_dim
     Gr_matrix(i2,i1)=Gr_matrix(i1,i2)
   enddo
 enddo
 !
 if (l_G_retarded_db) then
   deallocate(Gr)
   call mem_est("G_retarded")
 endif
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
   logical function local_condition(i_k,i_n,i_p,i_m)
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
