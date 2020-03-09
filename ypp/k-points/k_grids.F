!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine k_grids(en,k,Xk,q)
 !
 use pars,                ONLY:SP,schlen
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nXkibz
 use D_lattice,           ONLY:nsym,lattice
 use com,                 ONLY:msg
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a,rlu_v_is_zero,v_is_zero,v_norm
 use YPPm,                ONLY:coo_in,coo_out,l_k_grid,l_q_grid,&
&                              l_shifted_grid,l_high_sym_pts,K_transform,&
&                              n_u_k_pts,n_u_q_pts,l_random_grid,&
&                              U_k_pts,U_q_pts,U_q_shifts,USER_k,k_grid_printer
 use LIVE_t,              ONLY:live_timing_is_on
 use zeros,               ONLY:k_iku_zero
 !
#include<memory.h>
 !
 type(levels) ::en       
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 type(bz_samp)::FINAL_k,Q_user
 real(SP)     ::real_ctl,v(3),ka(3),q0(3),kout(Xk%nibz,3),k_plus_q(Xk%nibz,3)
 integer      ::i1,i2,is,ik,iq
 integer,  allocatable :: int_ctl(:)
 real(SP), allocatable :: BZ_weights(:)
 character(schlen)     :: ch
 logical               :: No_Weight,Force_User_points,List_internal_points,Expand_internal_points
 !
 call bz_samp_reset(USER_k)
 call bz_samp_reset(Q_user)
 call bz_samp_reset(FINAL_k)
 !
 call section('*',"BZ grid analyzer/generator") 
 !
 ! Setup logicals 
 !
 if (l_high_sym_pts) l_high_sym_pts=trim(lattice)/='none'
 !
 !Q/Xk (IBZ->BZ)
 !
 call k_ibz2bz( q,'i',.false.)
 call k_ibz2bz(Xk,'i',.false.)
 !
 call msg("s",'Q-points  (BZ)', q%nbz)
 call msg("s",'K-points  (BZ)',Xk%nbz)
 !
 call parser('NoWeights'   ,No_Weight)
 call parser('ForceUserPts',Force_User_points)
 call parser('ListPts'     ,List_internal_points)
 call parser('ExpandPts'   ,Expand_internal_points)
 !
 if (l_random_grid) then
   call k_random(No_Weight)
   goto 1
 endif
 !
 if (len_trim(coo_in)==0) return
 !
 if (List_internal_points) then
   !
   write(ch,'(a,a,a)') "== Built-in grids [",trim(coo_out),"] =="
   !==================================================================
   call section('+',trim(ch))
   !
   if (l_k_grid) then
     if (Expand_internal_points) then
       call k_expand(Xk)
       call msg("s",'K-points (BZ) formatted')
       call k_grid_printer(Xk%nbz,Xk%ptbz,'iku',"Y")
       call msg("s",'K-points (BZ) un-formatted')
       call k_grid_printer(Xk%nbz,Xk%ptbz,'iku',"none")
       call msg("s",'K-points (BZ) PW-formatted')
       call k_grid_printer(Xk%nbz,Xk%ptbz,'iku',"PW")
     else
       YAMBO_ALLOC(BZ_weights,(Xk%nibz))
       BZ_weights(:)=Xk%weights(:)*real(Xk%nbz,SP)
       call msg("s",'K-points (IBZ) formatted')
       call k_grid_printer(Xk%nibz,Xk%pt,'iku',"Y",wk=BZ_weights)
       call msg("s",'K-points (IBZ) un-formatted')
       call k_grid_printer(Xk%nibz,Xk%pt,'iku',"none",wk=BZ_weights)
       call msg("s",'K-points (BZ) PW-formatted')
       call k_grid_printer(Xk%nibz,Xk%pt,'iku',"PW",wk=BZ_weights)
       YAMBO_FREE(BZ_weights)
     endif
   endif
   !
   if (l_q_grid) then
     if (Expand_internal_points) then
       call k_expand(q)
       call msg("s",'Q-points (BZ) formatted')
       call k_grid_printer(q%nbz,q%ptbz,'iku',"Y")
       call msg("s",'Q-points (BZ) un-formatted')
       call k_grid_printer(q%nbz,q%ptbz,'iku',"none")
       call msg("s",'Q-points (BZ) PW-formatted')
       call k_grid_printer(q%nbz,q%ptbz,'iku',"PW")
     else
       YAMBO_ALLOC(BZ_weights,(q%nibz))
       BZ_weights(:)=q%weights(:)*real(q%nbz,SP)
       call msg("s",'Q-points (IBZ) formatted')
       call k_grid_printer(q%nibz,q%pt,'iku',"Y",wk=BZ_weights)
       call msg("s",'Q-points (IBZ) un-formatted')
       call k_grid_printer(q%nibz,q%pt,'iku',"none",wk=BZ_weights)
       call msg("s",'Q-points (IBZ) PW-formatted')
       call k_grid_printer(q%nibz,q%pt,'iku',"PW",wk=BZ_weights)
       YAMBO_FREE(BZ_weights)
     endif
   endif
   !
   goto 1
   !
 endif
 !
 if (.not.l_high_sym_pts) then
   !
   write(ch,'(a,a,a)') "== Built-in IBZ K-grid [",trim(coo_out),"] =="
   !==================================================================
   call section('+',trim(ch))
   YAMBO_ALLOC(BZ_weights,(Xk%nibz))
   BZ_weights(:)=Xk%weights(:)*real(Xk%nbz,SP)
   call k_grid_printer(Xk%nibz,Xk%pt,'iku',"none",wk=BZ_weights)
   YAMBO_FREE(BZ_weights)
   !
 endif
 !
 if (l_k_grid) then
   !
   call section('=',"== GW K-grid analyzer ==")
   !===========================================
   !
   !Input file parsing
   !
   real_ctl=0.
   USER_k%nbz=1
   gw_main_loop: do ik=1,n_u_k_pts
     !
     YAMBO_FREE(USER_k%pt)
     YAMBO_ALLOC(USER_k%pt,(USER_k%nbz,3))
     USER_k%pt(USER_k%nbz,:)=U_k_pts(ik,:)
     !
     ! Transform the user_k_points in iku
     !
     if (trim(coo_in)=="cc")  call c2a(v_in=USER_k%pt(USER_k%nbz,:),mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=USER_k%pt(USER_k%nbz,:),mode="ka2i")
     !
     if (real_ctl/=999._SP) then
       !
       do i1=1,Xk%nbz
         if (v_is_zero(Xk%ptbz(i1,:)-USER_k%pt(USER_k%nbz,:),zero_=k_iku_zero)) then
           call warning(' Point already in the grid')
           exit gw_main_loop
         endif
       enddo
       !
       USER_k%nbz=USER_k%nbz+1
       !
     endif
   enddo gw_main_loop
   USER_k%nbz=USER_k%nbz-1
   !
   do i1=1,USER_k%nbz
     !      
     v=USER_k%pt(i1,:)
     if (trim(coo_in)=="cc")  call c2a(v_in=USER_k%pt(i1,:),v_out=v,mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=USER_k%pt(i1,:),v_out=v,mode="ka2i")
     USER_k%pt(i1,:)=v      
     !
   enddo
   !
   call msg("s",'User K-points ',USER_k%nbz)
   !
   ! Extended grid
   !
   YAMBO_ALLOC(FINAL_k%ptbz,(Xk%nibz+USER_k%nbz*q%nbz,3))
   FINAL_k%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
   FINAL_k%nbz=Xk%nibz
   do i1=1,USER_k%nbz
     do i2=1,q%nbz
       FINAL_k%nbz=FINAL_k%nbz+1
       FINAL_k%ptbz(FINAL_k%nbz,:)=USER_k%pt(i1,:)-q%ptbz(i2,:)
     enddo
   enddo
   !
   if (USER_k%nbz>0) call reduce_and_print(FINAL_k,.TRUE.)
   !
   call bz_samp_reset(FINAL_k)
   !
 endif
 !
 if (l_q_grid) then
   !
   call section('=',"== Q-grid analyzer ==")
   !========================================
   !
   ! Input file parsing
   !
   real_ctl=0.
   Q_user%nibz=1
   q_main_loop: do iq=1,n_u_q_pts
     !
     YAMBO_FREE(Q_user%pt)
     YAMBO_ALLOC(Q_user%pt,(Q_user%nibz,3))
     Q_user%pt(Q_user%nibz,:)=U_q_pts(iq,:)
     !
     do i1=1,q%nbz
       v=q%ptbz(i1,:)
       if (trim(coo_in)=="cc")  call c2a(v_in=q%ptbz(i1,:),v_out=v,mode="ki2c")
       if (trim(coo_in)=="rlu") call c2a(v_in=q%ptbz(i1,:),v_out=v,mode="ki2a")
       if (v_is_zero(v(:)-Q_user%pt(Q_user%nibz,:),zero_=k_iku_zero).and..not.Force_User_points) then
         call warning(' Point already in the grid. Listing internal Q-points.')
         cycle q_main_loop
       endif
     enddo
     !
     Q_user%nibz=Q_user%nibz+1
     !
   enddo q_main_loop
   Q_user%nibz=Q_user%nibz-1
   !
   if (Q_user%nibz>=1) call msg("s",'User Q-points ',Q_user%nibz)
   !
   if (Force_User_points) then
     call k_grid_printer(Q_user%nibz,Q_user%pt,trim(coo_in),"Y")
     goto 1
   endif
   !
   ! Extended grid
   !
   YAMBO_ALLOC(FINAL_k%ptbz,(Xk%nibz+Xk%nbz*Q_user%nibz,3))
   FINAL_k%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
   FINAL_k%nbz=Xk%nibz
   do i1=1,Q_user%nibz
     v=Q_user%pt(i1,:)
     if (trim(coo_in)=="cc")  call c2a(v_in=Q_user%pt(i1,:),v_out=v,mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=Q_user%pt(i1,:),v_out=v,mode="ka2i")
     do i2=1,Xk%nbz
       FINAL_k%nbz=FINAL_k%nbz+1
       FINAL_k%ptbz(FINAL_k%nbz,:)=v(:)-Xk%ptbz(i2,:)
     enddo
   enddo
   !
   if (Q_user%nibz>0) call reduce_and_print(FINAL_k,.FALSE.)
   !
   call bz_samp_reset(FINAL_k)
   !
 endif
 !
 if (l_shifted_grid) then
   !
   ! Generate shifted k-points set for dipole calculation
   !
  call section('=',"== Shifted grids generator ==")
   !================================================
   !
   if(any(abs(U_q_shifts)/=0._SP)) then
     !
     kout(:,:) = Xk%pt(:,:)
     !
     ! Convert IBZ mesh to coo_out
     !
     do ik=1,Xk%nibz
       call K_transform(kout(ik,:),'iku')
     enddo
     !
     i2=0
     do i1=1,3
       !
       if (v_norm(U_q_shifts(i1,:))>1.E-4.or.v_norm(U_q_shifts(i1,:))<1.E-6) then
         q0 = U_q_shifts(i1,:)*1.E-4/v_norm(U_q_shifts(i1,:))
         call msg("s","Renormalizing shift to :",v_norm(q0))
       else 
         q0 = U_q_shifts(i1,:)
       endif
       !
       i2 = i2 + 1
       !
       ! Convert U_q_shifts[iku] to coo_in
       !
       call K_transform(q0,trim(coo_in))
       !
       ! Print shift vector in new basis
       !
       write(ch,'(a,i1,a,3(f13.10,a),2a)') "Shifted K-grid[",i2,"]: {K} + (",q0(1),",",q0(2),",",q0(3),") [",trim(coo_out),"]"
       call section("=",trim(ch))
       !
       ! Apply the shift
       !
       do ik=1,Xk%nibz
         !
         k_plus_q(ik,:)=kout(ik,:)+q0(:)
         !
         if (v_norm(k_plus_q(ik,:)).lt.1.E-7) k_plus_q(ik,:) = 0.0_SP ! remove SP error k points
         !
       enddo
       !
       call k_grid_printer(Xk%nibz,k_plus_q,trim(coo_out),"none")
       !
     enddo
      !
   endif
   !
 end if
 !
 if (l_high_sym_pts) then
   !
   call section('+',"== Special Points for the "//trim(lattice)//" lattice ==")
   !===========================================================================  
   !
   call k_special(.FALSE.)
   !
 endif
 !
 live_timing_is_on=.true.
 !
1 continue
 !
 ! CLEAN
 !
 call bz_samp_reset(USER_k)
 call bz_samp_reset(Q_user)
 call k_ibz2bz(q,'d',.false.)
 call k_ibz2bz(Xk,'d',.false.)
 !
 contains
   !
   subroutine reduce_and_print(K_grid,GW_grid)
     !
     use zeros,   ONLY:k_rlu_zero,define_zeros
     use stderr,  ONLY:intc
     type(bz_samp)   ::K_grid
     logical         ::GW_grid
     !
     ! Work Space
     !
     real(SP), allocatable :: GWK_table(:)
     !
     ! Before doing any operation I need to redefine the zeros module
     ! components. This is beacuse K_grid contains additional points (the GW ones, for example)
     !
     call define_zeros(vector_=K_grid%ptbz,zero_=k_rlu_zero,RLU=.TRUE.)
     !
     call msg("s","Reducing & Expanding the "//trim(intc(K_grid%nbz))//" k-points ...")
     call k_reduce(K_grid,.true.)
     call k_expand(K_grid)
     !
     call msg("l","done")
     call msg("s","Reduced K-grid points:",K_grid%nibz)
     !
     ! the K_grid contains the final grid.
     ! When this contains the {k}+k_gw-{q} grids
     ! before reporting the points I want to sign the 
     ! position in the final grid of the given QP k-points 
     ! (read from the input file)
     !
     YAMBO_ALLOC(int_ctl,(K_grid%nibz))
     int_ctl=0
     !
     ! int_ctl =0 -> nothing
     ! int_ctl/=0 -> GW (index)
     !
     if (GW_grid) then
       do i1=1,K_grid%nibz
         call c2a(v_in=K_grid%pt(i1,:),v_out=ka,mode='ki2a')
         do i2=1,USER_k%nbz
           call c2a(v_in=USER_k%pt(i2,:),v_out=v,mode="ki2a")
           do is=1,nsym
             if (rlu_v_is_zero(v(:)-ka(:))) then
               if (int_ctl(i1)/=0) cycle
               int_ctl(i1)=i2
               exit
             endif
           enddo
         enddo
       enddo
     else
       int_ctl=0
     endif
     !
     YAMBO_ALLOC(GWK_table,(USER_k%nbz))
     !
     live_timing_is_on=.false.
     call msg("s","       ---------- Reduced K-grid ----------")
     GWK_table=0
     do i2=0,USER_k%nbz
       do i1=1,K_grid%nibz
         if (int_ctl(i1)/=i2.and..not.( i2==0.and.int_ctl(i1)/=0.and.i1<nXkibz )) cycle
         if (int_ctl(i1)/=0) then
            if(GWK_table(int_ctl(i1))>0) cycle
            GWK_table(int_ctl(i1))=1
         endif
         !
         call K_transform(K_grid%pt(i1,:),'iku')
         !
         if(No_Weight) then
           if (int_ctl(i1)==0) write (ch,'(3f12.7)') K_grid%pt(i1,:)
           if (int_ctl(i1)/=0) write (ch,'(3f12.7,i3)') K_grid%pt(i1,:),int_ctl(i1)
         else
           if (int_ctl(i1)==0) write (ch,'(4f12.7)') K_grid%pt(i1,:),K_grid%weights(i1)
           if (int_ctl(i1)/=0) write (ch,'(4f12.7,i3)') K_grid%pt(i1,:),&
&                                                     K_grid%weights(i1),int_ctl(i1)
         endif
         call msg("s",trim(ch))
       enddo
     enddo
     live_timing_is_on=.true.
     !
     YAMBO_FREE(GWK_table)
     YAMBO_FREE(int_ctl)
     !
   end subroutine
   !
end subroutine
