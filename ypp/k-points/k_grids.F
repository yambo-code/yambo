!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use com,                 ONLY:msg,warning
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a,rlu_v_is_zero,v_is_zero,v_norm
 use YPP,                 ONLY:coo_in,coo_out,l_k_grid,l_q_grid,&
&                              l_shifted_grid,l_high_sym_pts,K_transform,&
&                              n_u_k_pts,n_u_q_pts,l_random_grid,&
&                              U_k_pts,U_q_pts,U_q_shifts
 use LIVE_t,              ONLY:live_timing_is_on
 use zeros,               ONLY:k_iku_zero
 use stderr,              ONLY:set_real_printed_length
 !
#include<memory.h>
 type(levels) ::en       
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 type(bz_samp)::USER_K,Final_K,Q_user
 real(SP)     ::real_ctl,v(3),ka(3),q0(3),kout(Xk%nibz,3),k_plus_q(Xk%nibz,3)
 integer      ::i1,i2,is,ik,iq
 integer,  allocatable :: int_ctl(:)
 real(SP), allocatable :: BZ_weights(:)
 character(schlen)     :: ch
 logical               :: No_Weight,Force_User_points,List_internal_points,Expand_internal_points
 !
 call bz_samp_reset(USER_K)
 call bz_samp_reset(Q_user)
 call bz_samp_reset(Final_K)
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
 call msg("s",'Q-points  (BZ):', q%nbz)
 call msg("s",'K-points  (BZ):',Xk%nbz)
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
       YAMBO_ALLOC(BZ_weights,(Xk%nbz))
       BZ_weights=1._SP
       call msg("s",'K-points (BZ) formatted')
       call print_the_grid(Xk%ptbz,Xk%nbz,BZ_weights,'iku',.TRUE.,.FALSE.)
       call msg("s",'K-points (BZ) un-formatted')
       call print_the_grid(Xk%ptbz,Xk%nbz,BZ_weights,'iku',.FALSE.,.FALSE.)
       call msg("s",'K-points (BZ) PW-formatted')
       call print_the_grid(Xk%ptbz,Xk%nbz,BZ_weights,'iku',.FALSE.,.TRUE.)
       YAMBO_FREE(BZ_weights)
     else
       YAMBO_ALLOC(BZ_weights,(Xk%nibz))
       BZ_weights(:)=Xk%weights(:)*real(Xk%nbz,SP)
       call msg("s",'K-points (IBZ) formatted')
       call print_the_grid(Xk%pt,Xk%nibz,BZ_weights,'iku',.TRUE.,.FALSE.)
       call msg("s",'K-points (IBZ) un-formatted')
       call print_the_grid(Xk%pt,Xk%nibz,BZ_weights,'iku',.FALSE.,.FALSE.)
       call msg("s",'K-points (BZ) PW-formatted')
       call print_the_grid(Xk%pt,Xk%nibz,BZ_weights,'iku',.FALSE.,.TRUE.)
       YAMBO_FREE(BZ_weights)
     endif
   endif
   !
   if (l_q_grid) then
     if (Expand_internal_points) then
       call k_expand(q)
       YAMBO_ALLOC(BZ_weights,(q%nbz))
       BZ_weights=1._SP
       call msg("s",'Q-points (BZ) formatted')
       call print_the_grid(q%ptbz,q%nbz,BZ_weights,'iku',.TRUE.,.FALSE.)
       call msg("s",'Q-points (BZ) un-formatted')
       call print_the_grid(q%ptbz,q%nbz,BZ_weights,'iku',.FALSE.,.FALSE.)
       call msg("s",'Q-points (BZ) PW-formatted')
       call print_the_grid(q%ptbz,q%nbz,BZ_weights,'iku',.FALSE.,.TRUE.)
       YAMBO_FREE(BZ_weights)
     else
       YAMBO_ALLOC(BZ_weights,(q%nibz))
       BZ_weights(:)=q%weights(:)*real(q%nbz,SP)
       call msg("s",'Q-points (IBZ) formatted')
       call print_the_grid(q%pt,q%nibz,BZ_weights,'iku',.TRUE.,.FALSE.)
       call msg("s",'Q-points (IBZ) un-formatted')
       call print_the_grid(q%pt,q%nibz,BZ_weights,'iku',.FALSE.,.FALSE.)
       call msg("s",'Q-points (IBZ) PW-formatted')
       call print_the_grid(q%pt,q%nibz,BZ_weights,'iku',.FALSE.,.TRUE.)
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
   call print_the_grid(Xk%pt,Xk%nibz,BZ_weights,'iku',.FALSE.,.FALSE.)
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
   USER_K%nibz=1
   gw_main_loop: do ik=1,n_u_k_pts
     !
     YAMBO_FREE(USER_K%pt)
     YAMBO_ALLOC(USER_K%pt,(USER_K%nibz,3))
     USER_K%pt(USER_K%nibz,:)=U_k_pts(ik,:)
     !
     ! Transform the user_k_points in iku
     !
     if (trim(coo_in)=="cc")  call c2a(v_in=USER_K%pt(USER_K%nibz,:),mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=USER_K%pt(USER_K%nibz,:),mode="ka2i")
     !
     if (real_ctl/=999._SP) then
       !
       do i1=1,Xk%nbz
         if (v_is_zero(Xk%ptbz(i1,:)-USER_K%pt(USER_K%nibz,:),zero_=k_iku_zero)) then
           call warning(' Point already in the grid')
           exit gw_main_loop
         endif
       enddo
       !
       USER_K%nibz=USER_K%nibz+1
       !
     endif
   enddo gw_main_loop
   USER_K%nibz=USER_K%nibz-1
   !
   do i1=1,USER_K%nibz
     !      
     v=USER_K%pt(i1,:)
     if (trim(coo_in)=="cc")  call c2a(v_in=USER_K%pt(i1,:),v_out=v,mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=USER_K%pt(i1,:),v_out=v,mode="ka2i")
     USER_K%pt(i1,:)=v      
     !
   enddo
   !
   call msg("s",'User K-points :',USER_K%nibz)
   !
   ! Extended grid
   !
   YAMBO_ALLOC(Final_K%ptbz,(Xk%nibz+USER_K%nibz*q%nbz,3))
   Final_K%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
   Final_K%nbz=Xk%nibz
   do i1=1,USER_K%nibz
     do i2=1,q%nbz
       Final_K%nbz=Final_K%nbz+1
       Final_K%ptbz(Final_K%nbz,:)=USER_K%pt(i1,:)-q%ptbz(i2,:)
     enddo
   enddo
   !
   if (USER_K%nibz>0) call reduce_and_print(Final_K,.TRUE.)
   !
   call bz_samp_reset(Final_K)
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
   if (Q_user%nibz>=1) call msg("s",'User Q-points :',Q_user%nibz)
   !
   if (Force_User_points) then
     YAMBO_ALLOC(BZ_weights,(Q_user%nibz))
     BZ_weights=0._SP
     call print_the_grid(Q_user%pt,Q_user%nibz,BZ_weights,coo_in,.TRUE.,.FALSE.)
     YAMBO_FREE(BZ_weights)
     goto 1
   endif
   !
   ! Extended grid
   !
   YAMBO_ALLOC(Final_K%ptbz,(Xk%nibz+Xk%nbz*Q_user%nibz,3))
   Final_K%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
   Final_K%nbz=Xk%nibz
   do i1=1,Q_user%nibz
     v=Q_user%pt(i1,:)
     if (trim(coo_in)=="cc")  call c2a(v_in=Q_user%pt(i1,:),v_out=v,mode="kc2i")
     if (trim(coo_in)=="rlu") call c2a(v_in=Q_user%pt(i1,:),v_out=v,mode="ka2i")
     do i2=1,Xk%nbz
       Final_K%nbz=Final_K%nbz+1
       Final_K%ptbz(Final_K%nbz,:)=v(:)-Xk%ptbz(i2,:)
     enddo
   enddo
   !
   if (Q_user%nibz>0) call reduce_and_print(Final_K,.FALSE.)
   !
   call bz_samp_reset(Final_K)
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
       ! Print the shifted mesh
       ! Ensure 7 *significant figures*, otherwise round off errors may occur later
       !
       call set_real_printed_length(f_length=10,g_length=10)
       !
       YAMBO_ALLOC(BZ_weights,(Xk%nibz))
       BZ_weights=1._SP
       call print_the_grid(k_plus_q,Xk%nibz,BZ_weights,coo_out,.FALSE.,.FALSE.)
       YAMBO_FREE(BZ_weights)
       !
       call set_real_printed_length( )
       !
     enddo
      !
   endif
   !
 end if
 !
 ! GPL_EXCLUDE_START
 !
 if (l_high_sym_pts) then
   !
   call section('+',"== Special Points for the "//trim(lattice)//" lattice ==")
   !===========================================================================  
   !
   call k_special()
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 live_timing_is_on=.true.
 !
1 continue
 !
 ! CLEAN
 !
 call bz_samp_reset(USER_K)
 call bz_samp_reset(Q_user)
 call k_ibz2bz(q,'d',.false.)
 call k_ibz2bz(Xk,'d',.false.)
 !
 !
 contains
   !
   subroutine print_the_grid(k,nk,wk,coo_in_,formatted_,PW_format)
     !
     use stderr,     ONLY:f_format_length,intc
     !
     ! Whatever coo_in this routine prints the list of points using coo_out
     !
     integer         ::nk,i1_
     real(SP)        ::k(nk,3),wk(nk),nk_replica
     character(*)    ::coo_in_
     logical         ::formatted_,PW_format
     !
     live_timing_is_on=.false.
     do i1_=1,nk
       !
       if (trim(coo_in_)=="rlu") call c2a(v_in=k(i1_,:),mode="ka2i")
       if (trim(coo_in_)=="cc")  call c2a(v_in=k(i1_,:),mode="kc2i")
       !
       v=k(i1_,:)
       nk_replica=wk(i1_)
       !
       call K_transform(v,'iku')
       !
       if (formatted_) then
         if(No_Weight) then
           write(ch,'(3(f12.'//trim(intc(f_format_length))//',1x,a,1x))') v(1),'|',v(2),'|',v(3)
         else
           write(ch,'(4(f12.'//trim(intc(f_format_length))//',1x,a,1x))') v(1),'|',v(2),'|',v(3),'|',nk_replica
         endif
       else if (PW_format) then
           write(ch,'(3(f12.'//trim(intc(f_format_length))//',1x),i1)') v(:),1
       else
         if(No_Weight) then
           write(ch,'(3(f12.'//trim(intc(f_format_length))//',1x))') v(:)
         else
           write(ch,'(4(f12.'//trim(intc(f_format_length))//',1x))') v(:),nk_replica
         endif
       endif
       !
       call msg("s",'      ',trim(ch))
       !
     enddo
     live_timing_is_on=.true.
     !
   end subroutine 
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
         do i2=1,USER_K%nibz
           call c2a(v_in=USER_K%pt(i2,:),v_out=v,mode="ki2a")
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
     YAMBO_ALLOC(GWK_table,(USER_K%nibz))
     !
     live_timing_is_on=.false.
     call msg("s","       ---------- Reduced K-grid ----------")
     GWK_table=0
     do i2=0,USER_K%nibz
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
