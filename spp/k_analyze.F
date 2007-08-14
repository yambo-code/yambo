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
 subroutine k_analize(en,k,Xk,q)
 !
 use pars,                ONLY:SP,schlen
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,rl_sop,nXkibz
 use D_lattice,           ONLY:nsym
 use com,                 ONLY:msg,warning
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a,rlu_v_is_zero,v_is_zero,v_norm
 use SPP,                 ONLY:coo_in,coo_out,qlong
 use timing,              ONLY:live_timing_is_on
 use zeros,               ONLY:k_iku_zero,k_rlu_zero
 !
 implicit none
 type(levels) ::en       
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 type(bz_samp)::GWK,Final_K,Q_user
 real(SP)     ::real_ctl,v(3),ka(3),q0(3)
 integer      ::i1,i2,is,ik
 real(SP):: kout(Xk%nibz,3)
 integer,  allocatable :: int_ctl(:)
 character(schlen)     :: ch
 !
 call bz_samp_reset(GWK)
 call bz_samp_reset(Q_user)
 call bz_samp_reset(Final_K)
 !
 if (len_trim(coo_in)==0) return
 !
 !Q/Xk (IBZ->BZ)
 !
 call k_ibz2bz(q,'i',.false.)
 call k_ibz2bz(Xk,'i',.false.)
 call msg("s",'Q-points  (BZ):',q%nbz)
 !
 call section('*',"== Built-in K-grid ==")
 !=========================================
 live_timing_is_on=.false.
 do i1=1,Xk%nibz
   if (trim(coo_out)=="rlu") then
     call c2a(v_in=Xk%pt(i1,:),v_out=v,mode="ki2a")
   else if (trim(coo_out)=="iku") then
     v=Xk%pt(i1,:)
   endif
   write(ch,'(3(f12.7,1x))') v
   call msg("s",'      ',trim(ch))
 enddo
 live_timing_is_on=.true.
 !
 call section('*',"== GW K-grid analyzer ==")
 !===========================================
 !
 !Input file parsing
 !
 real_ctl=0.
 GWK%nibz=1
 gw_main_loop: do while(real_ctl/=999.)
   if (associated(GWK%pt)) deallocate(GWK%pt)
   allocate(GWK%pt(GWK%nibz,3))
   GWK%pt(GWK%nibz,:)=(/0.,0.,999./)
   call parser('GWKpts',GWK%pt)
   real_ctl=GWK%pt(GWK%nibz,3)
   if (real_ctl/=999.) then
     !
     do i1=1,Xk%nbz
       v=Xk%ptbz(i1,:)
       if (trim(coo_in)=="cc")  call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2c")
       if (trim(coo_in)=="rlu") call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2a")
       if (v_is_zero(v(:)-GWK%pt(GWK%nibz,:),zero_=k_iku_zero)) then
         call warning(' Point already in the grid')
         exit gw_main_loop
       endif
     enddo
     !
     GWK%nibz=GWK%nibz+1
     !
   endif
 enddo gw_main_loop
 GWK%nibz=GWK%nibz-1
 call msg("s",'User K-points :',GWK%nibz)
 !
 ! Extended grid
 !
 allocate(Final_K%ptbz(Xk%nibz+GWK%nibz*q%nbz,3))
 Final_K%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
 Final_K%nbz=Xk%nibz
 do i1=1,GWK%nibz
   v=GWK%pt(i1,:)
   if (trim(coo_in)=="cc")  call c2a(v_in=GWK%pt(i1,:),v_out=v,mode="kc2i")
   if (trim(coo_in)=="rlu") call c2a(v_in=GWK%pt(i1,:),v_out=v,mode="ka2i")
   do i2=1,q%nbz
     Final_K%nbz=Final_K%nbz+1
     Final_K%ptbz(Final_K%nbz,:)=v(:)-q%ptbz(i2,:)
   enddo
 enddo
 !
 if (GWK%nibz>0) call reduce_and_print(Final_K,.TRUE.)
 !
 call bz_samp_reset(Final_K)
 !
 call section('*',"== Q-grid analyzer ==")
 !========================================
 !
 !Input file parsing
 !
 real_ctl=0.
 Q_user%nibz=1
 q_main_loop: do while(real_ctl/=999.)
   if (associated(Q_user%pt)) deallocate(Q_user%pt)
   allocate(Q_user%pt(Q_user%nibz,3))
   Q_user%pt(Q_user%nibz,:)=(/0.,0.,999./)
   call parser('Qpts',Q_user%pt)
   real_ctl=Q_user%pt(Q_user%nibz,3)
   if (real_ctl/=999.) then
     !
     do i1=1,q%nbz
       v=q%ptbz(i1,:)
       if (trim(coo_in)=="cc")  call c2a(v_in=q%ptbz(i1,:),v_out=v,mode="ki2c")
       if (trim(coo_in)=="rlu") call c2a(v_in=q%ptbz(i1,:),v_out=v,mode="ki2a")
       if (v_is_zero(v(:)-Q_user%pt(Q_user%nibz,:),zero_=k_iku_zero)) then
         call warning(' Point already in the grid')
         exit q_main_loop
       endif
     enddo
     !
     Q_user%nibz=Q_user%nibz+1
     !
   endif
 enddo q_main_loop
 Q_user%nibz=Q_user%nibz-1
 !
 call msg("s",'User Q-points :',Q_user%nibz)
 !
 ! Extended grid
 !
 allocate(Final_K%ptbz(Xk%nibz+Xk%nbz*Q_user%nibz,3))
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
 ! Generate shifted k-points set for longitudinal gauge calculation
 !
 call section('*',"== Longitudinal gauge grids generator ==")
 !===========================================================
 !
 call parser('KShift1',qlong(1,:))
 call parser('KShift2',qlong(2,:))
 call parser('KShift3',qlong(3,:))
 !
 if(any(abs(qlong)/=0.)) then
   !
   live_timing_is_on=.false.
   kout(:,:) = Xk%pt(:,:)
   write(ch,'(a,a,a)') " -- Unshifted IBZ K-grid [",trim(coo_out),"] -----"
   call msg("s",trim(ch))
   !
   ! Convert IBZ mesh to coo_out
   !
   do ik=1,Xk%nibz
     if (trim(coo_out)=="cc") call c2a(v_in=kout(ik,:),mode="ki2c")
     if (trim(coo_out)=="rlu") call c2a(v_in=kout(ik,:),mode="ki2a")
     write(ch,'(3f12.7)') kout(ik,:)
     call msg("s",trim(ch))
   enddo
   i2=0
   do i1=1,3
     q0 = qlong(i1,:)
     !
     if (v_norm(q0)>1.E-5) q0 = qlong(i1,:)*1.E-5/v_norm(q0)
     !
     if(v_norm(q0).gt.1e-6) then
       i2 = i2 + 1
       !
       ! Convert input q0 to iku
       !
       if (trim(coo_in)=="cc")  call c2a(v_in=q0(:),mode="kc2i")
       if (trim(coo_in)=="rlu") call c2a(v_in=q0(:),mode="ka2i")
       !
       ! Convert q0[iku] to coo_out
       !
       if (trim(coo_out)=="cc")  call c2a(v_in=q0(:),mode="ki2c")
       if (trim(coo_out)=="rlu") call c2a(v_in=q0(:),mode="ki2a")
       !
       ! Print shift vector in new basis
       !
       write(ch,'(a,i1,a,3(f8.5,a),2a)') " -- Shifted K-grid[",i2,&
&      "]: {K} + (",q0(1),",",q0(2),",",q0(3),") [",trim(coo_out),"]"
       call msg("s",trim(ch))
       !
       ! Print the shifted mesh
       !
       do ik=1,Xk%nibz
         write(ch,'(3f12.7)') kout(ik,:)+q0(:)
         call msg("s",trim(ch))
       enddo
     endif
   enddo
   !
 endif
 live_timing_is_on=.true.
 !
 ! CLEAN
 !
 call bz_samp_reset(GWK)
 call bz_samp_reset(Q_user)
 call k_ibz2bz(q,'d',.false.)
 call k_ibz2bz(Xk,'d',.false.)
 !
 !
 contains
   !
   subroutine reduce_and_print(K_grid,GW_grid)
     !
     type(bz_samp)::K_grid
     logical      ::GW_grid
     !
     ! Work Space
     !
     real(SP), allocatable :: GWK_table(:)
     !
     call msg("s","Reducing & Expanding the extended grid...")
     call k_reduce(K_grid)
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
     allocate(int_ctl(K_grid%nibz))
     int_ctl=0
     !
     ! int_ctl =0 -> nothing
     ! int_ctl/=0 -> GW (index)
     !
     if (GW_grid) then
       do i1=1,K_grid%nibz
         call c2a(v_in=K_grid%pt(i1,:),v_out=v,mode='ki2a')
         do i2=1,GWK%nibz
           do is=1,nsym
             call c2a(v_in= matmul(rl_sop(:,:,is),GWK%pt(i2,:)) ,v_out=ka,mode='ki2a')
             if (rlu_v_is_zero(v(:)-ka(:),zero_=k_rlu_zero)) then
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
     allocate(GWK_table(GWK%nibz))
     !
     live_timing_is_on=.false.
     call msg("s","       ---------- Reduced K-grid ----------")
     GWK_table=0
     do i2=0,GWK%nibz
       do i1=1,K_grid%nibz
         if (int_ctl(i1)/=i2.and..not.( i2==0.and.int_ctl(i1)/=0.and.i1<nXkibz )) cycle
         if (int_ctl(i1)/=0) then
            if(GWK_table(int_ctl(i1))>0) cycle
            GWK_table(int_ctl(i1))=1
         endif
         if (trim(coo_out)=="cc") call c2a(v_in=K_grid%pt(i1,:),mode="ki2c")
         if (trim(coo_out)=="rlu") call c2a(v_in=K_grid%pt(i1,:),mode="ki2a")
         if (int_ctl(i1)==0) write (ch,'(4f12.7)') K_grid%pt(i1,:),K_grid%weights(i1)
         if (int_ctl(i1)/=0) write (ch,'(4f12.7,i3)') K_grid%pt(i1,:),&
&                                                     K_grid%weights(i1),int_ctl(i1)
         call msg("s",trim(ch))
       enddo
     enddo
     live_timing_is_on=.true.
     !
     deallocate(GWK_table)
     deallocate(int_ctl)
     !
   end subroutine
   !
 end subroutine
