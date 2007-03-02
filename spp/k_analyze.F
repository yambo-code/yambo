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
 use com,                 ONLY:msg
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a,rlu_v_is_zero,v_is_zero,v_norm
 use SPP,                 ONLY:Kcoo_in,Kcoo_out,qlong
 use timing,              ONLY:live_timing_is_on
 use zeros,               ONLY:k_iku_zero,k_rlu_zero
 !
 implicit none
 type(levels) ::en       
 type(bz_samp)::k,Xk,q
 !
 ! Work Space
 !
 type(bz_samp)::GWK,GWK_Q
 real(SP)     ::real_ctl,v(3),ka(3),q0(3)
 integer      ::i1,i2,is,ik
 real(SP):: kout(Xk%nibz,3)
 real(SP), allocatable :: GWK_table(:)
 integer,  allocatable :: int_ctl(:)
 character(schlen)     :: ch
 !
 call bz_samp_reset(GWK)
 call bz_samp_reset(GWK_Q)
 !
 if (len_trim(Kcoo_in)==0) return
 !
 call section('*',"== GW K-grid analyzer ==")
 !
 !Q/Xk (IBZ->BZ)
 !
 call k_ibz2bz(q,'i',.false.)
 call k_ibz2bz(Xk,'i',.false.)
 call msg("s",'Q-points  (BZ):',q%nbz)
 !
 !Input file parsing
 !
 real_ctl=0.
 GWK%nibz=1
 main_loop: do while(real_ctl/=999.)
   if (associated(GWK%pt)) deallocate(GWK%pt)
   allocate(GWK%pt(GWK%nibz,3))
   GWK%pt(GWK%nibz,:)=(/0.,0.,999./)
   call parser('GWKpts',GWK%pt)
   real_ctl=GWK%pt(GWK%nibz,3)
   if (real_ctl/=999.) then
     !
     do i1=1,Xk%nbz
       v=Xk%ptbz(i1,:)
       if (trim(Kcoo_in)=="cc")  call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2c")
       if (trim(Kcoo_in)=="rlu") call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2a")
       if (v_is_zero(v(:)-GWK%pt(GWK%nibz,:),zero_=k_iku_zero)) exit main_loop
     enddo
     !
     GWK%nibz=GWK%nibz+1
     !
   endif
 enddo main_loop
 GWK%nibz=GWK%nibz-1
 allocate(GWK_table(GWK%nibz))
 call msg("s",'Read K-points :',GWK%nibz)
 !
 ! Extended grid
 !
 allocate(GWK_Q%ptbz(Xk%nibz+GWK%nibz*q%nbz,3))
 GWK_Q%ptbz(:Xk%nibz,:)=k%pt(:Xk%nibz,:)
 GWK_Q%nbz=Xk%nibz
 do i1=1,GWK%nibz
   v=GWK%pt(i1,:)
   if (trim(Kcoo_in)=="cc")  call c2a(v_in=GWK%pt(i1,:),v_out=v,mode="kc2i")
   if (trim(Kcoo_in)=="rlu") call c2a(v_in=GWK%pt(i1,:),v_out=v,mode="ka2i")
   do i2=1,q%nbz
     GWK_Q%nbz=GWK_Q%nbz+1
     GWK_Q%ptbz(GWK_Q%nbz,:)=v(:)-q%ptbz(i2,:)
   enddo
 enddo
 call msg("s","Reducing & Expanding the extended grid...")
 call k_reduce(GWK_Q)
 call k_expand(GWK_Q)
 !
 call msg("l","done")
 call msg("s","Reduced K-grid points:",GWK_Q%nibz)
 !
 ! the GWK_Q contains the k+(k-q) grids.
 ! Before reporting the points I want to sign the 
 ! position in the final grid of the given QP k-points (read from the input file)
 !
 allocate(int_ctl(GWK_Q%nibz))
 int_ctl=0
 !
 ! int_ctl =0 -> nothing
 ! int_ctl/=0 -> GW (index)
 !
 do i1=1,GWK_Q%nibz
   call c2a(v_in=GWK_Q%pt(i1,:),v_out=v,mode='ki2a')
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
 !
 live_timing_is_on=.false.
 call msg("s"," ========== Reduced K-grid ==========")
 GWK_table=0
 do i2=0,GWK%nibz
   do i1=1,GWK_Q%nibz
     if (int_ctl(i1)/=i2.and..not.( i2==0.and.int_ctl(i1)/=0.and.i1<nXkibz )) cycle
     if (int_ctl(i1)/=0) then
        if(GWK_table(int_ctl(i1))>0) cycle
        GWK_table(int_ctl(i1))=1
     endif
     if (trim(Kcoo_out)=="cc") call c2a(v_in=GWK_Q%pt(i1,:),mode="ki2c")
     if (trim(Kcoo_out)=="rlu") call c2a(v_in=GWK_Q%pt(i1,:),mode="ki2a")
     if (int_ctl(i1)==0) write (ch,'(4f14.6)') GWK_Q%pt(i1,:),GWK_Q%weights(i1)
     if (int_ctl(i1)/=0) write (ch,'(4f14.6,i3)') GWK_Q%pt(i1,:),GWK_Q%weights(i1),int_ctl(i1)
     call msg("s",trim(ch))
   enddo
 enddo
 live_timing_is_on=.true.
 !
 ! Generate shifted k-points set for longitudinal gauge calculation
 !
 call parser('KShift1',qlong(1,:))
 call parser('KShift2',qlong(2,:))
 call parser('KShift3',qlong(3,:))
 !
 if(any(abs(qlong).gt.1e-6)) then
   live_timing_is_on=.false.
   kout(:,:) = Xk%pt(:,:)
   write(ch,'(a,a,a)') " == Unshifted IBZ K-grid [",trim(Kcoo_out),"] ====="
   call msg("s",trim(ch))
   !
   ! Convert IBZ mesh to Kcoo_out
   !
   do ik=1,Xk%nibz
     if (trim(Kcoo_out)=="cc") call c2a(v_in=kout(ik,:),mode="ki2c")
     if (trim(Kcoo_out)=="rlu") call c2a(v_in=kout(ik,:),mode="ki2a")
     write(ch,'(3f12.7)') kout(ik,:)
     call msg("s",trim(ch))
   enddo
   i2=0
   do i1=1,3
     q0(1:3) = qlong(i1,1:3)
     if(v_norm(q0).gt.1e-6) then
       i2 = i2 + 1
       !
       ! Convert input q0 to iku
       !
       if (trim(Kcoo_in)=="cc")  call c2a(v_in=q0(:),mode="kc2i")
       if (trim(Kcoo_in)=="rlu") call c2a(v_in=q0(:),mode="ka2i")
       !
       ! Convert q0[iku] to Kcoo_out
       !
       if (trim(Kcoo_out)=="cc")  call c2a(v_in=q0(:),mode="ki2c")
       if (trim(Kcoo_out)=="rlu") call c2a(v_in=q0(:),mode="ki2a")
       !
       ! Print shift vector in new basis
       !
       write(ch,'(a,i1,a,3(f8.5,a),2a)') " == Shifted K-grid[",i2,&
&      "]: {K} + (",q0(1),",",q0(2),",",q0(3),") [",trim(Kcoo_out),"]"
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
 call bz_samp_reset(GWK_Q)
 call k_ibz2bz(q,'d',.false.)
 call k_ibz2bz(Xk,'d',.false.)
 deallocate(GWK_table)
 deallocate(int_ctl)
 !
 end subroutine
