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
!===============================
 use pars,                ONLY:SP,schlen
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,rl_sop,nXkibz
 use D_lattice,           ONLY:nsym
 use com,                 ONLY:msg
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:c2a,rlu_v_is_zero,v_is_zero
 use SPP,                 ONLY:Kcoo_in,Kcoo_out
 use timing,              ONLY:live_timing_is_on
 implicit none
 type(levels) ::en       
 type(bz_samp)::k,Xk,q
!ws
 type(bz_samp)::GWK,GWK_Q
 real(SP)     ::real_ctl,v(3),ka(3)
 integer      ::i1,i2,is
 real(SP), allocatable :: GWK_table(:)
 integer,  allocatable :: int_ctl(:)
 character(schlen)     :: ch

!----------------------------------------------------------------
 call bz_samp_reset(GWK)
 call bz_samp_reset(GWK_Q)
!
 if (len_trim(Kcoo_in)==0) return
!
 call section('*',"== GW K-grid analyzer ==")
!
!Q/Xk (IBZ->BZ)
!--------------
 call k_ibz2bz(q,'i',.false.)
 call k_ibz2bz(Xk,'i',.false.)
 call msg("s",'Q-points  (BZ):',q%nbz)
!
!Input file parsing
!-------------------
 real_ctl=0.
 GWK%nibz=1
 main_loop: do while(real_ctl/=999.)
   if (associated(GWK%pt)) deallocate(GWK%pt)
   allocate(GWK%pt(GWK%nibz,3))
   GWK%pt(GWK%nibz,:)=(/0.,0.,999./)
   call parser('GWKpts',GWK%pt)
   real_ctl=GWK%pt(GWK%nibz,3)
   if (real_ctl/=999.) then
     do i1=1,Xk%nbz
       v=Xk%ptbz(i1,:)
       if (trim(Kcoo_in)=="cc")  call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2c")
       if (trim(Kcoo_in)=="rlu") call c2a(v_in=Xk%ptbz(i1,:),v_out=v,mode="ki2a")
       if (v_is_zero(v(:)-GWK%pt(GWK%nibz,:))) exit main_loop
     enddo
     GWK%nibz=GWK%nibz+1
   endif
 enddo main_loop
 GWK%nibz=GWK%nibz-1
 allocate(GWK_table(GWK%nibz))
 call msg("s",'Read K-points :',GWK%nibz)
!
!Extended grid
!-------------
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
 call msg("s","Reducing the extended grid...")
 call k_reduce(GWK_Q)
 call msg("l","done")
 call msg("s","Reduced K-grid points:",GWK_Q%nibz)
!
! the GWK_Q contains the k+(k-q) grids.
! Before reporting the points I wanto to sign the 
! position in the final grid of the given QP k-points (read from the input file)
!
 allocate(int_ctl(GWK_Q%nibz))
 int_ctl=0
!int_ctl =0 -> nothing
!int_ctl/=0 -> GW (index)
 do i1=1,GWK_Q%nibz
   call c2a(v_in=GWK_Q%pt(i1,:),v_out=v,mode='ki2a')
   do i2=1,GWK%nibz
     do is=1,nsym
       call c2a(v_in= matmul(rl_sop(:,:,is),GWK%pt(i2,:)) ,v_out=ka,mode='ki2a')
       if (rlu_v_is_zero(v(:)-ka(:))) then
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
     if (int_ctl(i1)==0) write (ch,'(3f10.4)') GWK_Q%pt(i1,:)
     if (int_ctl(i1)/=0) write (ch,'(3f10.4,i3)') GWK_Q%pt(i1,:),int_ctl(i1)
     call msg("s",trim(ch))
   enddo
 enddo
 live_timing_is_on=.true.

!
! CLEAN
!-------
 call bz_samp_reset(GWK)
 call bz_samp_reset(GWK_Q)
 call k_ibz2bz(q,'d',.false.)
 call k_ibz2bz(Xk,'d',.false.)
 deallocate(GWK_table)
 deallocate(int_ctl)

 end subroutine
!
! Database of high-Simmetry K-points (to handle somehow)
!
! fcc [iku] :
!
!   0.00000    0.00000    0.00000 G 
!   1.00000    0.00000    0.00000 X
!   0.50000    0.50000    0.50000 L
!   0.75000    0.75000    0.00000 K
!   1.00000    0.50000    0.00000 W
!   1.00000    0.25000    0.25000 U
!
! fcc [rlu]:
!
!   0.00000    0.00000    0.00000
!   0.00000    0.50000    0.50000
!   0.50000    0.50000    0.50000
!   0.37500    0.37500    0.75000
!   0.25000    0.50000    0.75000
!   0.25000    0.62500    0.62500
!
! graphene and others .. [rlu]
!
! K     2/3 1/3 0
! M	1/2 1/2 0 
! L	1/2 1/2 1/2
! H	1/2 1/3 1/2
! A	0 0 1/2 
!
