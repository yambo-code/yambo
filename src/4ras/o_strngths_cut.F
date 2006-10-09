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
 subroutine ostrngths_cut(Xen, Xk, X)
!====================================
 use pars
 use com,                     ONLY : msg
 use timing,                  ONLY : live_timing
 use electrons,               ONLY : levels, n_spin
 use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                    master_node, pp_indexes_reset
 use par_indexes_m,           ONLY : par_indexes
 use D_lattice,               ONLY : nsym, i_time_rev, alat, dl_sop, sop_inv
 use R_lattice,               ONLY : g_vec, bz_samp
 use X_m,                     ONLY : X_alloc, X_t
 use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP
 use memory_m,                ONLY : mem_est
 use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load, WF_free
 use optcut,                  ONLY : Xcut_q0grad, Xcut_rhoq0, ng_limits, setup_optcut, pscut
 use surface_geometry,        ONLY : setup_gvecaff
 implicit none
 type(bz_samp),  intent(in)       :: Xk
 type(levels),   intent(in)       :: Xen
 type(X_t)                        :: X
!ws
 integer                          :: io_db, io_err, i_spin
 integer                          :: ik, i1, icfft, ivfft, ic, iv, is

 real(SP)                         :: omega, q0s(3)
 type(pp_indexes)                 :: px
 real(SP), allocatable            :: kg(:,:)
 complex(SP)                      :: PS(3)
 character(schlen)                :: sch ! DEBUG
!functions
 integer,  external               :: ioOSTNTS_cut

 call section('=','Optical oscillators with cut off')

 if (allocated(Xcut_rhoq0)) return
 
 X%ngostnts = wf_ng
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=io_db)
 io_err = ioOSTNTS_cut(X, Xen, io_db)
 if (io_err==0) goto 1
!
 call pp_indexes_reset(px)
!
! WF loading
!============
!
 
 call wf_load(0, 1, X%ib, (/1,Xk%nibz/), space='G', title='-Oscillators/G space')

 allocate( kg(3,wf_ng) ) 
!
! OptGrad cut allocation
!====================
 allocate(Xcut_q0grad(3, X%ib(2), Xen%nbm, Xk%nibz, n_spin))
 call mem_est('Xcut_q0grad', (/product( (/3, X%ib(2), Xen%nbm, Xk%nibz, n_spin/) )/) )
 Xcut_q0grad = (0.,0.)

 call par_indexes(px, (/ Xk%nibz, Xen%nbm /),(/ 1, X%ib(1) /))

! ppios%mode='orh';ppios%verbose=1
! call io(pp,ppios)
! if (pp%ns/=0) X%Vnl_included=.true.
! call nlPPalloc(1)

 call live_timing('Optical strengths-cut',px%stps(myid+1))
 call pp_redux_wait

 call setup_gvecaff
 call nG_limits( Xk%nibz )


 do ik = 1, Xk%nibz

!--> DEBUG
!   write(sch,'(a,i4,a,i4)') 'Loop over k oscillators: ',ik,'/',Xk%nibz
!   call msg('ns',trim(sch))
!   write(*,*) trim(sch)
!<-- DEBUG

   do i1 = 1,3
     kg(i1,:) = ( Xk%pt(ik,i1) + g_vec(1:wf_ng, i1) ) * 2. * pi/alat(i1) ! -i grad Wf
   enddo
   call setup_optcut( ik )

   do iv = X%ib(1), Xen%nbm
     if (.not.px%i2p(ik,iv)) cycle
     do i_spin   = 1, n_spin
       ivfft=wf_state(iv,ik,i_spin)
       do ic = Xen%nbf+1, X%ib(2)
         omega = Xen%E(iv,ik,i_spin)-Xen%E(ic,ik,i_spin)

         if (associated(Xen%Eo)) omega=Xen%Eo(iv,ik,i_spin)-Xen%Eo(ic,ik,i_spin)
           if (any((/-omega<X%ehe(1).and.X%ehe(1)>0.,-omega>X%ehe(2).and.X%ehe(2)>0./))) cycle
         if (abs(omega)<=1.E-5) cycle
         icfft=wf_state(ic,ik,i_spin)

         call PScut(kg, PS, wf(:,ivfft), wf(:,icfft) )

          Xcut_q0grad(:,ic,iv,ik,i_spin) = PS(:)/omega
       enddo ! cond. bands
     enddo     ! spinors
     call live_timing(steps=1)
   enddo   ! valence bands
 enddo       ! k-points

 call live_timing
!call end_optcut
!
! CLEAN
!
 deallocate(kg)
 call WF_free()
 do i_spin = 1,n_spin
   call pp_redux_wait(Xcut_q0grad(:,:,:,:,i_spin))
 enddo

 if (master_node) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=io_db)
 io_err=ioOSTNTS_cut(X,Xen,io_db)

1 continue
! Note: this order is inefficient
 allocate(Xcut_rhoq0(X%ib(2),Xen%nbm,Xk%nbz,n_spin))
 call mem_est('Xcut_rhoq0',(/ product( (/X%ib(2),Xen%nbm,Xk%nbz,n_spin/) ) /) )
 Xcut_rhoq0=(0.,0.)
 do i1=1,Xk%nbz
   ik  = Xk%sstar(i1, 1)
   is  = sop_inv(Xk%sstar(i1, 2) )
   q0s = matmul( dl_sop(:,:,is), X%q0 )
   do i_spin = 1, n_spin
   do iv = X%ib(1), Xen%nbm
     do ic = Xen%nbf+1, X%ib(2)
       Xcut_rhoq0(ic,iv,i1,i_spin) = dot_product( q0s, Xcut_q0grad(:,ic,iv,ik,i_spin) ) ! CUT X
       if (is>nsym/(i_time_rev+1)) Xcut_rhoq0(ic,iv,i1,i_spin) = &
&                                  dot_product(Xcut_q0grad(:,ic,iv,ik,i_spin),q0s) 
     enddo
     Xcut_rhoq0(iv,iv,i1,i_spin)=(1.,0.)
   enddo
   enddo
 enddo
!
! CLEAN
 deallocate(Xcut_q0grad)
 call mem_est('Xcut_q0grad')

 end subroutine
