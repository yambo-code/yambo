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
 subroutine X_O_strengths(Xen,Xk,X,field_dir)
!============================================
 use pars,          ONLY:SP,pi
 use timing,        ONLY:live_timing
 use electrons,     ONLY:levels,n_spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,master_node,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use D_lattice,     ONLY:nsym,i_time_rev,alat,dl_sop,sop_inv,pp_kbv_dim,&
&                        n_atom_species,n_atoms,pp_n_l_comp,PP_free
 use R_lattice,     ONLY:g_vec,bz_samp
 use X_m,           ONLY:X_rhoq0,X_alloc,X_t,X_q0grad
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP,OP_RD,RD_CL,RD
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:wf,wf_ng,wf_state,wf_load,WF_free
 implicit none
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 type(X_t)     :: X
 real(SP)      :: field_dir(3)
!ws
 integer :: ik,i1,icfft,ivfft,ic,iv,is,ia,il,im,alloc_err,i_spin
 complex(SP), allocatable :: wf_xyz(:,:),kbv(:,:,:)
 complex(SP)              :: rho(3)
#if defined DP
 complex(DP):: zdotc
#else 
 complex(SP):: cdotc
#endif
 real(SP) :: omega,field_dir_rot(3)
 type(pp_indexes)::px
!
!I/O
!
 integer :: ID,io_err,ioKB_err
 integer, external :: ioOSTNTS,ioKB_PP
!
 if (allocated(X_rhoq0)) return
!
! Check first if DB is already done
!===================================
 X%ngostnts=wf_ng
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioOSTNTS(X,Xen,ID)
 if (io_err==0) goto 1
!
! Check not the KB PP DB 
!===================================
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 ioKB_err=ioKB_PP(ID)
 X%Vnl_included=.false.
 if (ioKB_err==0) then
   X%Vnl_included=.true.
   pp_kbv_dim=0
   do is = 1,n_atom_species
     do ia = 1,n_atoms(is)
       do il = 1, pp_n_l_comp(is)
         do im = 1,2*(il-1)+1
           pp_kbv_dim=pp_kbv_dim+1
         enddo
       enddo
     enddo
   enddo
   allocate(kbv(wf_ng,pp_kbv_dim,4),stat=alloc_err)
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 endif
!
 call pp_indexes_reset(px)
!
! WF loading
!============
!
 call wf_load(0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space')
 allocate(wf_xyz(wf_ng,3))
!
! OptGrad allocation
!====================
 call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))
 X_q0grad=(0.,0.)

 call par_indexes(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
 call live_timing('Optical strengths',px%stps(myid+1))
 call pp_redux_wait
 do ik=1,Xk%nibz
!
! [Vnl,r]
!
   if (ioKB_err==0) then
     if (ik< Xk%nibz) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_err=ioKB_PP(ID)
     call X_O_kb_pp_comp(ik,Xk,kbv)
   endif

   do iv=X%ib(1),Xen%nbm
     if (.not.px%i2p(ik,iv)) cycle

     do i_spin=1,n_spin

       ivfft=wf_state(iv,ik,i_spin)
!
! wf_xyz = -i grad fdo(iv,ik)
!
       do i1=1,3
         wf_xyz(:,i1)=wf(:,ivfft)*(Xk%pt(ik,i1)+g_vec(:wf_ng,i1))*2.*pi/alat(i1)
       enddo
       do ic=Xen%nbf+1,X%ib(2)
         omega=Xen%E(iv,ik,i_spin)-Xen%E(ic,ik,i_spin)
         if (associated(Xen%Eo)) omega=Xen%Eo(iv,ik,i_spin)-Xen%Eo(ic,ik,i_spin)
         if (any((/-omega<X%ehe(1).and.X%ehe(1)>0.,-omega>X%ehe(2).and.X%ehe(2)>0./))) cycle
         if (abs(omega)<=1.E-5) cycle
         icfft=wf_state(ic,ik,i_spin)
!
! rho = wf_xyz^* fdo(ic,ik)= i < fdo(iv,ik)| grad |  fdo(ic,ik) >
!
#if defined DP
         rho(1)=zdotc(wf_ng,wf_xyz(:,1),1,wf(:,icfft),1)
         rho(2)=zdotc(wf_ng,wf_xyz(:,2),1,wf(:,icfft),1)
         rho(3)=zdotc(wf_ng,wf_xyz(:,3),1,wf(:,icfft),1)
#else
         rho(1)=cdotc(wf_ng,wf_xyz(:,1),1,wf(:,icfft),1)
         rho(2)=cdotc(wf_ng,wf_xyz(:,2),1,wf(:,icfft),1)
         rho(3)=cdotc(wf_ng,wf_xyz(:,3),1,wf(:,icfft),1)
#endif
         if (ioKB_err==0) call X_O_kbsum(ivfft,icfft,rho,kbv)
!
!     X_q0grad = i <v | p |c> / (Ev-Ec) = i < v k | r | c k >
!
         X_q0grad(:,ic,iv,ik,i_spin)=rho(:)/omega
   
       enddo

     enddo
     call live_timing(steps=1)
   enddo
 enddo
 call live_timing
!
! CLEAN
!
 deallocate(wf_xyz)
 call WF_free()
 if (ioKB_err==0) then
   deallocate(kbv)
   call mem_est("KBV")
   call PP_free()
 endif 
 do i_spin=1,n_spin
   call pp_redux_wait(X_q0grad(:,:,:,:,i_spin))
 enddo

 if (master_node) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=ioOSTNTS(X,Xen,ID)

1 call X_alloc('OptOsc',(/X%ib(2),Xen%nbm,Xk%nbz/))
 X_rhoq0=(0.,0.)
 do i1=1,Xk%nbz
   ik=Xk%sstar(i1,1)
   is=sop_inv(Xk%sstar(i1,2))
   field_dir_rot=matmul(dl_sop(:,:,is),field_dir)
   do iv=X%ib(1),Xen%nbm
     do ic=Xen%nbf+1,X%ib(2)
       do i_spin=1,n_spin
!
!     X_rhoq0 = i q . < v k | r | c k >
!
         X_rhoq0(ic,iv,i1,i_spin)=&
&          dot_product(field_dir_rot,X_q0grad(:,ic,iv,ik,i_spin))
         if (is>nsym/(i_time_rev+1)) X_rhoq0(ic,iv,i1,i_spin)=&
&          dot_product(X_q0grad(:,ic,iv,ik,i_spin),field_dir_rot) 
       enddo
     enddo
     X_rhoq0(iv,iv,i1,:)=(1.,0.)
   enddo
 enddo
!
! CLEAN
 call X_alloc('OptGrad')
 call pp_indexes_reset(px)

 end subroutine
