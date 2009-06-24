!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine K_doubles(Ken,BS_W)
 !
 ! This subroutine computes and write to disk in ndb.BS_C_doubles
 ! the coupling part of the second order kernel:
 ! C^{i}_{sd} with s={c,v} ; d={c1,c2,v1,v2} ; i=1,8
 ! see comments around lines 380 for a more detailed description of C
 !
 ! The subroutine K_doubles_solvers.F will read this file and pass the
 ! matrix BS_mat_doubles to the subroutine BS_doubles_mount.F which will
 ! compute the second order kernel for a specific frequency
 !
 use pars,          ONLY:SP,pi
 use drivers,       ONLY:l_td_hf
 use memory_m,      ONLY:mem_est
 use electrons,     ONLY:levels,n_sp_pol,spin_occ,spin
 use wave_func,     ONLY:wf_load,WF_free
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use R_lattice,     ONLY:bare_qpg
 use com,           ONLY:msg,error
 use timing,        ONLY:live_timing
 use BS,            ONLY:BS_bands,BS_eh_E,BS_W_is_diagonal,&
&                        BS_n_g_W,BS_blk_dim,O_phase,&
&                        BS_eh_table,BS_identifier,BS_K_dim,BS_mat
 use BS_doubles,    ONLY:BS_mat_doubles,BS_res_K_doubles,BS_K_doubles_dim,  &
&                        BS_doubles_table,BS_bands_doubles,BS_doubles_E,    &
&                        static_approx,&
! DOUBLES CORR PROJECT
&                        BS_mat_tmp,Eigen_doubles,doubles_corr
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use X_m,           ONLY:DIP_iq_dot_r
 use collision,     ONLY:ggwinfo,collision_reset
 use IO_m,          ONLY:io_control,OP_RD_CL,REP,VERIFY,OP_WR_CL
 !
 implicit none
 type(levels)     ::Ken  
 type(pp_indexes) ::px
 !
 ! Main loop variables
 !
 integer    :: iv1,ic1,iv2,ic2,iv3,ic3,icv1,icv2,icv3,icin,&
&              iOvv,iOcc,i1,i2,i3,ncin,&
&              icv,ic,iv,nc,nv,nv_doubles,nc_doubles,i_doubles
 complex(SP),allocatable :: Interaction_cccv(:,:,:,:),Interaction_vvcv(:,:,:,:)
 ! DOUBLES CORR PROJECT
 integer    ,allocatable :: BS_eh_table_m1(:,:)
 integer                 :: lib_driver
 real(SP)   ,allocatable :: BS_E_tmp(:)
 !
 ! From lapac libraries, functions for the scalar product of two vectors
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 !
 ! Elements for the construction of the Kernel:
 ! 1 - interaction
 complex(SP) ::BS_W(BS_n_g_W,1,1)
 type(ggwinfo)          ::isc
 ! 2 - oscillators
 complex(SP),allocatable::Ovv(:,:,:),Occ(:,:,:),Ocv(:,:,:)
 !
 ! I/O
 integer           ::io_err,ID     
 integer, external ::ioBS_doubles  
 !
 ! Davide doubles
 ! preparation lines:
 BS_res_K_doubles=l_td_hf
! BS_res_K_doubles=.false.
 doubles_corr=.true.
! doubles_corr=.false.
 lib_driver=USE_LK
 BS_bands_doubles(1)=BS_bands(1)
 BS_bands_doubles(2)=BS_bands(2)
 !
 nc=BS_bands(2)-Ken%nbf
 nv=Ken%nbf-BS_bands(1)+1
 nc_doubles=BS_bands_doubles(2)-Ken%nbf
 nv_doubles=Ken%nbf-BS_bands_doubles(1)+1
 !
 BS_K_doubles_dim=(nc_doubles*nv_doubles)**2
 !
 ncin=2
 if(doubles_corr) ncin=1
 !
 if(.not.BS_res_K_doubles) return
 ! DEBUG <
 open(unit=3456, file='debug.txt', status='unknown')
 open(unit=3457, file='interaction_cccv.txt', status='unknown')
 open(unit=3458, file='interaction_vvcv.txt', status='unknown')
 open(unit=3459, file='BS_mat_doubles.txt', status='unknown')
 ! DEBUG >
 call section('*','Bethe-Salpeter Kernel + DOUBLES')
 !
 ! Check fo the dimension of the kernel
 if (BS_K_doubles_dim==0) then
   call error(' Null BSE doubles kernel block dimension(s) found. Increase doubles range')
 endif
 !
 ! Check for the doubles DB, if already present exit from the subroutine
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioBS_doubles(ID)
 if (io_err==0) then
   return
 endif
 !
 ! Start the computation of the database
 call msg('rsn','[BSE] Doubles dimension      :',BS_K_doubles_dim)
 !
 ! Load wave funtions
 call wf_load(BS_n_g_W,1,(/BS_bands_doubles(1),BS_bands_doubles(2)/),(/1,1/),&
&             space='R',title='-BSK_doubles')
 !
 ! *************************
 ! ****** Allocations ******
 ! *************************
 ! Allocation: Table and energies
 allocate(BS_doubles_table(BS_K_doubles_dim,4))
 if (doubles_corr) then
   allocate(BS_eh_table_m1(BS_bands_doubles(2),BS_bands_doubles(2)))
   allocate(BS_E_tmp(BS_K_dim))
   call mem_est('BS_E_tmp',(/size(BS_doubles_E)*2/))
   BS_eh_table_m1(:,:)=0
 endif
 BS_doubles_table(:,:)=0
 allocate(BS_doubles_E(BS_K_doubles_dim),static_approx(BS_K_doubles_dim))
 call mem_est('BS_doubles_E',(/size(BS_doubles_E)*2/))
 BS_doubles_E(:)=(0.,0.)
 static_approx(:)=0
 !
 ! Allocation: Oscillators
 allocate(Ovv(BS_n_g_W,nv_doubles,nv),Occ(BS_n_g_W,nc,nc_doubles),Ocv(BS_n_g_W,nc_doubles,nv_doubles))
 call mem_est('Occ Ovv Ocv',(/size(Occ)*2,size(Ovv)*2,size(Ocv)*2/))
 Occ(:,:,:)=(0.,0.)
 Ovv(:,:,:)=(0.,0.)
 Ocv(:,:,:)=(0.,0.)
 !
 ! Allocation: Interaction and BS_mat_doubles
 allocate(Interaction_cccv(nc,nc_doubles,nc_doubles,nv_doubles))
 allocate(Interaction_vvcv(nv_doubles,nv,nc_doubles,nv_doubles))
 call mem_est('Interaction_cccv',(/size(interaction_cccv)*2/))
 call mem_est('Interaction_vvcv',(/size(interaction_vvcv)*2/))
 Interaction_cccv(:,:,:,:)=(0.,0.)
 Interaction_vvcv(:,:,:,:)=(0.,0.)
 !
 ! ********************************
 ! ***** Before the main loop *****
 ! ********************************
 ! Doubles tables for indexes and energies
 i_doubles=0
 icv1=0
 do iv1=BS_bands_doubles(1),Ken%nbf
   do ic1=Ken%nbf+1,BS_bands_doubles(2)
     icv1=icv1+1
      if (doubles_corr) BS_eh_table_m1(iv1,ic1)=icv1
     do iv2=BS_bands_doubles(1),Ken%nbf
       do ic2=Ken%nbf+1,BS_bands_doubles(2)
         i_doubles=i_doubles+1
         BS_doubles_table(i_doubles,:)=(/iv1,iv2,ic1,ic2/)
         BS_doubles_E(i_doubles)=Ken%E(ic1,1,1)+Ken%E(ic2,1,1)-Ken%E(iv1,1,1)-Ken%E(iv2,1,1)
         ! STATIC APPROX <
         ! Check for BS_doubles_E. If greater than ?? eV then the corrisponding 
         ! index is recorded so that will be possible to do a static approximation
         ! for the kernel for that index
         !if ( BS_doubles_E(i_doubles)*27.2116> ?? ) then
         !  static_approx(i_doubles)=1
         !  write(3456,*) i_doubles
         !endif
         ! STATIC APPROX >
         ! DEBUG <
         !write(3456,*) ic1,ic2,iv1,iv2,BS_doubles_E(i_doubles)*27.2116
         ! DEBUG >
       enddo
     enddo
   enddo
 enddo
 !
 ! Oscillators construction
 ! 1 - preparations lines
 call collision_reset(isc)
 isc%is(:)=1
 isc%os(:)=1
 isc%qs(:)=1
 isc%ngrho=BS_n_g_W
 isc%iqref=1
 allocate(isc%rhotw(BS_n_g_W))
 allocate(isc%gamp(1,BS_n_g_W))
 !
 ! 2 - Ovv construction
 ! Paral <
 call pp_indexes_reset(px)
 call par_indexes(px,(/Ken%nbf,Ken%nbf/),(/BS_bands(1),BS_bands_doubles(1)/))
 call pp_redux_wait
 call live_timing('Ovv',px%stps(myid+1))
 ! Paral >
 !
 do iv=BS_bands(1),Ken%nbf
   do iv1=BS_bands_doubles(1),Ken%nbf
     ! Paral <
     if (.not.px%i2p(iv,iv1)) cycle
     call live_timing(steps=1)
     ! Paral >
     isc%is(1)=iv1
     isc%os(1)=iv
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Ovv(i1,iv1-BS_bands_doubles(1)+1,iv-BS_bands(1)+1)=isc%rhotw(i1)
     ! DEBUG <
     !write(3456,*) iv1,iv,Ovv(:,iv1,iv-BS_bands(1)+1)
     ! DEBUG >
   enddo
 enddo
 !
 ! Paral <
 call live_timing()
 call pp_redux_wait(Ovv)
 call pp_indexes_reset(px)
 !
 ! 3 -Occ construction
 call par_indexes(px,(/BS_bands(2),BS_bands_doubles(2)/),(/1+Ken%nbf,1+Ken%nbf/))
 call pp_redux_wait
 call live_timing('Occ',px%stps(myid+1))
 ! Paral >
 !
 do ic=1+Ken%nbf,BS_bands(2)
   do ic1=1+Ken%nbf,BS_bands_doubles(2)
     ! Paral <
     if (.not.px%i2p(ic,ic1)) cycle
     call live_timing(steps=1)
     ! Paral >
     isc%is(1)=ic
     isc%os(1)=ic1
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Occ(i1,ic-Ken%nbf,ic1-Ken%nbf)=isc%rhotw(i1)
     ! DEBUG <
     !write(3456,*) ic,ic1,Occ(:,ic-Ken%nbf,ic1-Ken%nbf)
     ! DEBUG >
   enddo
 enddo
 !
 ! Paral <
 call live_timing()
 call pp_redux_wait(Occ)
 call pp_indexes_reset(px)
 !
 ! 4 -Ocv construction
 call par_indexes(px,(/Ken%nbf,BS_bands_doubles(2)/),(/BS_bands_doubles(1),1+Ken%nbf/))
 call pp_redux_wait
 call live_timing('Ocv',px%stps(myid+1))
 ! Paral >
 !
 do iv2=BS_bands_doubles(1),Ken%nbf
   do ic2=1+Ken%nbf,BS_bands_doubles(2)
     ! Paral <
     if (.not.px%i2p(iv2,ic2)) cycle
     call live_timing(steps=1)
     ! Paral >
     isc%is(1)=ic2
     isc%os(1)=iv2
     call scatterBamp(isc)
     forall(i1=1:BS_n_g_W) Ocv(i1,ic2-Ken%nbf,iv2-BS_bands_doubles(1)+1)=isc%rhotw(i1)
     ! Warning, should evaluate the term  G=0 of Ocv but use this only in the 
     ! bubble diagrams when c=c' ; v=v'. Presently Ocv=0 for G=0
!     Ocv(1,ic2-Ken%nbf,iv2-BS_bands_doubles(1)+1)=-conjg(DIP_iq_dot_r(ic2,iv2,1,1))/bare_qpg(1,1)
     ! DEBUG <
     !write(3456,*) ic2,iv2,Ocv(:,ic2-Ken%nbf,iv2)
     ! DEBUG >
   enddo
 enddo
 !
 ! Paral <
 call live_timing()
 call pp_redux_wait(Ocv)
 call pp_indexes_reset(px)
 ! Paral >
 ! 
 call collision_reset(isc)
 !
 !  ********************************************
 !  *******       Main loop: start       *******
 !  ********************************************
 if (BS_res_K_doubles) then
   !
   !  Step 1: Here the code constructs U_{cc1,c2v2} and U_{v1v,c2v2} 
   !          starting Ocv, Occ and Ovv, and BS_W
   !
   !          1.a: U_{c c1 c2 v2} = Occ * BS_W * Ocv
   !
   ! Paral <
   call pp_indexes_reset(px)
   call par_indexes(px,(/nc,nc_doubles/),(/1,1/))
   call pp_redux_wait
   call live_timing('Interaction_cccv',px%stps(myid+1))
   ! Paral >
   !
   do ic=1,nc
     do ic1=1,nc_doubles
       !
       ! Paral <
       if (.not.px%i2p(ic,ic1)) cycle
       call live_timing(steps=1)
       ! Paral >
       if (BS_W_is_diagonal) then
         forall(i1=1:BS_n_g_W) Occ(i1,ic,ic1)=Occ(i1,ic,ic1)*BS_W(i1,1,1)          
       else
         !
         do i1=1,BS_n_g_W 
#if defined _DOUBLE
           Occ(i1,ic,ic1)=zdotu(BS_n_g_W,Occ(1,ic,ic1),1,BS_W(1,i1,1),1)  
#else
           Occ(i1,ic,ic1)=cdotu(BS_n_g_W,Occ(1,ic,ic1),1,BS_W(1,i1,1),1)
#endif
         enddo
       endif
       do iv2=1,nv_doubles
         do ic2=1,nc_doubles
           !
#if defined _DOUBLE
           Interaction_cccv(ic,ic1,ic2,iv2)=zdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Occ(1,ic,ic1),1)*4.*pi       
#else                                                         
           Interaction_cccv(ic,ic1,ic2,iv2)=cdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Occ(1,ic,ic1),1)*4.*pi       
#endif
           ! DEBUG <
           write(3457,*) ic+Ken%nbf,ic1+Ken%nbf,ic2+Ken%nbf,iv2+BS_bands_doubles(1)-1,&
&                        Interaction_cccv(ic,ic1,ic2,iv2)
           ! DEBUG >
         enddo
       enddo
     enddo
   enddo
   !
   ! Paral <
   call live_timing()
   call pp_redux_wait(Interaction_cccv)
   call pp_indexes_reset(px)
   !
   call par_indexes(px,(/nv,nv_doubles/),(/1,1/))
   call pp_redux_wait
   call live_timing('Interaction_vvcv',px%stps(myid+1))
   ! Paral >
   !
   !          1.b: U_{v v1 c2 v2} = Ovv * BS_W * Ocv
   !
   do iv=1,nv
     do iv1=1,nv_doubles
       !
       ! Paral <
       if (.not.px%i2p(iv,iv1)) cycle
       call live_timing(steps=1)
       ! Paral >
       if (BS_W_is_diagonal) then
         forall(i1=1:BS_n_g_W) Ovv(i1,iv1,iv)=Ovv(i1,iv1,iv)*BS_W(i1,1,1) 
       else
         !
         do i1=1,BS_n_g_W 
#if defined _DOUBLE
           Ovv(i1,iv1,iv)=zdotu(BS_n_g_W,Ovv(1,iv1,iv),1,BS_W(1,i1,1),1)
#else
           Ovv(i1,iv1,iv)=cdotu(BS_n_g_W,Ovv(1,iv1,iv),1,BS_W(1,i1,1),1)
#endif
         enddo
       endif
       do iv2=1,nv_doubles
         do ic2=1,nc_doubles
#if defined _DOUBLE
           Interaction_vvcv(iv1,iv,ic2,iv2)=zdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Ovv(1,iv1,iv),1)*4.*pi       
#else                                                         
           Interaction_vvcv(iv1,iv,ic2,iv2)=cdotc(BS_n_g_W,Ocv(1,ic2,iv2),1,Ovv(1,iv1,iv),1)*4.*pi
#endif
           ! DEBUG <
           write(3458,*) iv+BS_bands(1)-1,iv1+BS_bands_doubles(1)-1,ic2+Ken%nbf,iv2+BS_bands_doubles(1)-1,&
&                        Interaction_vvcv(iv1,iv,ic2,iv2)
           ! DEBUG >
         enddo
       enddo
     enddo
   enddo 
   !
   ! Paral <
   call live_timing()
   call pp_redux_wait(Interaction_vvcv)
   call pp_indexes_reset(px)
   !
 endif
 !
 deallocate(Occ,Ovv,Ocv)
 call mem_est("Occ Ovv Ocv")
 !
 if (BS_res_K_doubles) then
   allocate(BS_mat_doubles(BS_K_dim,BS_K_doubles_dim,ncin+1))
   call mem_est('BS_mat_doubles',(/size(BS_mat_doubles)*2/))
   BS_mat_doubles(:,:,:)=(0.,0.)
   !
   !  Step 2: Here the code constructs the half-diagrams
   !          D1(icv,i_doubles) and D2(icv,i_doubles),
   !          starting from U_{cc1,c2v2} and U_{v1v,c2v2}
   !
   !          c  ===<===:===<===  c1              c  ===<===x===<===  c1
   !                    :                                   
   !                q  /:\                                    ==<===  c2
   !                    :                                    /
   !        D1          :===<===  c2          D2             ===>===  v2
   !                     \                                  : 
   !                      ==>===  v2                     q \:/
   !                                                        :
   !          v  ===>===x===>===  v1                 ===>===:===>===  v1
   !
   !          and the final simmetrized expression:
   !          C_{sd}=0.5*[D1+D2 -(c1<->c2) -(v1<->v2) +(c1,v1<->c2,v2)]
   !          Here the prefactor 0.5 to each half-diagram is needed as we have 8 half-diagrams
   !          which composed will give 8*8=64 diagrams, but equal at groups of 4
   !          (i.e. D1*D2 = D1(c1<->c2)*D2(c1<->c2) = ...) so we have to divide by 4(=0.5*0.5)
   !
   !  In the code:
   !  BS_mat_doubles(:,:,1)=sqrt(0.5)*[D1+D2 +(c1,v1<->c2,v2)]*sqrt(2)                 spin:(up,down)
   !  BS_mat_doubles(:,:,2)=sqrt(0.5)*[D1+D2 +(c1,v1<->c2,v2) -(c1<->c2) -(v1<->v2) ]  spin:(up,up)
   !
   !DEBUG <
   write(3459,*) '                                                                       ',&
&                '       kin1            kin2            kin3            kin4     ',&
&                '       kin5            kin6            kin7            kin8     ',&
&                '       kin1            kin2            kin3            kin4     ',&
&                '       kin5            kin6            kin7            kin8     '
   !DEBUG >
   !
   ! Paral <
   call par_indexes(px,(/4*ncin,BS_K_doubles_dim/))
   call pp_redux_wait
   call live_timing('BS_mat_doubles',px%stps(myid+1))
   ! Paral >
   do icin=1,4*ncin
     do i_doubles=1,BS_K_doubles_dim
       ! Paral <
       if (.not.px%i2p(icin,i_doubles)) cycle
       call live_timing(steps=1)
       ! Paral >
       iv1=BS_doubles_table(i_doubles,1)-BS_bands_doubles(1)+1
       iv2=BS_doubles_table(i_doubles,2)-BS_bands_doubles(1)+1
       ic1=BS_doubles_table(i_doubles,3)-Ken%nbf
       ic2=BS_doubles_table(i_doubles,4)-Ken%nbf
       !
       do icv=1,BS_K_dim
         iv=BS_eh_table(icv,2)-BS_bands(1)+1
         ic=BS_eh_table(icv,3)-Ken%nbf
         select case (icin)
           case(1)
             if ( (iv+BS_bands(1)) /= (iv1+BS_bands_doubles(1)) ) cycle
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                   Interaction_cccv(ic,ic1,ic2,iv2)                  !D1
           case(2)
             if ( (iv+BS_bands(1)) /= (iv2+BS_bands_doubles(1)) ) cycle 
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                   Interaction_cccv(ic,ic2,ic1,iv1)                  !D1cv
           case(3)
             if (ic/=ic1) cycle
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                   Interaction_vvcv(iv1,iv,ic2,iv2)                  !D2
           case(4)
             if (ic/=ic2) cycle 
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                   Interaction_vvcv(iv2,iv,ic1,iv1)                  !D2cv
           case(5)
             if ( (iv+BS_bands(1)) /= (iv1+BS_bands_doubles(1)) ) cycle
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                  -Interaction_cccv(ic,ic2,ic1,iv2)                  !D1c
           case(6)
             if ( (iv+BS_bands(1)) /= (iv2+BS_bands_doubles(1)) ) cycle 
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                  -Interaction_cccv(ic,ic1,ic2,iv1)                  !D1v
           case(7)
             if (ic/=ic1) cycle
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                  -Interaction_vvcv(iv2,iv,ic2,iv1)                  !D2v
           case(8)
             if (ic/=ic2) cycle
             BS_mat_doubles(icv,i_doubles,ncin+1)=&
&                  -Interaction_vvcv(iv1,iv,ic1,iv2)                  !D2c
         end select
!       ! DEBUG <
!       write(3459,*) ic+Ken%nbf,iv+BS_bands(1)-1,&
!&                    ic1+Ken%nbf,ic2+Ken%nbf,iv1+BS_bands_doubles(1)-1,iv2+BS_bands_doubles(1)-1,&
!&                    (real(BS_mat_doubles(icv,i_doubles,i1)),i1=1,8),&
!&                    (aimag(BS_mat_doubles(icv,i_doubles,i1)),i1=1,8)
!       ! DEBUG >
       enddo
     enddo
     !
     ! sqrt(0.5) is due to second order
     ! For BS_mat_doubles_1 we obtain an overall factor 0.5
     ! For BS_mat_doubles_2 we obtain an overall factor 0.5
     if(icin<=4) BS_mat_doubles(:,:,1)=BS_mat_doubles(:,:,1)+0.5*BS_mat_doubles(:,:,ncin+1)
     if(.not.doubles_corr) BS_mat_doubles(:,:,2)=&
&                    BS_mat_doubles(:,:,2)+0.5*sqrt(0.5)*BS_mat_doubles(:,:,ncin+1)
     BS_mat_doubles(:,:,ncin+1)=(0.,0.)
   enddo
   !
   ! Paral <
   call pp_redux_wait(BS_mat_doubles)
   call live_timing()
   call pp_indexes_reset(px)
   ! Paral >
   !
   !
   !
   ! DUOBLES CORRELATION PROJECT
   ! *******************************
   ! We do a static diagonalization to get the RPA eigenvalues and
   ! eigenvectors. Then construct from these the RPA doubles eigenvectors.
   ! (Write a new database for the doubles with CU instead of C (this for restart) 
   ! and BS_doubles_E(i_doubles)=E_1^RPA+E_2^RPA
   !
   if (doubles_corr) then
     BS_mat_doubles(:,:,2)=(0.,0.)
     BS_doubles_E(:)=0.
     do icv1=2,BS_K_dim
       forall(icv2=1:icv1-1) BS_mat_tmp(icv1,icv2,1)=conjg(BS_mat_tmp(icv2,icv1,1))
     enddo
     do icv1=1,BS_K_dim
       iv1=BS_eh_table(icv1,2)
       ic1=BS_eh_table(icv1,3)
       BS_mat_tmp(icv1,icv1,1)=BS_mat_tmp(icv1,icv1,1)+Ken%E(ic1,1,1)-Ken%E(iv1,1,1)
     enddo
     call mat_dia_inv(DIAGO,lib_driver,BS_mat_tmp(:,:,1),E_real=BS_E_tmp)
     allocate(Eigen_doubles(BS_K_dim**2,1))
     !
     ! Paral <
     call par_indexes(px,(/BS_K_doubles_dim/),(/1/))
     call pp_redux_wait
     call live_timing('Doubles correlation',px%stps(myid+1))
     ! Paral >
     !
     do i_doubles=1,BS_K_doubles_dim
       ! Paral <
       if (.not.px%i1p(i_doubles)) cycle
       call live_timing(steps=1)
       ! Paral >
       iv1=BS_doubles_table(i_doubles,1)
       iv2=BS_doubles_table(i_doubles,2)
       ic1=BS_doubles_table(i_doubles,3)
       ic2=BS_doubles_table(i_doubles,4)
       icv1=BS_eh_table_m1(iv1,ic1)
       icv2=BS_eh_table_m1(iv2,ic2)
       BS_doubles_E(i_doubles)=BS_E_tmp(icv1)+BS_E_tmp(icv2)
       ! Important: here the indexes loops in the same way in i_doubles and icv1,icv2 in the
       ! tables, i_doubles=icv1,ivc2=iv1,ic1,iv2,ic2 ---> the last index goes faster
       do icv=1,BS_K_dim
         Eigen_doubles( 1+(icv-1)*BS_K_dim : icv*BS_K_dim ,1)=&
&                  BS_mat_tmp(:,icv1,1)*BS_mat_tmp(icv,icv2,1)
       enddo
       call cgemv('n',BS_K_dim,BS_K_doubles_dim,(1._SP,0._SP),&
&        BS_mat_doubles(:,:,1),BS_K_dim,Eigen_doubles(:,1),1,&
&        (0._SP,0._SP),BS_mat_doubles(:,i_doubles,2),1)
     enddo
     !
     ! Paral <
     call pp_redux_wait(BS_mat_doubles(:,:,2))
     call pp_redux_wait(BS_doubles_E)
     call live_timing()
     call pp_indexes_reset(px)
     ! Paral >
     BS_mat_doubles(:,:,1)=BS_mat_doubles(:,:,2)
     !
     do icv=1,BS_K_dim
       write(*,*) BS_E_tmp(icv)*27.2116
     enddo
     deallocate(Eigen_doubles,BS_E_tmp,BS_eh_table_m1)
     !
   endif
   !
   ! Write BS_mat_doubles to disk
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioBS_doubles(ID)
   !
   ! Final deallocation
   deallocate(Interaction_cccv,Interaction_vvcv)
   call mem_est('Interaction_cccv Interaction_vvcv')
   deallocate(BS_mat_doubles)
   call mem_est('BS_mat_doubles')
   deallocate(BS_doubles_table,BS_doubles_E)
   call mem_est("BS_doubles_E")
   !
 endif
 stop
 !
 !  ********************************************
 !  *******       Main loop: end         *******
 !  ********************************************
 !
 ! DEBUG >
 close(unit=3456)
 close(unit=3457)
 close(unit=3458)
 close(unit=3459)
 ! DEBUG <
 !
end subroutine
