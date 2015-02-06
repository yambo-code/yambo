!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG DS
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
subroutine K_Haydock(iq,W)
 !
 use pars,           ONLY:SP,pi,schlen,cI,cZERO,cONE
 use com,            ONLY:error
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_epsilon,X_drude_term
 use R_lattice,      ONLY:d3k_factor,q_norm
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,isec
 use BS,             ONLY:BSS_n_descs,Haydock_treshold,BSS_description,& 
&                         io_BSS_Haydock, BS_K_Coupling,&
&                         Haydock_terminator, BSE_mode,Haydock_v0_mod,BS_Blocks_symmetrize_K,&
&                         BS_HAYVEC_free,BS_HAYVEC_alloc, BS_T_grp,BS_nT_grps,&
&                         Haydock_vector,BS_BLK, N_BS_BLKS, Haydock_parallel_mask,Haydock_parallel_group_comm
 use interfaces,     ONLY:K_multiply_by_V,K_dot_product
 use LIVE_t,         ONLY:LIVE_message
 use stderr,         ONLY:tty_size,intc,real2ch
 use electrons,      ONLY:spin_occ
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 use parallel_m,     ONLY:PAR_IND_T_groups,PP_bcast
#if defined _KERR
 use KERR,           ONLY:BSS_rhoq0_kerr,B_Hall
 use drivers,        ONLY:l_kerr,l_anomalous_Hall
 use fields,         ONLY:global_gauge
 use X_m,            ONLY:iw_ref
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(w_samp)  ::W
 integer       ::iq
 !
 ! Work Space
 !
 integer            ::it,it_on_disk,i1,ierr,i_g,I_TP,I_TK,I_B
 integer, parameter ::Max_iterations=1000
 type(Haydock_vector),allocatable :: Vn(:),Vnm1(:),Vnp1(:),V0(:)
 complex(SP),allocatable :: Cf(:)
#if defined _KERR
 complex(SP),allocatable :: Cf_kerr(:)
#endif
 real(SP)           ::reached_treshold, Bf(Max_iterations)&
&,                    Af(Max_iterations)
 real(SP)           ::ItParity
 complex(SP)        ::v0_mod_
 character(schlen)  ::dumb_ch
 logical            ::use_cpl_scheme
 logical, external  ::stop_now
 !
 ! I/O
 !
 integer           :: ID,io_err
 !
 call PARALLEL_Haydock_VEC_COMMs('assign')
 !
#if defined _TIMING
 call timing('Haydock Solver',OPR='start')
#endif
 !
 if (isec(2)/=0) then
   call section('=','Haydock solver')
 else if (isec(2)==0) then
   call section('+','Haydock solver')
 endif
 !
 call BS_Blocks_symmetrize_K()
 !
 ! Use terminator?
 if (.not.Haydock_terminator) call parser('BSHayTer',Haydock_terminator)
 !
 ! Which approach
 use_cpl_scheme=BS_K_Coupling
#if defined _KERR
 use_cpl_scheme=BS_K_Coupling.or.l_kerr
#endif
 !
 ! Allocate Haydock vectors:
 if (use_cpl_scheme) then
   allocate(Cf(Max_iterations+1))
#if defined _KERR
   if(l_kerr) allocate(Cf_kerr(Max_iterations+1))
#endif
   allocate(V0(BS_nT_grps))
   call BS_HAYVEC_alloc(V0)
 end if
 !
 allocate(Vn(BS_nT_grps),Vnp1(BS_nT_grps),Vnm1(BS_nT_grps))
 call BS_HAYVEC_alloc(Vn)
 call BS_HAYVEC_alloc(Vnm1)
 call BS_HAYVEC_alloc(Vnp1)
  !
  !
  ! Haydock Coefficients and restart vector on disk
  !=================================================
 it_on_disk=0
 ! call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 ! !
 ! if (use_cpl_scheme) then 
 !  !
#if defined _KERR
 !  if(l_kerr) then
 !    io_err=ioBSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1),  &
 !    &                   Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1),Cf_kerr=Cf_kerr(1))
 !  else
#endif
 !   io_err=io_BSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1)&
 !        &,Vmm1,Vm,Vnp1=Vmp1,Cf=Cf(1))   
#if defined _KERR
 !  endif
#endif
 ! else
 !   io_err=io_BSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1),Vmm1,Vm)
 !endif
 !
 !
 if (io_err>0) call error('K_Haydock: restart of Haydock temporary&
      & disabled') 
 !
 ! if (io_err>0) then 
!    !
!    it_on_disk=io_err 
!    call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
!    if (use_cpl_scheme) then 
#if defined _KERR
!     if(l_kerr) then
!       io_err=ioBSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,Af(:it_on_disk),Bf(:it_on_disk+1),&
!       &                    Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1),Cf_kerr=Cf_kerr(:it_on_disk+1))
!     else
#endif
!       io_err=ioBSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,Af(:it_on_disk),&
!       &                   Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1))
#if defined _KERR
!     endif
#endif
!    else
!      io_err=io_BSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,Af(:it_on_disk),&
!      &      Bf(:it_on_disk+1),Vmm1,Vm)
!      call msg('s','[Haydock] Restart DB with accuracy ',reached_treshold)
!    end if
!    !
!  endif
 !
 ! Update Descriptions
 !=====================
 BSS_n_descs=BSS_n_descs+2
 BSS_description(BSS_n_descs-1)=' '
 BSS_description(BSS_n_descs)=' Haydock|Accuracy (requested) [o/o]:'&
      &// trim(real2ch(Haydock_treshold))
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='        |Accuracy (reached)   [o/o]:'&
      &// trim(real2ch(0._SP))
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='        |Iteration            [o/o]: 0'
 !
 call msg('r','Accuracy (requested) [o/o]:',Haydock_treshold)
 !
 ! Initialization of Haydock vectors and tridiagonal matrix elements
 !
 if (it_on_disk==0) then
   do i_g=1,BS_nT_grps
     if (.not.Haydock_parallel_mask(i_g)) cycle
     Vn(i_g)%fragment  = cZERO
     Vnm1(i_g)%fragment= cZERO
     Vnp1(i_g)%fragment= cZERO
   enddo
   !
   Af= 0.
   Bf= 0.
 end if
 !
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
   Vn(i_g)%fragment(:) =BS_T_grp(i_g)%dipoles_R(:)
 enddo
 do i_g = 2,BS_nT_grps
   if (.not.Haydock_parallel_mask(i_g)) cycle
#if defined _MPI
   call PP_bcast(Vn(i_g)%fragment(:),0,COMM=Haydock_parallel_group_comm(i_g))
#endif    
 end do
 !
 if (use_cpl_scheme) then ! [***] Initialization non Hermitian case
   !
   ! Before starting iterate one needs
   !
   ! | Vm > =     |q->0>/(<q->0| F (H |q->0>))
   ! | Vmp1 > = H |Vm>
   !
   ItParity = (-1._SP)**it_on_disk
   if (it_on_disk==0) then
      Cf= (0.,0.)
#if defined _KERR
      if(l_kerr) Cf_kerr= (0.,0.)
#endif
      !
      call K_multiply_by_V(it_on_disk,Vn,Vnp1,iter_par=ItParity,call_kind='Hay')
      do i_g=1,BS_nT_grps
        if (.not.Haydock_parallel_mask(i_g)) cycle
        !
      enddo
      call K_dot_product(v0_mod_,Vn,Vnp1)
      Haydock_v0_mod=sqrt(2._SP*real(v0_mod_,SP))
      Cf(1) =  Haydock_v0_mod
#if defined _KERR
      if(l_kerr) Cf_kerr(1)= 2._SP*cI*aimag(dot_product(BSS_rhoq0_kerr(:BS_K_dim),Vn))/Haydock_v0_mod**2._SP
#endif
      do i_g=1,BS_nT_grps
        if (.not.Haydock_parallel_mask(i_g)) cycle
        !
        V0(i_g)%fragment   = Vn(i_g)%fragment ! This vector is needed to determine the Cf element at each iteration
        Vn(i_g)%fragment   = Vn(i_g)%fragment/Haydock_v0_mod
        Vnp1(i_g)%fragment = Vnp1(i_g)%fragment/Haydock_v0_mod
      enddo
   else
   !   Haydock_v0_mod = Bf(1)
   !   Bf(1) = 0._SP
   endif
   !
 else ! [***] Initialization Hermitian case
   !
   ! it=1:|Vm>=|q->0>/(<q->0|q->0>)^(1/2)
   !
   call K_dot_product(v0_mod_,Vn,Vn) 
   Haydock_v0_mod=sqrt(real(v0_mod_,SP))
   !
   if (it_on_disk==0) then
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       Vn(i_g)%fragment  = Vn(i_g)%fragment/Haydock_v0_mod
     enddo
   endif
   !
 endif
 !
 ! [***] Loop ...
 !
 do it=it_on_disk+1,Max_iterations
   !
   reached_treshold=0.
   !
   ! Hermitian case: 
   !
   if (.not.use_cpl_scheme) then
     !
     !|Vm+1> = H |Vm>
     !
     call  K_multiply_by_V(it,Vn,Vnp1,call_kind='Hay')
     !
     ! A(n) = <Vm|Vm+1>
     !
     call K_dot_product(v0_mod_,Vn,Vnp1) 
     Af(it)=real(v0_mod_,SP)
     !
     !|Vm+1> = |Vm+1> - A(n)|Vm> - B(n)|Vm-1>
     !
     do i_g = 1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Af(it)*cONE,Vn(i_g)%fragment,Vnp1(i_g)%fragment)
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment,Vnp1(i_g)%fragment)
     end do
     !
     ! B(n+1)= <Vm+1|Vm+1>^(1/2)
     !
     call K_dot_product(v0_mod_,Vnp1,Vnp1) 
     Bf(it+1)=sqrt(real(v0_mod_,SP))
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=NaN likely because some eigenvalue of the BSE is negative.')
     !
     !|Vm-1> = |Vm>
     !
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle 
       Vnm1(i_g)%fragment  =Vn(i_g)%fragment
     enddo
     !
     !|Vm> =|Vm+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       Vn(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
     enddo
     !
     ! [***] Non-Hermitian case: 
     !
   else 
     !
     ! A(n) = <Vm+1|F|Vm+1> (that is <Vm|HFH|Vm>), 
     !      =  0   by symmetry 
     !
     ItParity=(-1._SP)**it
     Af(it)=0._SP
     !
     !|Vm+1> = |Vm+1> - B(n)|Vm-1> (as Am is zero by symmetry)
     !
     do i_g = 1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment,Vnp1(i_g)%fragment)
     end do
     !
     ! |Vm-1> = |Vm> 
     ! |Vm>   = |Vm+1> 
     !
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       Vnm1(i_g)%fragment = Vn(i_g)%fragment
       Vn(i_g)%fragment   = Vnp1(i_g)%fragment
     enddo
     !
     !|Vm+1> = H |Vm>
     !
     call K_multiply_by_V(it-it_on_disk,Vn,Vnp1,iter_par=ItParity,call_kind='Hay')
     !
     ! B(n+1)= <Vm|F|Vm+1>^(1/2) (that is <Vm|FH|Vm>^(1/2))
     !       = (2*Re(<Vm|Vmp1>))^(1/2) by symmetry, 
     !          where the dot_product is just on eh pair space 
     !
     call K_dot_product(v0_mod_,Vn,Vnp1) 
     Bf(it+1)=sqrt(2._SP*real(v0_mod_,SP))
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=NaN likely because some eigenvalue of the BSE is negative.')
     !
#if !defined _BLUEGENE
     !if( isnan(Bf(it+1)) ) call error(' Bf=NaN likely because some eigenvalue of BSE is negative.')
#endif
     !
     !|Vm> =|Vm+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.Haydock_parallel_mask(i_g)) cycle
       Vnp1(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
       Vn(i_g)%fragment    =Vn(i_g)%fragment/Bf(it+1)
     enddo
     !
     ! Using symmetry arguments
     ! Cf=(1-ItParity)Re(<rhoq0|Vm>) + i*(1+ItParity)Im(<rhoq0|Vm>) 
     ! with all product evaluated just in the eh pairs subspace 
     !
     call K_dot_product(v0_mod_,V0,Vn) 
     Cf(it+1)= ((1._SP-ItParity)*real(v0_mod_,SP))/Haydock_v0_mod
#if defined _KERR
     if(l_kerr) Cf_kerr(it+1)= &
&             ((1._SP+ItParity)*cI*aimag(dot_product(BSS_rhoq0_kerr(:BS_K_dim),Vn)))/Haydock_v0_mod
#endif
     !
   endif
   !
   if (it>2) then
     !
     if (use_cpl_scheme) then 
       call build_L_and_check_convergence(Af(:it),Bf(:it+1),it,Cf(:it),1)
     else
       call build_L_and_check_convergence(Af(:it),Bf(:it+1),it)
     end if
#if defined _KERR
     if(l_kerr) call build_L_and_check_convergence(Af(:it),Bf(:it+1),it,Cf_kerr(:it),2)
#endif
     !
     BSS_description(BSS_n_descs-1)='        |Accuracy (reached)   [o/o]:'//&
&                                    trim(real2ch(reached_treshold))
     BSS_description(BSS_n_descs)='        |Iteration                 :'//trim(intc(it))
     !
     call K_output_file(iq,1)
     dumb_ch='[Haydock] Iteration '//trim(intc(it))//' Accuracy :'//&
&            trim(real2ch(reached_treshold))//'|'//trim(real2ch(Haydock_treshold))
     if (tty_size>0.and.it_on_disk==0) call LIVE_message(dumb_ch,"r","","%s")
     if (tty_size>0.and.it_on_disk> 0) call LIVE_message(dumb_ch,"n","","%s")
     if (tty_size<0) call LIVE_message(dumb_ch,"n","","%s")
   else
     !
     dumb_ch='[Haydock] Iteration '//trim(intc(it))
     if (tty_size<0) call LIVE_message(dumb_ch,"n","","%s")
     if (tty_size>0.and.it==1) call LIVE_message(dumb_ch,"n","","%s")
     if (tty_size>0.and.it==2) call LIVE_message(dumb_ch,"n","","%s")
     !
   endif
   !
   ! Haydock Coefficients and restart vectors on disk
   !--------------------------------------------------
   !call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
!   if (use_cpl_scheme) then
!     Bf(1) = Haydock_v0_mod
#if defined _KERR
!     if(l_kerr) then
!       io_err=ioBSS_Haydock(ID,it,BS_K_dim,reached_treshold,  &
!       &                    Af(:it),Bf(:it+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1),Cf_kerr=Cf_kerr(:it+1))
!     else 
#endif
!      io_err=io_BSS_Haydock(ID,it,BS_K_dim,reached_treshold,  &
! &                          Af(:it),Bf(:it+1),Vmm1,Vm,Vnp1=Vmp1,Cf=Cf(:it+1))
#if defined _KERR
!     endif
#endif
!     Bf(1) = 0._SP
!   else
!     io_err=io_BSS_Haydock(ID,it,BS_K_dim,reached_treshold,Af(:it),Bf(:it+1),Vmm1,Vm)
!   end if
!   !
   if ( (reached_treshold>0.and.reached_treshold<abs(Haydock_treshold)) .or. &
&       stop_now(.FALSE.)) then
     call msg('r','Accuracy (reached)   [o/o]:',reached_treshold)
     call msg('r','Iterations                :',it)
     exit
   endif
   !
 enddo
 !
 ! CLEAN
 !=======
 !
 call BS_HAYVEC_free(Vn)
 call BS_HAYVEC_free(Vnp1)
 call BS_HAYVEC_free(Vnm1)
 deallocate(Vn,Vnp1,Vnm1)
 !
 if (use_cpl_scheme)  then
   call BS_HAYVEC_free(V0)
   if (allocated(V0)) deallocate(V0)
   if (allocated(Cf)) deallocate(Cf)
#if defined _KERR
   if (allocated(Cf_kerr)) deallocate(Cf_kerr)
#endif
 end if
 !
#if defined _TIMING
 call timing('Haydock Solver',OPR='stop')
#endif
 !
 contains
   !
   subroutine build_L_and_check_convergence(Af,Bf,it,Cf,ii)
     implicit none
     integer    :: it
     real(SP)   :: Bf(it+1),Af(it)
     complex(SP),intent(in),optional :: Cf(it)
     integer, intent(in),optional :: ii
     !
     ! Work Space
     !
     integer     :: i1,iw,i_eps(2)
     real(SP)    :: Co
#if defined _KERR
     real(SP)    :: factor_jj
     real(SP)    :: gauge_factor
#endif
     complex(SP) :: X_o(4,it), YbyX(2)
     complex(SP),allocatable :: X_i(:,:)
     !
     ! related to terminator
     !
     complex(SP) :: X_t(2)
     real(SP)    :: Av1, Av2
     integer, parameter :: RES = 3, ARES = 2, COUPL = 1, MIN_ITER = 10
     logical     :: UseTerminator
     logical     :: causal, coupling
     !
     Co=real(spin_occ)/(2._SP*pi)**3._SP*d3k_factor*4._SP*pi/q_norm(1)**2._SP
     !
     X_t =(0._SP,0._SP)
     UseTerminator = .false.  
     !
     causal  = (trim(BSE_mode)=='causal'.and..not.use_cpl_scheme)
     coupling= use_cpl_scheme
     !
     if (it > MIN_ITER.and.Haydock_terminator) UseTerminator = .true.
     if (coupling) allocate(X_i(2,it))
     if (Useterminator) then 
       if (.not.coupling) then
         Av1 = 0._SP
         Av2 = 0._SP
         do i1 = 1, it
           Av1 = Av1 + Af(i1)
           Av2 = Av2 + Bf(i1)
         end do
         Av1 = Av1/it
         Av2 = Av2/it
       else
         Av1 = 0._SP
         Av2 = 0._SP
         do i1 = 2,it+1,2
           Av1 = Av1 + Bf(i1)
           Av2 = Av2 + Bf(i1+1)
         end do
         Av1 = Av1/(it/2+mod(it,2))
         Av2 = Av2/(it/2)
       end if
     end if
     !
     do iw=1,W%n_freqs
       X_o=(0._SP,0._SP)
       if (UseTerminator) then 
         if (.not.coupling) then
           X_t(1) = terminator(W%p(iw), Av1,Av2, RES)
           if (causal) X_t(2) = terminator(W%p(iw),-Av1,Av2,ARES)
         endif
         if (coupling.and.(mod(it,2)==0)) X_t(1) = terminator(W%p(iw),Av1,Av2,COUPL)
         if (coupling.and.(mod(it,2)==1)) X_t(1) = terminator(W%p(iw),Av2,Av1,COUPL)
       endif
       X_o(:2,it) = 1._SP/(W%p(iw)-Af(it)-Bf(it+1)**2*X_t(1))
       if (causal) X_o(3:4,it)= 1._SP/(W%p(iw)+Af(it)-Bf(it+1)**2*X_t(2))
       do i1=it-1,1,-1
         X_o(1,i1) = 1._SP/(W%p(iw)-Af(i1)-Bf(i1+1)**2*X_o(1,i1+1) )
         if (causal) X_o(3,i1)= 1._SP/(W%p(iw)+Af(i1)-Bf(i1+1)**2*X_o(3,i1+1))
       enddo
       do i1=it-2,1,-1
         X_o(2,i1) = 1._SP/(W%p(iw)-Af(i1)-Bf(i1+1)**2*X_o(2,i1+1) )
         if (causal) X_o(4,i1)= 1._SP/(W%p(iw)+Af(i1)-Bf(i1+1)**2*X_o(4,i1+1))
       enddo
       if (coupling) then
         X_i(:,1) = -X_o(:2,1)
         X_i(:,2) = -Bf(2)*X_o(:2,2)*X_i(:,1)
         YbyX(:) = X_i(:,1)*Cf(1) + X_i(:,2)*Cf(2)
         do i1 = 2,it-1
           X_i(1,i1+1) = -Bf(i1+1)*X_o(1,i1+1)*X_i(1,i1)
           YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
         end do
         do i1 = 2,it-2
           X_i(2,i1+1) = -Bf(i1+1)*X_o(2,i1+1)*X_i(2,i1)
           YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
         end do
         X_o(:2,1) = YbyX(:)
       endif
       i_eps(1)=2
       i_eps(2)=4
       if(present(ii)) then
         i_eps(1)=3*ii-1
         i_eps(2)=3*ii+1
       endif
       X_epsilon(1,iw)=W%p(iw)
       !
#if defined _KERR
       !
       if(trim(global_gauge)=='length')   factor_jj=0._SP
       if(trim(global_gauge)=='velocity') factor_jj=X_epsilon(3,iw_ref)
       !
       if(l_anomalous_Hall) B_Hall(2)=B_Hall(2)*Co
       !
       gauge_factor=1._SP
       if(trim(global_gauge)=='velocity')  gauge_factor=(real(W%p(iw))**2+epsilon(1.))/q_norm(1)**2
       X_epsilon(i_eps(:),iw)=1._SP-(X_o(:2,1)-factor_jj)*Co/gauge_factor*Haydock_v0_mod**2._SP
       if (causal) X_epsilon(i_eps(:),iw)=X_epsilon(i_eps(:),iw)+X_o(3:4,1)*Co/gauge_factor*Haydock_v0_mod**2._SP
       if (i_eps(1)==2) then
         X_epsilon(2,iw)=X_epsilon(2,iw)+X_drude_term(iw)*Co/gauge_factor
         X_epsilon(4,iw)=X_epsilon(4,iw)+X_drude_term(iw)*Co/gauge_factor
       endif
       if (i_eps(1)==5) X_epsilon(i_eps(:),iw)=1._SP-X_epsilon(i_eps(:),iw)
       !
       ! I add the term describing the Anomalous Hall effect which is
       ! missing in the length gauge (this should be zero for systems with a gap)
       if (trim(global_gauge)=='length'.and.l_anomalous_Hall) X_epsilon(5,iw)=X_epsilon(5,iw)+B_Hall(2)/W%p(iw)
       !
#else
       !
       X_epsilon(i_eps(:),iw)=1._SP-X_o(:2,1)*Co*Haydock_v0_mod**2._SP
       if (causal) X_epsilon(i_eps(:),iw)=X_epsilon(i_eps(:),iw)+X_o(3:4,1)*Co*Haydock_v0_mod**2._SP
       if (i_eps(1)==2) then
         X_epsilon(2,iw)=X_epsilon(2,iw)+X_drude_term(iw)*Co
         X_epsilon(4,iw)=X_epsilon(4,iw)+X_drude_term(iw)*Co
       endif
       if (i_eps(1)==5) X_epsilon(i_eps(:),iw)=1._SP-X_epsilon(i_eps(:),iw)
#endif
       !
       if (Haydock_treshold>0._SP) reached_treshold=max(reached_treshold,&
&         abs(X_epsilon(i_eps(1),iw)-X_epsilon(i_eps(2),iw))/abs(X_epsilon(2,iw)))
       if (Haydock_treshold<0._SP) reached_treshold=reached_treshold+&
&         abs(X_epsilon(i_eps(1),iw)-X_epsilon(i_eps(2),iw))/abs(X_epsilon(2,iw))/real(W%n_freqs)
       !
     enddo
     !
     if (use_cpl_scheme) deallocate(X_i)
   end subroutine
   !
   function terminator(x,c1,c2,which)
     !
     complex(SP) :: terminator, f, g, x
     real(SP) :: c1,c2
     integer  :: which
     !
     select case(which)
     case (1) 
       f = x**2 - c1**2 + c2**2
       g = 2._SP*x*c2**2
       terminator = f + sqrt(f**2 -2._SP*x*g) 
       terminator = terminator/g
     case (2)
       f = (x+c1)
       terminator = f + sqrt(f**2 -4._SP*c2**2.)
       terminator = terminator/2._SP/c2**2
     case (3)
       f = (x-c1)
       terminator = f + sqrt(f**2 -4._SP*c2**2.)
       terminator = terminator/2._SP/c2**2
     end select
   end function terminator
   !
end subroutine
