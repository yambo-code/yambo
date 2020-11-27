!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine K_Haydock(iq,W,mode)
 !
 use pars,           ONLY:SP,schlen,cI,cZERO,cONE,pi
 use functions,      ONLY:NAN
 use X_m,            ONLY:global_gauge
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,isec
 use BS_solvers,     ONLY:BSS_desc,Haydock_threshold,io_BSS_Haydock,&
&                         Haydock_vector,BS_HAYVEC_alloc,BS_HAYVEC_free,Haydock_terminator,Haydock_v0_mod,Haydock_iterIO
 use BS,             ONLY:BS_K_Coupling,BS_Blocks_symmetrize_K,BS_T_grp,BS_nT_grps,BS_K_dim,&
&                         l_BS_ares_asymm,BS_H_dim
 use LIVE_t,         ONLY:LIVE_message
 use stderr,         ONLY:tty_size,intc,real2ch
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 use parallel_m,     ONLY:PAR_IND_T_groups,PP_bcast,PAR_IND_T_Haydock,PAR_COM_T_Haydock,PAR_COM_Xk_ibz_INDEX, &
                             PAR_COM_eh_INDEX
 use timing_m,       ONLY:timing
 use descriptors,    ONLY:IO_desc_add
 !
#include<memory.h>
 !
 type(w_samp), intent(in) :: W
 integer,      intent(in) :: iq
 character(*), intent(in) :: mode
 !
 ! Work Space
 !
 integer            ::it,it_on_disk,i_g,ID,io_err,i_desc_acc,i_desc_iter
 integer, parameter ::Max_iterations=1000
 type(Haydock_vector),allocatable :: Vn(:),Vnm1(:),Vnp1(:),V0(:),V_tmp(:)
 complex(SP), pointer :: Vn_f(:),Vnm1_f(:),Vnp1_f(:)
 complex(SP), allocatable :: Cf(:) 
 !
#if defined _KERR
 type(Haydock_vector),allocatable :: V0_right(:)
#endif
 !
 real(SP)           ::reached_threshold
 real(SP)           ::Bf(Max_iterations+1),Af(Max_iterations)
 integer            ::ItParity,HAYVEC_size,Cf_size
 complex(SP)        ::v0_mod_
 character(schlen)  ::dumb_ch
 logical            ::use_cpl_scheme,velocity_correction,use_left_and_right,Haydock_converged
 logical, external  ::STOP_now
 character(1)       ::sec
 !
 ! I/O
 !
 !integer           :: ID,io_err
 !
 call K_output_file(iq,"open Haydock")
 !
 call PARALLEL_Haydock_VEC_COMMs('assign')
 !
 call timing('Haydock Solver',OPR='start')
 !
 if (isec(2)/=0) sec="="
 if (isec(2)==0) sec="+"
 call section(sec,'Haydock Solver for '//trim(mode)//' @q'//trim(intc(iq)))
 !
 call BS_Blocks_symmetrize_K()
 !
 ! Use terminator?
 if (.not.Haydock_terminator) call parser('BSHayTer',Haydock_terminator)
 !
 ! Which approach
 use_left_and_right=.false.
 use_cpl_scheme=BS_K_Coupling !.or.allocated(BS_T_grp(1)%Z)
#if defined _KERR
 use_left_and_right=trim(mode)=="kerr".or.trim(mode)=="dichroism"
 use_cpl_scheme=BS_K_Coupling.or.use_left_and_right
#endif
 velocity_correction= trim(global_gauge)=="velocity" .and. (trim(mode)=="abs" .or. trim(mode)=="kerr")
 !
 if (.not.l_BS_ares_asymm) HAYVEC_size = BS_K_dim(1)
 if (     l_BS_ares_asymm) then
   if(.not.BS_K_coupling) HAYVEC_size = BS_K_dim(1)
   if(     BS_K_coupling) HAYVEC_size = BS_H_dim
 endif
 !
 ! Allocate Haydock vectors:
 !
 allocate(V0(BS_nT_grps))
 call BS_HAYVEC_alloc(V0)
 !
 if(velocity_correction) then
   allocate(V_tmp(BS_nT_grps))
   call BS_HAYVEC_alloc(V_tmp)
 endif
 !
#if defined _KERR
 if(use_left_and_right) then
   allocate(V0_right(BS_nT_grps))
   call BS_HAYVEC_alloc(V0_right)
 endif
#endif
 !
 Cf_size=2
 if (use_cpl_scheme) Cf_size=Max_iterations+1
 YAMBO_ALLOC(Cf,(Cf_size))
 !
 allocate(Vn(BS_nT_grps))
 allocate(Vnp1(BS_nT_grps))
 allocate(Vnm1(BS_nT_grps))
 call BS_HAYVEC_alloc(Vn)
 call BS_HAYVEC_alloc(Vnm1)
 call BS_HAYVEC_alloc(Vnp1)
 !
 ! Haydock Coefficients and restart vector from disk
 !=================================================
 it_on_disk=0
 reached_threshold=0._SP
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=io_BSS_Haydock(ID,1,reached_threshold,mode,Af(1),Bf(:2),Cf(:2),Vnm1_f,Vn_f,Vnp1_f)
 !
 if (io_err>0) then
   !
   YAMBO_ALLOC_P(Vn_f,(HAYVEC_size))
   YAMBO_ALLOC_P(Vnm1_f,(HAYVEC_size))
   if (use_cpl_scheme) then
     YAMBO_ALLOC_P(Vnp1_f,(HAYVEC_size))
   endif
   !
   it_on_disk=io_err
   if (use_cpl_scheme) Cf_size=it_on_disk+1
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
   io_err=io_BSS_Haydock(ID,it_on_disk,reached_threshold,mode,Af(:it_on_disk),&
&                   Bf(:it_on_disk+1),Cf(:Cf_size),Vnm1_f,Vn_f,Vnp1_f)
   !
   call msg('s','[Haydock] Restart DB with accuracy ',reached_threshold)
   !  
   call PP_bcast(Af,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)  
   call PP_bcast(Af,0,COMM=PAR_COM_eh_INDEX%COMM)  
   call PP_bcast(Bf,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)  
   call PP_bcast(Bf,0,COMM=PAR_COM_eh_INDEX%COMM)  
   call HAYVEC_scatter(Vn,Vn_f,HAYVEC_size)
   call HAYVEC_scatter(Vnm1,Vnm1_f,HAYVEC_size)
   if (use_cpl_scheme) then
     call PP_bcast(Cf,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
     call PP_bcast(Cf,0,COMM=PAR_COM_eh_INDEX%COMM)
     call HAYVEC_scatter(Vnp1,Vnp1_f,HAYVEC_size)
   endif
   !
   YAMBO_FREE_P(Vnm1_f)
   YAMBO_FREE_P(Vn_f)
   if (use_cpl_scheme) then
     YAMBO_FREE_P(Vnp1_f)
   endif
   !
 end if
 !
 ! Update Descriptions
 !=====================
 call IO_desc_add(BSS_desc,kind="s",str=" ",S=" ")
 !
 call IO_desc_add(BSS_desc,kind="r",str='[Haydock] Accuracy (requested)',term='o/o',R=(/Haydock_threshold/))
 call IO_desc_add(BSS_desc,kind="r",str='[Haydock] Accuracy (reached)',term='o/o',R=(/reached_threshold/))
 i_desc_acc=BSS_desc%n
 call IO_desc_add(BSS_desc,kind="i",str='[Haydock] Iterations',I=(/it_on_disk/))
 i_desc_iter=BSS_desc%n
 !
 call msg('r','Accuracy (requested) [o/o]',Haydock_threshold)
 !
 ! Initialization of Haydock vectors and tridiagonal matrix elements
 !
 if (it_on_disk==0) then
   !
   do i_g=1,BS_nT_grps
     !
     if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
     !
     Vn(i_g)%fragment  = cZERO
     Vnm1(i_g)%fragment= cZERO
     Vnp1(i_g)%fragment= cZERO
     !
     V0(i_g)%fragment  = cZERO
#if defined _KERR
     if(use_left_and_right) V0_right(i_g)%fragment  = cZERO
#endif
     if(velocity_correction) V_tmp(i_g)%fragment = cZERO
     !
   enddo
   !
   Af= 0._SP
   Bf= 0._SP
   !
 end if
 !
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
   if (trim(mode)=="jdos")                      V0(i_g)%fragment(:) = cONE
   if (trim(mode)=="abs".or.trim(mode)=="kerr") V0(i_g)%fragment(:) = BS_T_grp(i_g)%dipoles_opt(1,:,1)
   if (trim(mode)=="magnons")                   V0(i_g)%fragment(:) = BS_T_grp(i_g)%dipoles_mag(1,:,1)
   V0(i_g)%fragment(:) = V0(i_g)%fragment(:)*sqrt(cmplx(BS_T_grp(i_g)%f_RES(:),kind=SP))
   if(velocity_correction) V_tmp(i_g)%fragment(:) = V0(i_g)%fragment(:)/BS_T_grp(i_g)%E(:)
 enddo
 ! 
 do i_g = 1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
#if defined _MPI
   if(     velocity_correction) call PP_bcast(V_tmp(i_g)%fragment(:),0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
   if(.not.velocity_correction) call PP_bcast(   V0(i_g)%fragment(:),0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
#endif
 enddo
 !
 if(velocity_correction) call K_multiply_by_V(V_tmp,V0,1)
 !
#if defined _KERR
 if(use_left_and_right) then
   !
   do i_g=1,BS_nT_grps
     if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
     if (trim(mode)=="kerr") V0_right(i_g)%fragment  =BS_T_grp(i_g)%dipoles_opt(2,:,1)*sqrt(cmplx(BS_T_grp(i_g)%f_RES(:),kind=SP))
     if(velocity_correction) V_tmp(i_g)%fragment(:) = V0_right(i_g)%fragment(:)/BS_T_grp(i_g)%E(:)
   enddo
   !
   do i_g = 1,BS_nT_grps
     if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
#if defined _MPI
     if(     velocity_correction) call PP_bcast(   V_tmp(i_g)%fragment(:),0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
     if(.not.velocity_correction) call PP_bcast(V0_right(i_g)%fragment(:),0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
#endif
   enddo
   !
   if(velocity_correction) call K_multiply_by_V(V_tmp,V0_right,1)
   !
 endif
#endif
 !
 if(velocity_correction) then
   call BS_HAYVEC_free(V_tmp)
   deallocate(V_tmp)
 endif
 !
 if (it_on_disk==0) then
   !
   if (use_cpl_scheme) then ! [***] Initialization non Hermitian case
     !
     ! Before starting iterate one needs
     !
     ! | Vm > =     |q->0>/(<q->0| F (H |q->0>))
     ! | Vmp1 > = H |Vm>
     !
     Cf= cZERO
     !
     call K_multiply_by_V(V0,Vnp1,1)
     call K_dot_product(v0_mod_,V0,Vnp1,1)
     if(.not.l_BS_ares_asymm) Haydock_v0_mod=sqrt(2*real(v0_mod_))
     if(     l_BS_ares_asymm) Haydock_v0_mod=sqrt(  real(v0_mod_))
     !
     if(.not.use_left_and_right) call K_dot_product(v0_mod_,V0,Vnp1,0)
#if defined _KERR
     if(     use_left_and_right) call K_dot_product(v0_mod_,V0_right,Vnp1,0)
#endif
     !
     if(.not.l_BS_ares_asymm) Cf(1)= 2*cI*aimag(v0_mod_)/Haydock_v0_mod**2
     if(     l_BS_ares_asymm) Cf(1)=            v0_mod_ /Haydock_v0_mod**2
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vn(i_g)%fragment   = V0(i_g)%fragment/Haydock_v0_mod
       Vnp1(i_g)%fragment = Vnp1(i_g)%fragment/Haydock_v0_mod
     enddo
     !
   else ! [***] Initialization Hermitian case
     !
     ! it=1:|Vm>=|q->0>/(<q->0|q->0>)^(1/2)
     !
     call K_dot_product(v0_mod_,V0,V0,1) 
     Haydock_v0_mod=sqrt(real(v0_mod_,SP))
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vn(i_g)%fragment  = V0(i_g)%fragment/Haydock_v0_mod
     enddo
   endif
   !
 endif
 !
 ! [***] Loop ...
 !
 do it=it_on_disk+1,Max_iterations
   !
   if (.not.use_cpl_scheme) then
     !
     ! [***] Hermitian case: 
     !
     !|Vm+1> = H |Vm>
     !
     call  K_multiply_by_V(Vn,Vnp1,1)
     !
     ! A(n) = <Vm|Vm+1>
     !
     call K_dot_product(v0_mod_,Vn,Vnp1,1) 
     Af(it)=real(v0_mod_,SP)
     !
     !|Vm+1> = |Vm+1> - A(n)|Vm> - B(n)|Vm-1>
     !
     do i_g = 1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Af(it)*cONE,Vn(i_g)%fragment,Vnp1(i_g)%fragment)
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment,Vnp1(i_g)%fragment)
     end do
     !
     ! B(n+1)= <Vm+1|Vm+1>^(1/2)
     !
     call K_dot_product(v0_mod_,Vnp1,Vnp1,1) 
     Bf(it+1)=sqrt(real(v0_mod_,SP))
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=NaN likely because some eigenvalue of the BSE is negative.')
     !
     !|Vm-1> = |Vm>
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle 
       Vnm1(i_g)%fragment  =Vn(i_g)%fragment
     enddo
     !
     !|Vm> =|Vm+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vn(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
     enddo
     !
   else 
     !
     ! [***] Non-Hermitian case: 
     !
     if (.not.l_BS_ares_asymm) then
       !
       ! A(n) = <Vm+1|F|Vm+1> (that is <Vm|HFH|Vm>), 
       !      =  0   by symmetry 
       !
       ItParity=(-1)**it
       Af(it)=0._SP
       !
     else
       !
       !|Vm+1> = H |Vm>
       !call  K_multiply_by_V(Vn,Vnp1,1)
       !
       ! A(n) = <Vm+1|F|Vm+1>
       call K_dot_product(v0_mod_,Vnp1,Vnp1,1) 
       Af(it)=real(v0_mod_,SP)
       !
     endif
     !
     !|Vm+1> = |Vm+1> - A(n)|Vm> - B(n)|Vm-1>
     !
     if (l_BS_ares_asymm) then
       do i_g = 1,BS_nT_grps
         if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
         call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Af(it)*cONE,Vn(i_g)%fragment,Vnp1(i_g)%fragment)
       enddo
     endif
     !
     do i_g = 1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       call V_by_V_plus_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment,Vnp1(i_g)%fragment)
     end do
     !
     !
     ! |Vm-1> = |Vm> 
     ! |Vm>   = |Vm+1> 
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vnm1(i_g)%fragment = Vn(i_g)%fragment
       Vn(i_g)%fragment   = Vnp1(i_g)%fragment
     enddo
     !
     !|Vm+1> = H |Vm>
     !
     call K_multiply_by_V(Vn,Vnp1,ItParity)
     !
     ! B(n+1)= <Vm|F|Vm+1>^(1/2) (that is <Vm|FH|Vm>^(1/2))
     !       = (2*Re(<Vm|Vmp1>))^(1/2) by symmetry, 
     !          where the dot_product is just on eh pair space 
     !
     call K_dot_product(v0_mod_,Vn,Vnp1,1) 
     if (.not.l_BS_ares_asymm) Bf(it+1)=sqrt(2*real(v0_mod_,SP))
     if (     l_BS_ares_asymm) Bf(it+1)=sqrt(  real(v0_mod_,SP))
     ! Simple test for INF
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=INF likely because some eigenvalue of the BSE is negative.')
     ! Tests for NaN
     if (NAN(Bf(it+1)))  call error(' Bf=NaN likely because some eigenvalue of BSE is negative.')
     !
     !|Vm> =|Vm+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vnp1(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
       Vn(i_g)%fragment    =Vn(i_g)%fragment/Bf(it+1)
     enddo
     !
     ! This is working, despite the call of K_dot_prodict with 1 here
     ! From equations I would have expected 0 to be needed ...
     if (.not.use_left_and_right) call K_dot_product(v0_mod_,V0,Vn,1)
#if defined _KERR
     if (     use_left_and_right) call K_dot_product(v0_mod_,V0_right,Vn,1) 
#endif
     !
     if (.not.l_BS_ares_asymm) then
       ! Using symmetry arguments
       ! Cf=(1-ItParity)Re(<dipoles|Vm>) + i*(1+ItParity)Im(<dipoles|Vm>) 
       ! with all product evaluated just in the eh pairs subspace 
       Cf(it+1)=   ((1._SP-real(ItParity,SP))*real(v0_mod_,SP))/Haydock_v0_mod +&
       &        cI*((1._SP+real(ItParity,SP))*aimag(v0_mod_))  /Haydock_v0_mod
     else
       Cf(it+1)=   v0_mod_/Haydock_v0_mod 
     endif
     !
   endif
   !
   if (it>2) then
     !
     if (use_cpl_scheme) Cf_size=it
     call K_Haydock_response(iq,it,Cf_size,mode,W,Af(:it),Bf(:it+1),Cf(:Cf_size),reached_threshold)
     !
     BSS_desc%rval(1,i_desc_acc) =reached_threshold
     BSS_desc%ival(1,i_desc_iter)=it
     !
     call K_output_file(iq,"Haydock")
     dumb_ch='[Haydock] Iteration '//trim(intc(it))//' Accuracy '//&
&            trim(real2ch(reached_threshold))//'|'//trim(real2ch(Haydock_threshold))
     if (tty_size>0.and.(it_on_disk==0.or. it_on_disk< it-1)) call LIVE_message(dumb_ch,"r","","%s")
     if (tty_size>0.and.(it_on_disk> 0.and.it_on_disk==it-1)) call LIVE_message(dumb_ch,"n","","%s")
     if (tty_size<0) call LIVE_message(dumb_ch,"n","","%s")
     !
   else
     !
     dumb_ch='[Haydock] Iteration '//trim(intc(it))
     call LIVE_message(dumb_ch,"n","","%s")
     !
   endif
   !
   Haydock_converged=(reached_threshold>0.and.reached_threshold<abs(Haydock_threshold)) .or. STOP_now(.FALSE.)
   !
   if ( mod(it,Haydock_iterIO)==0 .or. Haydock_converged) then
     !
     ! Haydock Coefficients and restart vectors to disk
     !--------------------------------------------------
     !
     YAMBO_ALLOC_P(Vn_f,(HAYVEC_size))
     YAMBO_ALLOC_P(Vnm1_f,(HAYVEC_size))
     if (use_cpl_scheme) then
       YAMBO_ALLOC_P(Vnp1_f,(HAYVEC_size))
     endif
     !
     call HAYVEC_gather(Vnm1,Vnm1_f,HAYVEC_size)
     call HAYVEC_gather(Vn,Vn_f,HAYVEC_size)
     if (use_cpl_scheme) then
       call HAYVEC_gather(Vnp1,Vnp1_f,HAYVEC_size) 
       Cf_size=it+1
     endif
     !
     call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
     io_err=io_BSS_Haydock(ID,it,reached_threshold,mode,Af(:it),Bf(:it+1),Cf(:Cf_size),Vnm1_f,Vn_f,Vnp1_f)
     !
     YAMBO_FREE_P(Vn_f)
     YAMBO_FREE_P(Vnm1_f)
     if (use_cpl_scheme) then
       YAMBO_FREE_P(Vnp1_f)
     endif
     !
   endif
   !
   if (Haydock_converged) then
     !
     call msg('r','Accuracy (reached)   [o/o]',reached_threshold)
     call msg('r','Iterations                ',it)
     exit
     !
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
#if defined _KERR
   if (allocated(V0_right)) deallocate(V0_right)
#endif
   YAMBO_FREE(Cf)
 end if
 !
 call PARALLEL_Haydock_VEC_COMMs('reset')
 !
 ! This is allocated in K_Transitions setup
 YAMBO_FREE(PAR_IND_T_Haydock%element_1D)
 !
 call timing('Haydock Solver',OPR='stop')
 !
 contains
   !
   subroutine HAYVEC_gather(V,fullV,fullV_size)
     !
     ! Gather the Haydock vector fragments into one 
     ! large vector of full dimension  
     ! on the master for I/O 
     ! 
     use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_T_groups, &
                             PAR_COM_eh_INDEX,PP_redux_wait
     type(Haydock_vector),intent(in) :: V(BS_nT_grps)
     integer,    intent(in)  :: fullV_size
     complex(SP),intent(out) :: fullV(fullV_size)
     ! ws: 
     integer                 :: istart_g,istop_g
     !
     fullV = cZERO
     istop_g = 0
     !
     ! In this loop I need T_groups indexes since I do not want duplicates 
     !
     do i_g=1,BS_nT_grps
       istart_g = istop_g + 1 
       istop_g  = istart_g + BS_T_grp(i_g)%size - 1
       if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle      
       fullV(istart_g:istop_g) = V(i_g)%fragment(1:BS_T_grp(i_g)%size)
     enddo
     !
     call PP_redux_wait( fullV ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait( fullV ,COMM=PAR_COM_eh_INDEX%COMM )
     !
   end subroutine HAYVEC_gather  
   !
   subroutine HAYVEC_scatter(V,fullV,fullV_size)
     !
     ! Scatter the full dimension vector on the master (read from disk/restart)
     ! into Haydock vector fragments 
     ! 
     use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX, &
                             PAR_COM_eh_INDEX
     integer,    intent(in) :: fullV_size
     complex(SP),intent(in) :: fullV(fullV_size)
     type(Haydock_vector),intent(inout) :: V(BS_nT_grps)
     ! ws: 
     integer                 :: istart_g,istop_g
     !
     ! In this loop I need T_groups Haydock since I do need duplicates 
     !
     call PP_bcast(fullV,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)  
     call PP_bcast(fullV,0,COMM=PAR_COM_eh_INDEX%COMM)  
     istop_g = 0
     !
     do i_g=1,BS_nT_grps
       istart_g = istop_g + 1 
       istop_g  = istart_g + BS_T_grp(i_g)%size - 1
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       V(i_g)%fragment(1:BS_T_grp(i_g)%size) = fullV(istart_g:istop_g)  
     enddo
     !
   end subroutine HAYVEC_scatter
   !
end subroutine
