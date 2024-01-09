!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS IMA
!
subroutine K_Haydock(iq,W,OBS,Xk)
 !
 use pars,           ONLY:SP,schlen,cI,cZERO,cONE
 use functions,      ONLY:NAN
 use X_m,            ONLY:global_gauge,X_OUTPUT_driver
 use wrapper_omp,    ONLY:V_plus_alpha_V_omp
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,isec
 use BS_solvers,     ONLY:BSS_desc,Haydock_threshold,io_BSS_Haydock,&
&                         Haydock_vector,BS_HAYVEC_alloc,BS_HAYVEC_free,Haydock_terminator,Haydock_v0_mod, &
&                         Haydock_iterIO,Haydock_iterMAX,HAYVEC_size,BSS_uses_DbGd,FG_factor
 use BS,             ONLY:BS_K_Coupling,BS_Blocks_symmetrize_K,BS_T_grp,BS_nT_grps,BS_K_dim,&
&                         l_BS_ares_from_res,BS_H_dim,BS_res_ares_n_mat,BS_nkFGbz_in_Tgrp
 use LIVE_t,         ONLY:LIVE_message
 use stderr,         ONLY:tty_size,intc,real2ch
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,OP_WR_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 use parallel_int,   ONLY:PP_bcast
 use parallel_m,     ONLY:PAR_IND_T_groups,PAR_IND_T_Haydock,PAR_COM_T_Haydock,PAR_COM_Xk_ibz_INDEX, &
                          PAR_COM_eh_INDEX
 use timing_m,       ONLY:timing
 use descriptors,    ONLY:IO_desc_add
 use R_lattice,      ONLY:bz_samp
 !
#include<memory.h>
 !
 type(w_samp), intent(in) :: W
 integer,      intent(in) :: iq
 character(*), intent(in) :: OBS
 type(bz_samp),intent(in) :: Xk
 !
 ! Work Space
 !
 integer            ::it,it_on_disk,i_g,ID,io_err,i_desc_acc,i_desc_iter
 type(Haydock_vector),allocatable :: Vn(:),Vnm1(:),Vnp1(:),V0(:),V_tmp(:)
 complex(SP), pointer :: Vn_f(:)   => null()
 complex(SP), pointer :: Vnm1_f(:) => null()
 complex(SP), pointer :: Vnp1_f(:) => null()
 complex(SP), allocatable :: Cf(:) 
 !
 type(Haydock_vector),allocatable :: W0(:)
 integer    :: i_ColFG,i_T,i_k_bz
 !
 real(SP)           ::reached_threshold
 real(SP)           ::Bf(Haydock_iterMAX+1),Af(Haydock_iterMAX)
 integer            ::ItParity,Cf_size,Haydock_n_converged,Haydock_required_converged
 complex(SP)        ::v0_mod_
 character(schlen)  ::dumb_ch
 logical            ::impose_pseudo_herm_scheme,use_left_and_right,velocity_correction,tmp_converged
 logical, external  ::STOP_now
 character(1)       ::sec
 character(11)      ::scheme
 !
 if (BS_res_ares_n_mat==2) call error(" Haydock not coded for case  with two BSE matricies")
 !
 ! Initialize & write the output file 
 !====================================
 call X_OUTPUT_driver("DEF INIT",solver="Haydock") 
 call K_OUTPUT(iq,W,"open Haydock close ",trim(OBS))
 !
 call PARALLEL_Haydock_VEC_COMMs('assign')
 !
 call timing('Haydock Solver',OPR='start')
 !
 Haydock_n_converged=0
 Haydock_required_converged=2
 !
 call BS_Blocks_symmetrize_K()
 !
 ! Use terminator?
 if (.not.Haydock_terminator) call parser('BSHayTer',Haydock_terminator)
 ! Developer option
 call parser('ImposePseudoHerm',impose_pseudo_herm_scheme)
 !
 ! Which approach
 use_left_and_right=.false.
 scheme="hermitian"
 if(BS_K_Coupling.or.impose_pseudo_herm_scheme) scheme="pseudo-herm"
 use_left_and_right=trim(OBS)=="kerr".or.trim(OBS)=="dichroism"
 velocity_correction= trim(global_gauge)=="velocity" .and. (trim(OBS)=="optics" .or. trim(OBS)=="kerr")
 !
 if (isec(2)/=0) sec="="
 if (isec(2)==0) sec="+"
 call section(sec,'Haydock Solver in the '//trim(OBS)//' basis @q'//trim(intc(iq))//" using the "//trim(scheme)//" scheme")
 !
 if (     l_BS_ares_from_res) HAYVEC_size = BS_K_dim(1)
 if (.not.l_BS_ares_from_res) then
   if(.not.BS_K_coupling) HAYVEC_size = BS_K_dim(1)
   if(     BS_K_coupling) HAYVEC_size = BS_H_dim
 endif
 !
 if (BSS_uses_DbGd) then
   call msg('r','Using Double Grid method') 
   call msg('r','Number of Coarse grid BZ kpts : '//trim(intc(Xk%nbz))) 
   call msg('r','Number of Fine grid BZ kpts   : '//trim(intc(Xk%FGbz%N))) 
   call LIVE_message('[Haydock] Using Double Grid method') 
   call LIVE_message('[Haydock] Number of Coarse grid BZ kpts : '//trim(intc(Xk%nbz))) 
   call LIVE_message('[Haydock] Number of Fine grid BZ kpts   : '//trim(intc(Xk%FGbz%N))) 
 end if
 !
 ! Allocate Haydock vectors:
 !
 if (BSS_uses_DbGd) then
   do i_g=1,BS_nT_grps
   HAYVEC_size = HAYVEC_size + BS_T_grp(i_g)%size * (BS_nkFGbz_in_Tgrp(i_g)-1)
   enddo
 end if
 !
 allocate(V0(BS_nT_grps))
 call BS_HAYVEC_alloc(V0,BS_nkFGbz_in_Tgrp)
 !
 if(velocity_correction) then
   allocate(V_tmp(BS_nT_grps))
   call BS_HAYVEC_alloc(V_tmp,BS_nkFGbz_in_Tgrp)
 endif
 !
 if(use_left_and_right) then
   allocate(W0(BS_nT_grps))
   call BS_HAYVEC_alloc(W0,BS_nkFGbz_in_Tgrp)
 endif
 !
 ItParity=1
 it_on_disk=0
 reached_threshold=100._SP
 !
 Cf_size=2
 if (trim(scheme)=="pseudo-herm".or.use_left_and_right) Cf_size=Haydock_iterMAX+1
 YAMBO_ALLOC(Cf,(Cf_size))
 Cf=cZERO
 !
 allocate(Vn(BS_nT_grps))
 allocate(Vnp1(BS_nT_grps))
 allocate(Vnm1(BS_nT_grps))
 call BS_HAYVEC_alloc(Vn,BS_nkFGbz_in_Tgrp)
 call BS_HAYVEC_alloc(Vnm1,BS_nkFGbz_in_Tgrp)
 call BS_HAYVEC_alloc(Vnp1,BS_nkFGbz_in_Tgrp)
 !
 ! Haydock Coefficients and restart vector from disk
 !=================================================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=io_BSS_Haydock(ID,iq,1,reached_threshold,OBS,Af(1),Bf(:2),Cf(:2),Vnm1_f,Vn_f,Vnp1_f)
 !
 if (io_err>0) then
   !
   YAMBO_ALLOC_P(Vn_f,(HAYVEC_size))
   YAMBO_ALLOC_P(Vnm1_f,(HAYVEC_size))
   if (scheme=="pseudo-herm") then
     YAMBO_ALLOC_P(Vnp1_f,(HAYVEC_size))
   endif
   !
   it_on_disk=io_err
   if (scheme=="pseudo-herm") Cf_size=it_on_disk
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
   io_err=io_BSS_Haydock(ID,iq,it_on_disk,reached_threshold,OBS,Af(:it_on_disk),&
&                        Bf(:it_on_disk+1),Cf(:Cf_size),Vnm1_f,Vn_f,Vnp1_f)
   !
   call msg('s','[Haydock] Restart DB with accuracy ',reached_threshold)
   !  
   call PP_bcast(Af,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_bcast(Af,0,COMM=PAR_COM_eh_INDEX%COMM)
   call PP_bcast(Bf,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_bcast(Bf,0,COMM=PAR_COM_eh_INDEX%COMM)
   if (scheme=="pseudo-herm".or.use_left_and_right) then
     call PP_bcast(Cf,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
     call PP_bcast(Cf,0,COMM=PAR_COM_eh_INDEX%COMM)
   endif
   !
   call K_Haydock_scatter(Vn,Vn_f,HAYVEC_size,BS_nkFGbz_in_Tgrp)
   call K_Haydock_scatter(Vnm1,Vnm1_f,HAYVEC_size,BS_nkFGbz_in_Tgrp)
   if (scheme=="pseudo-herm") call K_Haydock_scatter(Vnp1,Vnp1_f,HAYVEC_size,BS_nkFGbz_in_Tgrp)
   !
   YAMBO_FREE_P(Vnm1_f)
   YAMBO_FREE_P(Vn_f)
   if (scheme=="pseudo-herm") then
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
 call msg('r','Accuracy (requested)      ',Haydock_threshold,"[o/o]")
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
     if(use_left_and_right) W0(i_g)%fragment  = cZERO
     if(velocity_correction) V_tmp(i_g)%fragment = cZERO
     !
   enddo
   !
   Af= 0._SP
   Bf= 0._SP
   !
 end if
 !
 ! IAM (2022/01/19):
 ! V0 in DbGd initialised as V0 in coarse grid
 !
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
   do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
     if (trim(OBS)=="optics")      V0(i_g)%fragment(:,i_ColFG) = BS_T_grp(i_g)%dipoles_opt(1,:,1)
     if (trim(OBS)=="kerr")        V0(i_g)%fragment(:,i_ColFG) = BS_T_grp(i_g)%dipoles_opt(2,:,1)
     if (trim(OBS)=="magnons")     V0(i_g)%fragment(:,i_ColFG) = BS_T_grp(i_g)%dipoles_mag(1,:,1)
     V0(i_g)%fragment(:,i_ColFG) = V0(i_g)%fragment(:,i_ColFG)*sqrt(cmplx(BS_T_grp(i_g)%f_RES,kind=SP))
   enddo
   !
   ! IA (2022/01/19):
   ! Setting some elements to 0 as the number of mapped points 
   ! is not the same for every k point in the transition group.
   ! Within a given transition group there may be transitions
   ! with, e.g., 128 or 129 mapped points
   !
   if (BSS_uses_DbGd) then
     do i_T=1,BS_T_grp(i_g)%size
       i_k_bz=BS_T_grp(i_g)%table(i_T,1)
       do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
         if (i_ColFG <= Xk%FGbz%N_mapped(i_k_bz)) cycle
         V0(i_g)%fragment(i_T,i_ColFG)=cZERO
       enddo
     enddo
   endif
   if(velocity_correction) V_tmp(i_g)%fragment = V0(i_g)%fragment/BS_T_grp(i_g)%E
 enddo
 !
 do i_g = 1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
#if defined _MPI
   if(     velocity_correction) call PP_bcast(V_tmp(i_g)%fragment,0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
   if(.not.velocity_correction) call PP_bcast(   V0(i_g)%fragment,0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
#endif
 enddo
 !
 if(velocity_correction) call K_multiply_by_V(V_tmp,V0,1)
 !
 if(use_left_and_right) then
   !
   do i_g=1,BS_nT_grps
     if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
     do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
       if (trim(OBS)=="kerr") W0(i_g)%fragment(:,i_ColFG) = BS_T_grp(i_g)%dipoles_opt(1,:,1)
       W0(i_g)%fragment(:,i_ColFG) = W0(i_g)%fragment(:,i_ColFG)*(sqrt(cmplx(BS_T_grp(i_g)%f_RES,kind=SP)))
     enddo
       if(velocity_correction) V_tmp(i_g)%fragment = W0(i_g)%fragment/BS_T_grp(i_g)%E
   enddo
   !
   do i_g = 1,BS_nT_grps
     if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
#if defined _MPI
     if(     velocity_correction) call PP_bcast(V_tmp(i_g)%fragment,0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
     if(.not.velocity_correction) call PP_bcast(   W0(i_g)%fragment,0,COMM=PAR_COM_T_Haydock(i_g)%COMM)
#endif
   enddo
   !
   if(velocity_correction) call K_multiply_by_V(V_tmp,W0,1)
   !
 endif
 !
 if(velocity_correction) then
   call BS_HAYVEC_free(V_tmp)
   deallocate(V_tmp)
 endif
 !
 if (it_on_disk==0) then
   !
   if (scheme=="pseudo-herm") then       ! [***] Initialization non Hermitian case
     !
     ! Before starting iterate one needs
     !
     ! | Vn > =     |V0>/(<V0|F(H|V0>))
     ! | Vnp1 > = H |Vn>
     !
     Cf= cZERO
     !
     call K_multiply_by_V(V0,Vnp1,-1)
     call K_dot_product(v0_mod_,V0,Vnp1,1)
     !
     ! For the coupling case if A = -R* (l_BS_ares_from_res=.FALSE.), V0 = (u0, -iu0*)
     ! At each iteration however the relation changes V1= (u1, iu1*),
     ! this is why the ItParity is needed 
     !
     if(     l_BS_ares_from_res) Haydock_v0_mod=sqrt(2*real(v0_mod_))
     if(.not.l_BS_ares_from_res) Haydock_v0_mod=sqrt(  real(v0_mod_))
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       V0(i_g)%fragment   = V0(i_g)%fragment/Haydock_v0_mod
       if (use_left_and_right) W0(i_g)%fragment = W0(i_g)%fragment/Haydock_v0_mod
       Vn(i_g)%fragment   = V0(i_g)%fragment
       Vnp1(i_g)%fragment = Vnp1(i_g)%fragment/Haydock_v0_mod
     enddo
     !
     ! <w0|u0>=<V0|F|V0>  since <w0|=<V0|F
     ! Is it correct that also V0 was renormalized ??
     if (.not.use_left_and_right) call K_dot_product(v0_mod_,V0,V0,1)
     if (     use_left_and_right) call K_dot_product(v0_mod_,W0,V0,1)
     !
     if(     l_BS_ares_from_res) Cf(1)=2*cI*aimag(v0_mod_)
     if(.not.l_BS_ares_from_res) Cf(1)=           v0_mod_
     !
   else                           ! [***] Initialization Hermitian case
     !
     ! it=1:|Vn>=|q->0>/(<q->0|q->0>)^(1/2)
     !
     call K_dot_product(v0_mod_,V0,V0,1)
     Haydock_v0_mod=sqrt(real(v0_mod_,SP))
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       V0(i_g)%fragment  = V0(i_g)%fragment/Haydock_v0_mod
       if (use_left_and_right) W0(i_g)%fragment = W0(i_g)%fragment/Haydock_v0_mod
       Vn(i_g)%fragment  = V0(i_g)%fragment
     enddo
     !
     if (use_left_and_right) then
       call K_dot_product(v0_mod_,W0,V0,1)
       Cf(1)= v0_mod_
     endif
     !
   endif
   !
 endif
 !
 ! DEBUG <
 ! write(*,*) "Haydock_v0_mod= ", Haydock_v0_mod
 ! DEBUG >
 !
 ! [***] Loop ...
 !
 do it=it_on_disk+1,Haydock_iterMAX
   !
   if (trim(scheme)=="hermitian") then  ! [***] Hermitian case:
     !
     !|Vn+1> = H |Vn>
     !
     call  K_multiply_by_V(Vn,Vnp1,1)
     !
     ! A(n) = <Vn|Vn+1>
     !
     call K_dot_product(v0_mod_,Vn,Vnp1,1)
     Af(it)=real(v0_mod_,SP)
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
     do i_g = 1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
         call V_plus_alpha_V_omp(BS_T_grp(i_g)%size,-Af(it)*cONE,Vn(i_g)%fragment(:,i_ColFG),Vnp1(i_g)%fragment(:,i_ColFG))
         call V_plus_alpha_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment(:,i_ColFG),Vnp1(i_g)%fragment(:,i_ColFG))
       enddo
     end do
     !
     ! B(n+1)= <Vn+1|Vn+1>^(1/2)
     !
     call K_dot_product(v0_mod_,Vnp1,Vnp1,1)
     Bf(it+1)=sqrt(real(v0_mod_,SP))
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=NaN likely because some eigenvalue of the BSE is negative.')
     !
     !|Vn-1> = |Vn>
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vnm1(i_g)%fragment  =Vn(i_g)%fragment
     enddo
     !
     !|Vn> =|Vn+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vn(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
     enddo
     !
     ! DEBUG <
     !! check orthonotmality
     !call K_dot_product(v0_mod_,V0,Vn,1)
     !write(100,*) "Ortho check = ",v0_mod_
     ! DEBUG
     !
     if (use_left_and_right) then
       Cf_size=it+1
       call K_dot_product(v0_mod_,W0,Vn,1)
       Cf(it+1)= v0_mod_
     endif
     !
   else                           ! [***] Non-Hermitian case:
     !
     ! A(n) = <Vn+1|F|Vn+1> (that is <Vn|HFH|Vn>),
     !
     if (l_BS_ares_from_res) then
       ! A(n) =  0   by symmetry
       Af(it)=0._SP
       ItParity=(-1)**(it+1)
     else
       call K_dot_product(v0_mod_,Vnp1,Vnp1,1)
       Af(it)=real(v0_mod_,SP)
     endif
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
     if (.not.l_BS_ares_from_res) then
       do i_g = 1,BS_nT_grps
         if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
         do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
           call V_plus_alpha_V_omp(BS_T_grp(i_g)%size,-Af(it)*cONE,Vn(i_g)%fragment(:,i_ColFG),Vnp1(i_g)%fragment(:,i_ColFG))
         enddo 
       enddo
     endif
     !
     do i_g = 1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_g)
         call V_plus_alpha_V_omp(BS_T_grp(i_g)%size,-Bf(it)*cONE,Vnm1(i_g)%fragment(:,i_ColFG),Vnp1(i_g)%fragment(:,i_ColFG))
       enddo
     end do
     !
     ! |Vn-1> = |Vn>
     ! |Vn>   = |Vn+1>
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vnm1(i_g)%fragment = Vn(i_g)%fragment
       Vn(i_g)%fragment   = Vnp1(i_g)%fragment
     enddo
     !
     !|Vn+1> = H |Vn>
     !
     call K_multiply_by_V(Vn,Vnp1,ItParity)
     !
     ! B(n+1)= <Vn|F|Vn+1>^(1/2) (that is <Vn|FH|Vn>^(1/2))
     !
     call K_dot_product(v0_mod_,Vn,Vnp1,1) 
     !       = (2*Re(<Vn|Vnp1>))^(1/2) by symmetry,
     !          where the dot_product is just on eh pair space
     if (     l_BS_ares_from_res) Bf(it+1)=sqrt(2*real(v0_mod_,SP))
     if (.not.l_BS_ares_from_res) Bf(it+1)=sqrt(  real(v0_mod_,SP))
     ! Simple test for INF
     if( Bf(it+1)==(Bf(it+1)+1._SP) ) call error('Bf=INF likely because some eigenvalue of the BSE is negative.')
     ! Tests for NaN
     if (NAN(Bf(it+1)))  call error(' Bf=NaN likely because some eigenvalue of BSE is negative.')
     !
     !|Vn> =|Vn+1> / B(n+1)
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       Vnp1(i_g)%fragment  =Vnp1(i_g)%fragment/Bf(it+1)
       Vn(i_g)%fragment    =Vn(i_g)%fragment/Bf(it+1)
     enddo
     !
     ! Cf(n) = <w0|q_n> = <V0|F|Vn>
     ! F is needed since <w0| = <V0| F
     !
     Cf_size=it+1
     !
     if (.not.use_left_and_right) call K_dot_product(v0_mod_,V0,Vn,1)
     if (     use_left_and_right) call K_dot_product(v0_mod_,W0,Vn,1)
     !
     if (l_BS_ares_from_res) then
       ! Using symmetry arguments
       ! Cf=(1-ItParity)Re(<dipoles|Vn>) + i*(1+ItParity)Im(<dipoles|Vn>)
       ! with all product evaluated just in the eh pairs subspace
       Cf(it+1)=   ((1._SP+real(ItParity,SP))*real(v0_mod_,SP)) +&
       &        cI*((1._SP-real(ItParity,SP))*aimag(v0_mod_))
     else
       Cf(it+1)=   v0_mod_
     endif
     !
   endif
   !
   !
   if (it>2) then
     !
     call K_Haydock_response(iq,it,Cf_size-1,OBS,scheme,W,Af(:it),Bf(:it+1),Cf(:Cf_size-1),reached_threshold,FG_factor)
     !
     BSS_desc%rval(1,i_desc_acc) =reached_threshold
     BSS_desc%ival(1,i_desc_iter)=it
     !
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
   ! DEBUG<
   !write(*,*) it,"Af= ",Af(it)," ; Bf= ",Bf(it)," ; Cf= ",real(Cf(Cf_size-1)),aimag(Cf(Cf_size-1)),reached_threshold
   ! DEBUG>
   !
   tmp_converged=(reached_threshold<abs(Haydock_threshold)) .or. STOP_now(.FALSE.)
   !
   if(     tmp_converged) Haydock_n_converged=Haydock_n_converged+1
   if(.not.tmp_converged) Haydock_n_converged=0
   !
   if ( mod(it,max(1,Haydock_iterIO/10))==0) call K_OUTPUT(iq,W,"override dump Haydock close",OBS)
   !
   if ( mod(it,Haydock_iterIO)==0 .or. Haydock_n_converged==Haydock_required_converged) then
     !
     ! Haydock Coefficients and restart vectors to disk
     !--------------------------------------------------
     !
     YAMBO_ALLOC_P(Vn_f,(HAYVEC_size))
     YAMBO_ALLOC_P(Vnm1_f,(HAYVEC_size))
     if (trim(scheme)=="pseudo-herm") then
       YAMBO_ALLOC_P(Vnp1_f,(HAYVEC_size))
     endif
     !
     call K_Haydock_gather(Vnm1,Vnm1_f,HAYVEC_size,BS_nkFGbz_in_Tgrp)
     call K_Haydock_gather(Vn,Vn_f,HAYVEC_size,BS_nkFGbz_in_Tgrp)
     if (trim(scheme)=="pseudo-herm") then
       call K_Haydock_gather(Vnp1,Vnp1_f,HAYVEC_size,BS_nkFGbz_in_Tgrp) 
     endif
     !
     call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
     io_err=io_BSS_Haydock(ID,iq,it,reached_threshold,OBS,Af(:it),Bf(:it+1),Cf(:Cf_size),Vnm1_f,Vn_f,Vnp1_f)
     !
     YAMBO_FREE_P(Vn_f)
     YAMBO_FREE_P(Vnm1_f)
     if (trim(scheme)=="pseudo-herm") then
       YAMBO_FREE_P(Vnp1_f)
     endif
     !
   endif
   !
   if (Haydock_n_converged==Haydock_required_converged) then
     !
     call K_OUTPUT(iq,W,"override dump Haydock close ",OBS)
     !
     call msg('r','Accuracy (reached)        ',reached_threshold,"[o/o]")
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
 call BS_HAYVEC_free(V0)
 deallocate(V0)
 if (use_left_and_right)  then
   call BS_HAYVEC_free(W0)
   deallocate(W0)
 endif
 if (use_left_and_right.or.trim(scheme)=="pseudo-herm")  then
   YAMBO_FREE(Cf)
 end if
 !
 call PARALLEL_Haydock_VEC_COMMs('reset')
 !
 call timing('Haydock Solver',OPR='stop')
 !
end subroutine
