!
! Copyright (C) 2000-2013 A. Marini, M. Gruning and the YAMBO team 
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
subroutine K_Haydock(iq,W)
 !
 use pars,           ONLY:SP,pi,schlen,cI
 use com,            ONLY:error
 use wrapper,        ONLY:V_by_V_plus_V
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_epsilon
 use R_lattice,      ONLY:d3k_factor,q_norm
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,isec
 use BS,             ONLY:BSS_n_descs,BS_K_dim,BS_mat,Haydock_treshold,&
&                         BSS_rhoq0,BSS_description,Haydock_v0_mod,&
&                         BS_DB_is_fragmented, BS_eh_f,ioBSS_Haydock,&
&                         BS_K_Coupling,Haydock_terminator,&
&                         K_BSmat_by_V,K_BSmat_by_V_Haydock,Haydock_fast
#if defined _KERR
 use KERR,           ONLY:BSS_rhoq0_kerr,iw_ref,B_Hall
 use drivers,        ONLY:l_kerr,l_anomalous_Hall
#endif
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use electrons,      ONLY:spin_occ
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use parser_m,       ONLY:parser
 implicit none
 type(w_samp)  ::W
 integer       ::iq
 !
 ! Work Space
 !
 integer            ::it,it_on_disk,i1
 integer, parameter ::Max_iterations=1000
 complex(SP),allocatable :: Vn(:),Vnm1(:),Vnp1(:)
 complex(SP),allocatable :: Cf(:)
#if defined _KERR
 complex(SP),allocatable :: Cf_kerr(:)
#endif
 real(SP)           ::reached_treshold, Bf(Max_iterations),Af(Max_iterations)
 real(SP)           ::ItParity
 character(schlen)  ::dumb_ch
 logical, external  ::stop_now
 logical            ::use_cpl_scheme
 logical            ::cpl_scheme_fast
 !
 ! I/O
 !
 integer           :: ID,io_err
 !
 if (isec(2)/=0) then
   call section('=','Haydock solver')
 else if (isec(2)==0) then
   call section('+','Haydock solver')
 endif
 !
 ! Use terminator?
 if (.not.Haydock_terminator) call parser('BSHayTer',Haydock_terminator)
 !
 ! Which approach
 call parser('BSHayFast',Haydock_fast)
 use_cpl_scheme=BS_K_Coupling
 cpl_scheme_fast=Haydock_fast
#if defined _KERR
 use_cpl_scheme=BS_K_Coupling.or.l_kerr
 cpl_scheme_fast=(BS_K_Coupling.and.Haydock_fast).or.(.not.BS_K_Coupling.and.l_kerr)
#endif
 !
 if (use_cpl_scheme) then
   !
   allocate(Cf(Max_iterations+1))
   if(     cpl_scheme_fast) allocate(Vn(BS_K_dim),Vnm1(BS_K_dim),Vnp1(BS_K_dim))
   if(.not.cpl_scheme_fast) allocate(Vn(2*BS_K_dim),Vnm1(2*BS_K_dim),Vnp1(2*BS_K_dim))
#if defined _KERR
   if(l_kerr) allocate(Cf_kerr(Max_iterations+1))
#endif
   !
 else
   allocate(Vn(BS_K_dim),Vnm1(BS_K_dim),Vnp1(BS_K_dim))
 end if
 !
 !
 !Haydock Coefficients and restart vector on disk
 !
 it_on_disk=0
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 !
 if (use_cpl_scheme) then 
   !
#if defined _KERR
   if(l_kerr) then
     if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1),  &
&                                               Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1),Cf_kerr=Cf_kerr(1))
     if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,1,2*BS_K_dim,reached_treshold,Af(1),Bf(1),&
&                                               Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1),Cf_kerr=Cf_kerr(1))
   else
#endif
     if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1))   
     if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,1,2*BS_K_dim,reached_treshold,Af(1),Bf(1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1))   
#if defined _KERR
   endif
#endif
   !
 else
   io_err=ioBSS_Haydock(ID,1,BS_K_dim,reached_treshold,Af(1),Bf(1),Vnm1,Vn)
   !
 endif
 !
 if (io_err>0) then 
   !
   it_on_disk=io_err 
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
   if (use_cpl_scheme) then 
#if defined _KERR
     if(l_kerr) then
       if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,&
&            Af(:it_on_disk),Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1),Cf_kerr=Cf_kerr(:it_on_disk+1))
       if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it_on_disk,2*BS_K_dim,reached_treshold,&
&            Af(:it_on_disk),Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1),Cf_kerr=Cf_kerr(:it_on_disk+1))
     else
#endif
       if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,&
&            Af(:it_on_disk),Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1))
       if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it_on_disk,2*BS_K_dim,reached_treshold,&
&            Af(:it_on_disk),Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1))
#if defined _KERR
   endif
#endif
   else
     io_err=ioBSS_Haydock(ID,it_on_disk,BS_K_dim,reached_treshold,Af(:it_on_disk),&
&            Bf(:it_on_disk+1),Vnm1,Vn)
     call msg('s','[Haydock] Restart DB with accuracy ',reached_treshold)
   end if
   !
 endif
 !
 ! Update Descriptions
 !
 BSS_n_descs=BSS_n_descs+2
 BSS_description(BSS_n_descs-1)=' '
 BSS_description(BSS_n_descs)=' Haydock|Accuracy (requested) [o/o]:'//&
&                             trim(real2ch(Haydock_treshold))
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='        |Accuracy (reached)   [o/o]:'//&
&                             trim(real2ch(0._SP))
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='        |Iteration            [o/o]: 0'
 !
 call msg('r','Use Database fragments    :',BS_DB_is_fragmented)
 if (BS_DB_is_fragmented) call msg('s','[Haydock] Use Database fragments')
 call msg('r','Accuracy (requested) [o/o]:',Haydock_treshold)
 !
 ! [***] Initialization non Hermitian case
 !
 !
 if (it_on_disk==0) then
   Vn=(0.,0.)
   Vnm1=(0.,0.)
   Vnp1=(0.,0.)
   Af= 0.
   Bf= 0.
 end if
 ! 
 if (use_cpl_scheme) then
   !
   ! Before starting iterate one needs
   !
   ! | Vn > =     |q->0>/(<q->0| F (H |q->0>))
   ! | Vnp1 > = H |Vn>
   !
   ItParity = (-1._SP)**it_on_disk
   if (it_on_disk==0) then
     Cf= (0.,0.)
#if defined _KERR
     if(l_kerr) Cf_kerr= (0.,0.)
#endif
     Vn(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)*BS_eh_f(:BS_K_dim)
     !
     if(cpl_scheme_fast) then
       call K_BSmat_by_V_Haydock(iq,it_on_disk,Vn,Vnp1,iter_par=ItParity)
       Haydock_v0_mod=sqrt(2._SP*real(dot_product(Vn,Vnp1)))
       Cf(1) = 0.
#if defined _KERR
       if(l_kerr) Cf_kerr(1)= 2._SP*cI*aimag(dot_product(BSS_rhoq0_kerr(:BS_K_dim),Vn))/Haydock_v0_mod**2._SP
#endif
     else
       Vn(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)*BS_eh_f(:BS_K_dim)
       call K_BSmat_by_V(iq,iter=it_on_disk,Vi=Vn,Vo=Vnp1)
       Haydock_v0_mod=sqrt(fdot_product(Vn,Vnp1))
       Cf(1) = dot_product(BSS_rhoq0,Vn)/Haydock_v0_mod**2._SP
#if defined _KERR
       if(l_kerr) Cf_kerr(1)= dot_product(BSS_rhoq0_kerr,Vn)/Haydock_v0_mod**2._SP
#endif
     endif
     !
     Vnp1(:) = Vnp1(:)/Haydock_v0_mod
     Vn(:)   = Vn(:)/Haydock_v0_mod
     !
   else
     Haydock_v0_mod = Bf(1)
     Bf(1) = 0._SP
   endif     
   !
 else
   !
   ! [***] Initialization Hermitian case
   !
   !
   ! it=1:|Vn>=|q->0>/(<q->0|q->0>)^(1/2)
   !
   Haydock_v0_mod=sqrt(dot_product(BSS_rhoq0,BSS_rhoq0))
   if (it_on_disk==0) forall(i1=1:BS_K_dim) Vn(i1)=BSS_rhoq0(i1)/Haydock_v0_mod
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
     !|Vn+1> = H |Vn>
     !
     call K_BSmat_by_V_Haydock(iq,it-it_on_disk,Vn,Vnp1)
     !if(     cpl_scheme_fast) call K_BSmat_by_V_Haydock(iq,it-it_on_disk,Vn,Vnp1)
     !if(.not.cpl_scheme_fast) call K_BSmat_by_V(iq,iter=it-it_on_disk,Vi=Vn,Vo=Vnp1)
     !
     ! A(n) = <Vn|Vn+1>
     !
     Af(it)=real(dot_product(Vn,Vnp1))
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
     call V_by_V_plus_V(BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,Vnp1)
     call V_by_V_plus_V(BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,Vnp1)
     !
     ! B(n+1)= <Vn+1|Vn+1>^(1/2)
     !
     Bf(it+1)=sqrt(dot_product(Vnp1,Vnp1))
     if( isnan(Bf(it+1)) ) call error('Bf=NaN likely because some eigenvalue of BSE is negative.')
     !
     !|Vn-1> = |Vn>
     !
     Vnm1=Vn
     !
     !|Vn> =|Vn+1> / B(n+1)
     !
     forall(i1=1:BS_K_dim) Vn(i1)=Vnp1(i1)/Bf(it+1)
     !
     ! Non-Hermitian case: 
     !
   else 
     !
     ! A(n) = <Vn+1|F|Vn+1> (that is <Vn|HFH|Vn>), 
     !      =  0   by symmetry 
     !
     ItParity=(-1._SP)**it
     Af(it)=0._SP
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
     if(cpl_scheme_fast) then
       call V_by_V_plus_V(BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,Vnp1)
     else
       call V_by_V_plus_V(2*BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,Vnp1)
       call V_by_V_plus_V(2*BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,Vnp1) 
     endif
     !
     ! |Vn-1> = |Vn> 
     ! |Vn>   = |Vn+1> 
     !
     Vnm1=Vn
     Vn  =Vnp1
     !
     !|Vn+1> = H |Vn>
     !
     if(     cpl_scheme_fast) call K_BSmat_by_V_Haydock(iq,it-it_on_disk,Vn,Vnp1,iter_par=ItParity)
     if(.not.cpl_scheme_fast) call K_BSmat_by_V(iq,iter=it-it_on_disk,Vi=Vn,Vo=Vnp1)
     !
     ! B(n+1)= <Vn|F|Vn+1>^(1/2) (that is <Vn|FH|Vn>^(1/2))
     !       = (2*Re(<Vn|Vnp1>))^(1/2) by symmetry, 
     !          where the dot_product is just on eh pair space 
     !
     if(     cpl_scheme_fast) Bf(it+1)=sqrt(2._SP*real(dot_product(Vn,Vnp1)))
     if(.not.cpl_scheme_fast) Bf(it+1)=sqrt(fdot_product(Vn,Vnp1))
     if( isnan(Bf(it+1)) ) call error(' Bf=NaN likely because some eigenvalue of BSE is negative.')
     !
     !|Vn> =|Vn+1> / B(n+1)
     !
     if(cpl_scheme_fast) then
       forall(i1=1:BS_K_dim) Vn(i1)=Vn(i1)/Bf(it+1)
       forall(i1=1:BS_K_dim) Vnp1(i1)=Vnp1(i1)/Bf(it+1)
       !
       ! Using symmetry arguments
       ! Cf=(1-ItParity)Re(<rhoq0|Vn>) + i*(1+ItParity)Im(<rhoq0|Vn>) 
       ! with all product evaluated just in the eh pairs subspace 
       !
       Cf(it+1)= ((1._SP-ItParity)*real(dot_product(BSS_rhoq0(:BS_K_dim),Vn)))/Haydock_v0_mod
#if defined _KERR
       if(l_kerr) Cf_kerr(it+1)= &
&               ((1._SP+ItParity)*cI*aimag(dot_product(BSS_rhoq0_kerr(:BS_K_dim),Vn)))/Haydock_v0_mod
#endif
     else
       forall(i1=1:2*BS_K_dim) Vn(i1)=Vn(i1)/Bf(it+1)
       forall(i1=1:2*BS_K_dim) Vnp1(i1)=Vnp1(i1)/Bf(it+1)
       Cf(it+1)= dot_product(BSS_rhoq0,Vn)/Haydock_v0_mod
#if defined _KERR
       if(l_kerr) Cf_kerr(it+1)= dot_product(BSS_rhoq0_kerr,Vn)/Haydock_v0_mod
#endif
     endif
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
     if (tty_size>0.and.it_on_disk==0) call live_time_msg("r",dumb_ch,"","%s")
     if (tty_size>0.and.it_on_disk> 0) call live_time_msg("n",dumb_ch,"","%s")
     if (tty_size<0) call live_time_msg("n",dumb_ch,"","%s")
   else
     !
     dumb_ch='[Haydock] Iteration '//trim(intc(it))
     if (tty_size<0) call live_time_msg("n",dumb_ch,"","%s")
     if (tty_size>0.and.it==1) call live_time_msg("n",dumb_ch,"","%s")
     if (tty_size>0.and.it==2) call live_time_msg("r",dumb_ch,"","%s")
     !
   endif
   !
   !Haydock Coefficients and restart vectors on disk
   !
   call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
   if (use_cpl_scheme) then
     Bf(1) = Haydock_v0_mod
#if defined _KERR
     if(l_kerr) then
       if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it,BS_K_dim,reached_treshold,  &
&                            Af(:it),Bf(:it+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1),Cf_kerr=Cf_kerr(:it+1))
       if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it,2*BS_K_dim,reached_treshold,&
&                            Af(:it),Bf(:it+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1),Cf_kerr=Cf_kerr(:it+1))
     else 
#endif
       if(     cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it,BS_K_dim,reached_treshold,  &
&                            Af(:it),Bf(:it+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1))
       if(.not.cpl_scheme_fast) io_err=ioBSS_Haydock(ID,it,2*BS_K_dim,reached_treshold,&
&                            Af(:it),Bf(:it+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1))
#if defined _KERR
     endif
#endif
     Bf(1) = 0._SP
   else
     io_err=ioBSS_Haydock(ID,it,BS_K_dim,reached_treshold,Af(:it),Bf(:it+1),Vnm1,Vn)
   end if
   !
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
 !
 if (allocated(BS_mat)) deallocate(BS_mat)
 if (use_cpl_scheme) deallocate(Cf)
#if defined _KERR
 if (use_cpl_scheme.and.l_kerr) deallocate(Cf_kerr)
#endif
 deallocate(Vn,Vnm1,Vnp1) 
 !
 ! Either if BS_mat is allocated (no fragments) or is not allocated
 ! I need to clean the memory
 !
 call mem_est("BS_mat")
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
     complex(SP) :: X_o(2,it), YbyX(2)
     complex(SP),allocatable :: X_i(:,:)
     !
     ! related to terminator
     !
     complex(SP) :: X_t 
     real(SP)    :: Av1, Av2
     integer, parameter :: RES = 2, COUPL = 1, MIN_ITER = 10
     logical     :: UseTerminator
     !
     ! Co includes the spin_occ factor because Haydock is expected to work
     ! only when ALL BS_eh_f == spin_occ
     !
     Co=real(spin_occ)/(2._SP*pi)**3._SP*d3k_factor*4._SP*pi/q_norm(1)**2._SP*Haydock_v0_mod**2
     X_t =(0._SP,0._SP)
     UseTerminator = .false.  
     !
     if (it > MIN_ITER.and.Haydock_terminator) UseTerminator = .true.
     if (use_cpl_scheme) allocate(X_i(2,it))
     if (Useterminator) then 
       if (.not.use_cpl_scheme) then
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
     do iw=1,W%n(1)
       X_o=(0._SP,0._SP)
       if (UseTerminator) then 
         if (.not.use_cpl_scheme) X_t = terminator(W%p(iw),Av1,Av2,RES)
         if (use_cpl_scheme.and.(mod(it,2)==0)) X_t = terminator(W%p(iw),Av1,Av2,COUPL)
         if (use_cpl_scheme.and.(mod(it,2)==1)) X_t = terminator(W%p(iw),Av2,Av1,COUPL)
       endif
       X_o(:,it)=1._SP/(W%p(iw)-Af(it)-Bf(it+1)**2*X_t)
       do i1=it-1,1,-1
         X_o(1,i1)=1._SP/(W%p(iw)-Af(i1)-Bf(i1+1)**2*X_o(1,i1+1))
       enddo
       do i1=it-2,1,-1
         X_o(2,i1)=1._SP/(W%p(iw)-Af(i1)-Bf(i1+1)**2*X_o(2,i1+1))
       enddo
       if (use_cpl_scheme) then
         X_i(:,1) = -X_o(:,1)
         X_i(:,2) = -Bf(2)*X_o(:,2)*X_i(:,1)
         YbyX(:) = X_i(:,1)*Cf(1) + X_i(:,2)*Cf(2)
         do i1 = 2,it-1
           X_i(1,i1+1) = -Bf(i1+1)*X_o(1,i1+1)*X_i(1,i1)
           YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
         end do
         do i1 = 2,it-2
           X_i(2,i1+1) = -Bf(i1+1)*X_o(2,i1+1)*X_i(2,i1)
           YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
         end do
         X_o(:,1) = YbyX(:)
       endif
       i_eps(1)=2
       i_eps(2)=4
       if(present(ii)) then
         i_eps(1)=3*ii-1
         i_eps(2)=3*ii+1
       endif
       X_epsilon(1,iw)=W%p(iw)
       X_epsilon(i_eps(1),iw)=-X_o(1,1)*Co+1._SP
       X_epsilon(i_eps(2),iw)=-X_o(2,1)*Co+1._SP
       if (present(ii).and.ii==2) X_epsilon(i_eps(:),iw)=-(X_epsilon(i_eps(:),iw)-1)
       if (Haydock_treshold>0._SP) reached_treshold=max(reached_treshold,&
&         abs(X_epsilon(i_eps(1),iw)-X_epsilon(i_eps(2),iw))/abs(X_epsilon(2,iw)))
       if (Haydock_treshold<0._SP) reached_treshold=reached_treshold+&
&         abs(X_epsilon(i_eps(1),iw)-X_epsilon(i_eps(2),iw))/abs(X_epsilon(2,iw))/real(W%n(1))
     enddo
     if (use_cpl_scheme) deallocate(X_i)
   end subroutine
   !
   ! This is used only by the standard solver, not by the fast, nor in TDA
   function fdot_product(v,w)
     !
     real(SP)    ::fdot_product
     complex(SP) ::v(2*BS_K_dim),w(2*BS_K_dim)
     !
     fdot_product = real(&
&                   dot_product(v(1:BS_K_dim),w(1:BS_K_dim))&
&                  -dot_product(v(BS_K_dim+1:2*BS_K_dim),w(BS_K_dim+1:2*BS_K_dim)))
     !
   end function fdot_product
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
       f = (x-c1)
       terminator = f + sqrt(f**2 -4._SP*c2**2.)
       terminator = terminator/2._SP/c2**2
     end select
   end function terminator
   !
end subroutine
