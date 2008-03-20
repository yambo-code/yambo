!
! Copyright (C) 2000-2005 A. Marini, M. Gruning and the SELF team 
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
subroutine K_Haydock(iq,W)
 !
 use pars,           ONLY:SP,pi,schlen
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_epsilon
 use R_lattice,      ONLY:d3k_factor,q_norm
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,isec
 use BS,             ONLY:BSS_n_descs,BS_K_dim,BS_mat,Haydock_treshold,&
&                         BSS_rhoq0,BSS_description,Haydock_v0_mod,&
&                         BS_DB_is_fragmented,Kernel_by_V,&
&                         BS_eh_f,ioBSS_Haydock,BS_K_Coupling
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use electrons,      ONLY:spin_occ
 use IO_m,           ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
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
 real(SP)           ::reached_treshold, Bf(Max_iterations),Af(Max_iterations)
 character(schlen)  ::dumb_ch
 logical, external  ::stop_now
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
 if (BS_K_Coupling) then
   !
   ! GPL_EXCLUDE_START
   !
   allocate(Cf(Max_iterations+1),Vn(2*BS_K_dim),Vnm1(2*BS_K_dim),&
&           Vnp1(2*BS_K_dim))
   !
   ! GPL_EXCLUDE_END
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
 if (BS_K_Coupling) then 
   !
   ! GPL_EXCLUDE_START
   !
   io_err=ioBSS_Haydock(ID,1,2*BS_K_dim,reached_treshold,Af(1),Bf(1),&
&  Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(1))   
   !
   ! GPL_EXCLUDE_END
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
   if (BS_K_Coupling) then 
     !
     ! GPL_EXCLUDE_START
     !
     io_err=ioBSS_Haydock(ID,it_on_disk,2*BS_K_dim,reached_treshold,Af(:it_on_disk),&
&            Bf(:it_on_disk+1),Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it_on_disk+1))
     !
     ! GPL_EXCLUDE_END
     !
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
 if (BS_K_Coupling) then
   !
   ! GPL_EXCLUDE_START
   !
   ! Before starting iterate one needs
   !
   ! | Vn > =     |q->0>/(<q->0| F (H |q->0>))
   ! | Vnp1 > = H |Vn>
   !
   if (it_on_disk==0) then
     Cf= (0.,0.)
     Vn(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)  *BS_eh_f(:BS_K_dim)
     Vn(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)*BS_eh_f(:BS_K_dim)
     call Kernel_by_V(iq,iter=it_on_disk,Vi=Vn,Vo=Vnp1)
     Haydock_v0_mod=sqrt(fdot_product(Vn,Vnp1))
     Vnp1(:) = Vnp1(:)/Haydock_v0_mod
     Vn(:)   = Vn(:)/Haydock_v0_mod
     Cf(1) = dot_product(BSS_rhoq0,Vn)/Haydock_v0_mod
   else
     Haydock_v0_mod = Bf(1)
     Bf(1) = 0.
   endif     
   !
   ! GPL_EXCLUDE_END
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
   if (.not.BS_K_Coupling) then
     !
     !|Vn+1> = H |Vn>
     !
     call Kernel_by_V(iq,iter=it-it_on_disk,Vi=Vn,Vo=Vnp1)
     !
     ! A(n) = <Vn|Vn+1>
     !
     Af(it)=real(dot_product(Vn,Vnp1))
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
#if defined _DOUBLE
     call zaxpy(BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,1,Vnp1,1)
     call zaxpy(BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,1,Vnp1,1)
#else
     call caxpy(BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,1,Vnp1,1)
     call caxpy(BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,1,Vnp1,1)
#endif
     !
     ! B(n+1)= <Vn+1|Vn+1>^(1/2)
     !
     Bf(it+1)=sqrt(dot_product(Vnp1,Vnp1))
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
     ! GPL_EXCLUDE_START
     !
     ! A(n) = <Vn+1|F|Vn+1> (that is <Vn|HFH|Vn>)
     !
     Af(it)=fdot_product(Vnp1,Vnp1)
     !
     !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
     !
#if defined _DOUBLE
     call zaxpy(2*BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,1,Vnp1,1)
     call zaxpy(2*BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,1,Vnp1,1)
#else
     call caxpy(2*BS_K_dim,-Af(it)*(1._SP,0._SP),Vn,1,Vnp1,1)
     call caxpy(2*BS_K_dim,-Bf(it)*(1._SP,0._SP),Vnm1,1,Vnp1,1) 
#endif
     !
     ! |Vn-1> = |Vn> 
     ! |Vn>   = |Vn+1> 
     !
     Vnm1=Vn
     Vn  =Vnp1
     !
     !|Vn+1> = H |Vn>
     !
     call Kernel_by_V(iq,iter=it-it_on_disk,Vi=Vn,Vo=Vnp1)
     !
     ! B(n+1)= <Vn|F|Vn+1>^(1/2) (that is <Vn|FH|Vn>^(1/2))
     !
     Bf(it+1)=sqrt(fdot_product(Vn,Vnp1)) 
     !
     !|Vn> =|Vn+1> / B(n+1)
     !
     forall(i1=1:2*BS_K_dim) Vn(i1)=Vn(i1)/Bf(it+1)
     forall(i1=1:2*BS_K_dim) Vnp1(i1)=Vnp1(i1)/Bf(it+1)
     Cf(it+1)= dot_product(BSS_rhoq0,Vn)/Haydock_v0_mod
     !
     ! GPL_EXCLUDE_END
     !
   endif
   !
   if (it>2) then
     !
     if (BS_K_Coupling) then 
       !
       ! GPL_EXCLUDE_START
       !
       call build_L_and_check_convergence(Af(:it),Bf(:it),it,Cf(:it))
       !
       ! GPL_EXCLUDE_END
       !
     else
       call build_L_and_check_convergence(Af(:it),Bf(:it),it)
     end if
     !
     BSS_description(BSS_n_descs-1)='        |Accuracy (reached)   [o/o]:'//&
&                                    trim(real2ch(reached_treshold))
     BSS_description(BSS_n_descs)='        |Iteration                 :'//trim(intc(it))
     !
     call K_dump_to_o_file(iq,1)
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
   if (BS_K_Coupling) then
     !
     ! GPL_EXCLUDE_START
     !
     Bf(1) = Haydock_v0_mod 
     io_err=ioBSS_Haydock(ID,it,2*BS_K_dim,reached_treshold,Af(:it),Bf(:it+1),   &
&       Vnm1,Vn,Vnp1=Vnp1,Cf=Cf(:it+1))
     Bf(1) = 0.
     !
     ! GPL_EXCLUDE_END
     !
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
 !
 ! GPL_EXCLUDE_START
 !
 if (BS_K_Coupling) deallocate(Cf)
 !
 ! GPL_EXCLUDE_END
 !
 deallocate(Vn,Vnm1,Vnp1) 
 !
 ! Either if BS_mat is allocated (no fragments) or is not allocated
 ! I need to clean the memory
 !
 call mem_est("BS_mat")
 !
 contains
   !
   subroutine build_L_and_check_convergence(Af,Bf,it,Cf)
     implicit none
     integer    :: it
     real(SP)   :: Bf(it),Af(it)
     complex(SP),intent(in),optional :: Cf(it)
     !
     ! Work Space
     !
     integer     :: i1,iw
     real(SP)    :: Co
     complex(SP) :: X_o(2,it), YbyX(2)
     complex(SP),allocatable :: X_i(:,:)
     !
     ! Co includes the spin_occ factor because Haydock is expected to work
     ! only when ALL BS_eh_f == spin_occ
     !
     Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/q_norm(1)**2.*Haydock_v0_mod**2.
     !
     ! GPL_EXCLUDE_START
     !
     if (BS_K_Coupling) allocate(X_i(2,it))
     !
     ! GPL_EXCLUDE_END
     !
     do iw=1,W%n(1)
       X_o=(0._SP,0._SP)
       X_o(:,it)=1./(W%p(iw)-Af(it))
       do i1=it-1,1,-1
         X_o(1,i1)=1./(W%p(iw)-Af(i1)-Bf(i1+1)**2.*X_o(1,i1+1))
       enddo
       do i1=it-2,1,-1
         X_o(2,i1)=1./(W%p(iw)-Af(i1)-Bf(i1+1)**2.*X_o(2,i1+1))
       enddo
       !
       ! GPL_EXCLUDE_START
       !
       if (BS_K_Coupling) then
         X_i(:,1) = X_o(:,1)
         X_i(:,2) = Bf(2)*X_o(:,2)*X_i(:,1)
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
       !
       ! GPL_EXCLUDE_END
       !
       X_epsilon(1,iw)=W%p(iw)
       X_epsilon(2,iw)=-X_o(1,1)*Co+1.
       X_epsilon(4,iw)=-X_o(2,1)*Co+1.
       if (Haydock_treshold>0._SP) reached_treshold=max(reached_treshold,&
&         abs(X_epsilon(2,iw)-X_epsilon(4,iw))/abs(X_epsilon(2,iw)))
       if (Haydock_treshold<0._SP) reached_treshold=reached_treshold+&
&         abs(X_epsilon(2,iw)-X_epsilon(4,iw))/abs(X_epsilon(2,iw))/real(W%n(1))
     enddo
     !
     ! GPL_EXCLUDE_START
     !
     if (BS_K_Coupling) deallocate(X_i)
     !
     ! GPL_EXCLUDE_END
     !
   end subroutine
   !
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
end subroutine
