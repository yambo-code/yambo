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
 subroutine K_Haydock(iq,W)
 !
 use pars,           ONLY:SP,pi,schlen
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_epsilon
 use R_lattice,      ONLY:d3k_factor,q_norm
 use frequency,      ONLY:w_samp
 use com,            ONLY:msg,gen_fmt,isec
 use BS,             ONLY:BSS_n_descs,BS_K_dim,BS_mat,Haydock_treshold,&
&                         BSS_rhoq0,BSS_description,Haydock_v0_mod,&
&                         BS_DB_is_fragmented,Kernel_by_V
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:cstr,tty_size,string_pack
 use parser_m,       ONLY:parser
 use it_m,           ONLY:it_reset
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
 complex(SP)        ::Vn(BS_K_dim),Vnm1(BS_K_dim),Vnp1(BS_K_dim),Af(Max_iterations)
 real(SP)           ::Bf(Max_iterations),reached_treshold
 character(schlen)  ::ch(3)
 !
 ! I/O
 !
 integer           :: ID,io_err
 integer, external :: ioBSS_Haydock
 !
 if (isec(2)/=0) then
   call section('=','Haydock solver')
 else if (isec(2)==0) then
   call section('+','Haydock solver')
 endif
 !
 !Haydock Coefficients and restart vector on disk
 !
 it_on_disk=0
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioBSS_Haydock(ID,1,Af(1),Bf(1),reached_treshold,Vnm1,Vn,Vnp1)
 if (io_err>0) then
   it_on_disk=io_err
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
   io_err=ioBSS_Haydock(ID,it_on_disk,Af(:it_on_disk),Bf(:it_on_disk+1),&
&                   reached_treshold,Vnm1,Vn,Vnp1)
   call msg('s','[Haydock] Restart DB with accuracy ',reached_treshold)
 endif
 !
 BSS_n_descs=BSS_n_descs+1
 ch(1)=string_pack('(a,',trim(gen_fmt(r_v=(/Haydock_treshold/))),')')
 write (BSS_description(BSS_n_descs),trim(ch(1))) &
&      '# Haydock|Accuracy (requested) [o/o]:',Haydock_treshold
 BSS_n_descs=BSS_n_descs+1
 write (BSS_description(BSS_n_descs),trim(ch(1))) &
&      '#        |Accuracy (reached)   [o/o]:',0.
 BSS_n_descs=BSS_n_descs+1
 ch(1)=string_pack('(a,',trim(gen_fmt((/100/))),')')
 write(BSS_description(BSS_n_descs),trim(ch(1)))  &
&      '#        |Iteration            [o/o]:',0
 call msg('r','Use Database fragments    :',BS_DB_is_fragmented)
 if (BS_DB_is_fragmented) call msg('s','[Haydock] Use Database fragments')
 call msg('r','Accuracy (requested) [o/o]:',Haydock_treshold)
 !
 !it=1:|V1>=|q->0>/(<q->0|q->0>)^(1/2)
 !
 Haydock_v0_mod=sqrt(dot_product(BSS_rhoq0,BSS_rhoq0))
 if (it_on_disk==0) then
   Vn=(0.,0.)
   Vnm1=(0.,0.)
   Vnp1=(0.,0.)
   Af=(0.,0.)
   Bf=0.
   forall(i1=1:BS_K_dim) Vn(i1)=BSS_rhoq0(i1)/Haydock_v0_mod
 endif
 do it=it_on_disk+1,Max_iterations
   call it_reset(0)
   call parser('BSHayTrs',Haydock_treshold)
   reached_treshold=0.
   !
   !|Vn+1> = H |Vn>
   !
   call Kernel_by_V(iq,iter=it-it_on_disk,Vi=Vn,Vo=Vnp1)
   !
   !A(n) = <Vn|Vn+1>
   !
   Af(it)=dot_product(Vn,Vnp1)
   !
   !|Vn+1> = |Vn+1> - A(n)|Vn> - B(n)|Vn-1>
   !
#if defined _DOUBLE
   call zaxpy(BS_K_dim,-Af(it),Vn,1,Vnp1,1)
   call zaxpy(BS_K_dim,-cmplx(Bf(it)),Vnm1,1,Vnp1,1)
#else
   call caxpy(BS_K_dim,-Af(it),Vn,1,Vnp1,1)
   call caxpy(BS_K_dim,-cmplx(Bf(it)),Vnm1,1,Vnp1,1)
#endif
   !
   !B(n+1)= <Vn+1|Vn+1>^(1/2)
   !|Vn+1> =|Vn+1> / B(n+1)
   !
   Bf(it+1)=sqrt(dot_product(Vnp1,Vnp1))
   forall(i1=1:BS_K_dim) Vnp1(i1)=Vnp1(i1)/Bf(it+1)
   !
   !|Vn-1> = |Vn>
   !|Vn>   = |Vn+1>
   !
   Vnm1=Vn
   Vn  =Vnp1
   write (ch(2),'(a,i3.3)') '[Haydock] Iteration ',it
   if (it>2) then
     call Gh(Af(:it),Bf(:it),it)
     ch(1)=string_pack('(a,',trim(gen_fmt(r_v=(/reached_treshold/))),')')
     write (BSS_description(BSS_n_descs-1),trim(ch(1))) &
&          '#        |Accuracy (reached)   [o/o]:',reached_treshold
     ch(1)=string_pack('(a,',trim(gen_fmt((/it/))),')' )
     write(BSS_description(BSS_n_descs),trim(ch(1)))  &
&         '#        |Iteration                 :',it
     call K_dump_to_o_file(iq,1)
     write (ch(3),'(2a,f8.5,a,f8.5)') trim(ch(2)),' Accuracy :',&
&                                     reached_treshold,'|',abs(Haydock_treshold)
     if (tty_size>0.and.it_on_disk==0) call live_time_msg("r",ch(3),"","%s")
     if (tty_size>0.and.it_on_disk> 0) call live_time_msg("n",ch(3),"","%s")
     if (tty_size<0) call live_time_msg("n",ch(3),"","%s")
   else
     if (tty_size<0) call live_time_msg("n",ch(2),"","%s")
     if (tty_size>0.and.it==1) call live_time_msg("n",ch(2),"","%s")
     if (tty_size>0.and.it==2) call live_time_msg("r",ch(2),"","%s")
   endif
   !
   !Haydock Coefficients and restart vector on disk
   !
   call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),ID=ID)
   io_err=ioBSS_Haydock(ID,it,Af(:it),Bf(:it+1),reached_treshold,Vnm1,Vn,Vnp1)
   !
   if (reached_treshold>0.and.reached_treshold<abs(Haydock_treshold)) then
     call msg('r','Accuracy (reached)   [o/o]:',reached_treshold)
     call msg('r','Iterations                :',it)
     exit
   endif
 enddo
 !
 ! CLEAN
 !
 if (allocated(BS_mat)) deallocate(BS_mat)
 !
 ! Either if BS_mat is allocated (no fragments) or is not allocated
 ! I need to clean the memory
 !
 call mem_est("BS_mat")
 !
 contains
   !
   subroutine Gh(Af,Bf,it)
     implicit none
     integer    :: it
     real(SP)   :: Bf(it)
     complex(SP):: Af(it)
     !
     ! Work Space
     !
     integer :: i1,iw
     real(SP):: Co
     complex(SP) :: G(2,it)
     !
     ! Co includes the spin_occ factor because Haydock is expected to work
     ! only when ALL BS_eh_f == spin_occ
     !
     Co=spin_occ/(2.*pi)**3.*d3k_factor*4.*pi/q_norm(1)**2*Haydock_v0_mod**2.
     !
     do iw=1,W%n(1)
       G=(0.,0.)
       G(:,it)=1./(W%p(iw)-Af(it))
       do i1=it-1,1,-1
         G(1,i1)=1./(W%p(iw)-Af(i1)-Bf(i1+1)**2.*G(1,i1+1))
       enddo
       do i1=it-2,1,-1
         G(2,i1)=1./(W%p(iw)-Af(i1)-Bf(i1+1)**2.*G(2,i1+1))
       enddo
       X_epsilon(1,iw)=W%p(iw)
       X_epsilon(2,iw)=-G(1,1)*Co+1.
       X_epsilon(4,iw)=-G(2,1)*Co+1.
       if (Haydock_treshold>0.) reached_treshold=max(reached_treshold,&
&         abs(X_epsilon(2,iw)-X_epsilon(4,iw))/abs(X_epsilon(2,iw)))
       if (Haydock_treshold<0.) reached_treshold=reached_treshold+&
&         abs(X_epsilon(2,iw)-X_epsilon(4,iw))/abs(X_epsilon(2,iw))/real(W%n(1))
     enddo
   end subroutine
   !
 end subroutine
