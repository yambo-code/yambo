!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine QP_XX_Vxc(E,k,Xk,q)
 !
 ! Hartree S_x self-energy + call to V_xc 
 !
 use pars,          ONLY:SP,schlen,DP,pi
 use units,         ONLY:HARTREE
 use drivers,       ONLY:l_sc_run,l_acfdt,l_sc_exx,l_real_time
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg,depth
 use timing,        ONLY:live_timing
 use stderr,        ONLY:intc,real2ch
 use electrons,     ONLY:levels,spin_occ,n_sp_pol,spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset,master_cpu
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use QP_m,          ONLY:QP_Sx,QP_ng_Sx,QP_table,QP_nb,QP_n_states,QP_Sx_low_band,QP_Vxc
 use R_lattice,     ONLY:qindx_S,bz_samp
 use wave_func,     ONLY:wf,wf_load,wf_ng,WF_free
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer                 ::i1,i2,iq,ib,ibp,jb,ik
#if defined _DOUBLE
 complex(DP)             ::zdotc,zdotu
#else 
 complex(SP)             ::cdotc,cdotu
#endif
 complex(DP)             ::DP_Sx
 complex(SP),allocatable ::local_rhotw(:)
 character(schlen)       ::ch,ch_spin,sx_fmt,vxc_fmt
 !
 !QP list
 !
 integer           :: i4(4)
 integer, external :: QP_state_extract
 !
 !IO
 !
 integer           :: ID,io_err
 integer, external :: ioXXVXC
 !
 ! resets
 !
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
 !
 ! Section
 !
 !
 if (l_acfdt) then
   call section('p','EX(change)S(elf-energy) and Vxc potential')
 else if (.not.l_sc_run) then
   call section('*','EX(change)S(elf-energy) and Vxc potential')
 endif
 if (QP_ng_Sx==0) QP_ng_Sx=wf_ng
 !
 call QP_state_table_setup(E)
 !
 ! Allocation
 !
 if (.not.allocated(QP_Sx)) then
   allocate(QP_Sx(QP_n_states))
   call mem_est("QP_Sx",(/QP_n_states/))
 endif
 if (.not.allocated(QP_Vxc)) then
   allocate(QP_Vxc(QP_n_states))
   call mem_est("QP_Vxc",(/QP_n_states/))
 endif
 QP_Sx=(0.,0.)
 QP_Vxc=(0.,0.)
 !
 ! I/O
 !
 io_err=-1
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioXXVXC(ID)
 !
 if (io_err/=0) then
   !
   if (QP_Sx_low_band>E%nbm) QP_Sx_low_band=1
   call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
   call msg('rn','[EXS] Bands range :',(/QP_Sx_low_band,E%nbm/))
   !
   ! QP_table -> report
   !
   i4=(/1,1,0,0/)
   do while(QP_state_extract(i4)>0)
     write (ch,'(4(a,i3.3))') 'QP @ K ',i4(1),' - ',i4(2),' : b ',i4(3),' - ',i4(4)
     call msg('r',trim(ch))
   enddo
   call msg('r','')
   !
   ! WFs
   !
   call wf_load(QP_ng_Sx,maxval(qindx_S(:,:,2)),&
&               (/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-Sxx/Rho')
   !
   ! allocation again
   !
   allocate(isc%gamp(QP_ng_Sx,1),isc%rhotw(QP_ng_Sx),iscp%rhotw(QP_ng_Sx),&
&           local_rhotw(QP_ng_Sx))
   isc%qs    =1
   isc%ngrho =QP_ng_Sx
   iscp%ngrho=QP_ng_Sx
   !
   call scatterGamp(isc,'x')
   !
   ! par indexes
   !
   call par_indexes(px,(/q%nbz,E%nbm/),(/1,QP_Sx_low_band/))
   call pp_redux_wait
   !
   ch='EXS'
   !
   call live_timing(trim(ch),px%stps(myid+1)*QP_n_states)
   !
   do iq=1,q%nbz
     !
     isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
     if (isc%qs(2)/=isc%iqref) call scatterGamp(isc,'x')
     !
     do i1=1,QP_n_states
       !
       !  (n,k,sp_n).     
       !              |
       !              | (m,p,r,sp_m)
       !              |
       !              |
       !  (m,k,sp_m).     
       !
       ! n   =QP_table(i1,1)
       ! m   =QP_table(i1,2)
       ! k   =QP_table(i1,3)
       !
       ib =QP_table(i1,1)
       ibp=QP_table(i1,2)
       !
       isc%is=(/QP_table(i1,1),QP_table(i1,3),1,spin(QP_table(i1,:))/)
       isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iq,1),:),spin(QP_table(i1,:))/)
       iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,spin(QP_table(i1,:))/)
       !
       isc%qs(1)=qindx_S(QP_table(i1,3),iq,2)
       iscp%qs=isc%qs
       !
       do jb=QP_Sx_low_band,E%nbm
         if (.not.px%i2p(iq,jb)) cycle
         isc%os(1)=jb
         iscp%os=isc%os
         call live_timing(steps=1)
         !
         !
         call scatterBamp(isc)
         !
         !
         ! Normal case, the density matrix is diagonal
         !
         iscp%rhotw=isc%rhotw
         if (isc%is(1)/=iscp%is(1)) call scatterBamp(iscp)
         !
         local_rhotw=-4./spin_occ*pi*isc%rhotw*conjg(iscp%rhotw)*E%f(jb,isc%os(2),isc%os(4))
         !
           !
#if defined _DOUBLE
           DP_Sx=zdotu(QP_ng_Sx,local_rhotw,1,isc%gamp(1,1),1)
#else 
           DP_Sx=cdotu(QP_ng_Sx,local_rhotw,1,isc%gamp(1,1),1)
#endif
           !
         !
         QP_Sx(i1)=QP_Sx(i1)+DP_Sx
         !
       enddo
       !
     enddo
     !
   enddo
   !
   call live_timing()

   !
   !
   deallocate(isc%gamp,isc%rhotw,iscp%rhotw,local_rhotw)
   !
   call pp_redux_wait(QP_Sx)
   call collision_reset(isc)
   call collision_reset(iscp) 
   call pp_indexes_reset(px)
   !
   ! Vxc
   !
   if (.not.l_sc_run.and..not.l_real_time) then
     call DFT_Vxc(E,Xk)
     call WF_free()
   endif
   !
 endif
 !
 ! ... reporting the <Vxc/Sig_x> m.e.
 !
 ik=QP_table(1,3)
 call msg('nr','S_x/V_xc [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
 !
 do i1=1,QP_n_states
   !
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   if (QP_table(i1,3)/=ik) then
     ik=QP_table(i1,3)
     call msg('nr','S_x/V_xc [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
   endif
   !
   ch_spin=' '
   !
   if (n_sp_pol==2.and.spin(QP_table(i1,:))==1) ch_spin='(up)'
   if (n_sp_pol==2.and.spin(QP_table(i1,:))==2) ch_spin='(dn)'
   ! 
   sx_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|S_x|'//trim(intc(ibp))&
&     //trim(ch_spin)//'> ='
   vxc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|V_xc|'//trim(intc(ibp))&
&     //trim(ch_spin)//'> ='
   !
   ch=trim(sx_fmt)//' '//&
&    trim(real2ch(real(QP_Sx(i1)*HARTREE)))//' '//&
&    trim(real2ch(aimag(QP_Sx(i1)*HARTREE)))//' '//&
&    trim(vxc_fmt)//' '//&
&    trim(real2ch(real(QP_Vxc(i1)*HARTREE)))//' '//&
&    trim(real2ch(aimag(QP_Vxc(i1)*HARTREE)))
   !
   if (i1/=QP_n_states) call msg('r',trim(ch))
   if (i1==QP_n_states) call msg('rn',trim(ch))
   !
 enddo
 !
 ! I/O
 !
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioXXVXC(ID)
 endif
 !
 ! MPI barrier
 !
 call pp_redux_wait
 !
end subroutine
