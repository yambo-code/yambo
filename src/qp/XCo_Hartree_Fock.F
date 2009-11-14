!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
subroutine XCo_Hartree_Fock(E,k,Xk,q)
 !
 ! Hartree-Fock
 !
 use pars,          ONLY:SP,schlen,DP,pi
 use drivers,       ONLY:l_sc_run,l_sc_exx,l_rho_IO
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use timing,        ONLY:live_timing
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels,spin_occ,spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use QP_m,          ONLY:QP_Vnl_xc,QP_ng_Sx,QP_table,QP_nb,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
 use wave_func,     ONLY:wf,WF_load,WF_free
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP,OP_WR
#if defined _SC || defined _RT || defined _MAGNETIC
 use SC,            ONLY:it_now,SC_bands
 use global_XC,     ONLY:SC_HF
 use IO_m,          ONLY:NONE
 use oscillators_IO,ONLY:rho_VX_rho,rho_table,n_rho_states,Sx_cutoff,&
&                        oscillators_IO_alloc,oscillators_IO_free,V_kind,ioRHO_RHO_VX 
#endif
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer                 ::i2,iq,ib,ibp,jb,ik,Sx_upper_band,i_qp
#if defined _RT
 integer                 :: jb2,ik_minus_q,ic,COM_,n_tot_states,n_stored_states
 real(SP)                :: sx_max
 complex(SP), allocatable:: tmp_sx(:,:,:)
#endif
#if defined _DOUBLE
 complex(DP)             ::zdotc,zdotu
#else 
 complex(SP)             ::cdotc,cdotu
#endif
 complex(DP)             ::DP_Sx
 complex(SP),allocatable ::local_rhotw(:)
 character(schlen)       ::ch,ch_spin,sx_fmt,vxc_fmt,dump_ch
 !
 !QP list
 !
 integer              :: i4(4)
 integer, external    :: QP_state_extract
#if defined _RT
 logical, allocatable :: QP_DB_done(:)
#endif
 !
 !IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 !
 ! Resets & basis setup
 !
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
 !
 Sx_upper_band=E%nbm
 !
#if defined _RT
 !
 ! Oscillators I/O
 !
 if(l_rho_IO) then
   !
   allocate(QP_DB_done(QP_n_states))
   !
   Sx_upper_band=SC_bands
   !
   V_kind=SC_HF
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=ioRHO_RHO_VX(0,ID,QP_DB_done=QP_DB_done)
   !
   if(io_err==0.and.all(QP_DB_done)) then
     deallocate(QP_DB_done)
     return
   endif
   !
   write (dump_ch,'(a,f10.5)') &
      & '[RHO] Disk space required [Gb]:',real(QP_n_states*SC_bands**2*k%nibz*SP)/1.0e9
   call msg('s',dump_ch)
   !
   allocate(tmp_sx(SC_bands,SC_bands,k%nibz))
   call mem_est("tmp_sx",(/size(tmp_sx)/),(/2*SP/))
   !      
   call oscillators_IO_alloc()
   !
   n_tot_states   =0
   n_stored_states=0
   !
 endif
 !
#endif
 !
 call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
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
 call WF_load(QP_ng_Sx,maxval(qindx_S(:,:,2)),&
&             (/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-Sxx/Rho')
 !
 ! allocation again
 !
 allocate(isc%gamp(QP_ng_Sx,1),isc%rhotw(QP_ng_Sx),iscp%rhotw(QP_ng_Sx),&
&         local_rhotw(QP_ng_Sx))
 isc%qs    =1
 isc%ngrho =QP_ng_Sx
 iscp%ngrho=QP_ng_Sx
 !
 call scatterGamp(isc,'x')
 !
 ! par indexes
 !
#if defined _RT
 call par_indexes(px,(/QP_n_states/))
#else
 call par_indexes(px,(/q%nbz,E%nbm/))
#endif
 call pp_redux_wait
 !
 ch='EXS'
#if defined _SC  || defined _MAGNETIC || defined _RT
 if (l_sc_run) ch='EXS@it'//trim(intc(it_now))
 if (l_rho_IO) ch='RHO_IO'
#endif
 !
#if defined _RT
 call live_timing(trim(ch),px%stps(myid+1))
#else
 call live_timing(trim(ch),px%stps(myid+1)*QP_n_states)
#endif
 !
 do i_qp=1,QP_n_states
   !
#if defined _RT
   if (.not.px%i1p(i_qp)) cycle
   !        
   if (QP_DB_done(i_qp)) then
     call live_timing(steps=1)
     cycle
   endif
   !
   tmp_sx=(0.,0.)
   !
#endif
   do iq=1,q%nbz
     !
     isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
     if (isc%qs(2)/=isc%iqref) call scatterGamp(isc,'x')
     !
     !  (n,k,sp_n).     
     !              |
     !              | (m,p,r,sp_m)
     !              |
     !              |
     !  (m,k,sp_m).     
     !
     ! n   =QP_table(i_qp,1)
     ! m   =QP_table(i_qp,2)
     ! k   =QP_table(i_qp,3)
     !
     ib =QP_table(i_qp,1)
     ibp=QP_table(i_qp,2)
     !
     isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iq,1),:),spin(QP_table(i_qp,:))/)
     iscp%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     !
     isc%qs(1)=qindx_S(QP_table(i_qp,3),iq,2)
     iscp%qs=isc%qs
     !
#if defined _RT
     ik_minus_q=isc%os(2)
#endif
     !
     do jb=1,Sx_upper_band
       !      
#if !defined _RT
       if (.not.px%i2p(iq,jb)) cycle
#endif
       isc%os(1)=jb
       iscp%os=isc%os
       !
#if defined _SC  || defined _MAGNETIC
       !
       ! In Exact-Exchange only vc matrix elements so ... no cc' no vv' 
       ! (cv is not permitted by the order in QP_table)
       !                   (c)         (v)
       if (l_sc_exx.and.(ib>E%nbf.or.ibp<=E%nbf)) cycle
#endif
       !
       call scatterBamp(isc)
       !
#if defined _RT
       !
       ! Use the full density matrix  -i*G_lesser
       ! Symmetries can used to reduce this calculation
       ! parallelization doesn't work yet on rho_rho_gamp(:,:,:,:,:)
       ! and store
       !
       ! rho_rho_gamp(n,n',qp_n_states) = \sum_{G,q} ( rho(m,n,G,q,k) x rho*(m',n',G,q,k) /|q+G|^2 )
       !
       do jb2=1,SC_bands
         !      
         iscp%os(1)=jb2
         call scatterBamp(iscp)
         local_rhotw=-4./spin_occ*pi*isc%rhotw*conjg(iscp%rhotw)
#else
       !
       ! Normal case, the density matrix is diagonal
       !
       iscp%rhotw=isc%rhotw
       if (isc%is(1)/=iscp%is(1)) call scatterBamp(iscp)
       !
       local_rhotw=-4./spin_occ*pi*isc%rhotw*conjg(iscp%rhotw)*E%f(jb,isc%os(2),isc%os(4))
       !
#endif
       !
#if defined _DOUBLE
       DP_Sx=zdotu(QP_ng_Sx,local_rhotw,1,isc%gamp(1,1),1)
#else 
       DP_Sx=cdotu(QP_ng_Sx,local_rhotw,1,isc%gamp(1,1),1)
#endif
       !
#if defined _RT
         !
         tmp_sx(jb,jb2,ik_minus_q)=tmp_sx(jb,jb2,ik_minus_q)+DP_Sx
         !
       enddo
#else
       !
       QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)+DP_Sx
       !
       call live_timing(steps=1)
       !
#endif   
     enddo
     !
   enddo
   !
#if defined _RT
   !
   ! Reduction of tiny elements of tmp_sx.
   !
   if(l_rho_IO) then 
     !      
     sx_max=maxval(abs(tmp_sx(:,:,:)))        
     !
     ic=0
     !
     do jb=1,SC_bands
       do jb2=1,SC_bands
         do ik=1,E%nk
           if(abs(tmp_sx(jb,jb2,ik))>=sx_max*Sx_cutoff &
       &     .or.sx_cutoff<0.0)  ic=ic+1
         enddo
       enddo
     enddo
     !
     n_rho_states   =ic
     n_tot_states   =n_tot_states+SC_bands**2*E%nk
     n_stored_states=n_stored_states+n_rho_states
     !
     ic=0
     !
     do jb=1,SC_bands
       do jb2=1,SC_bands
         do ik=1,E%nk
           if(abs(tmp_sx(jb,jb2,ik))>=sx_max*Sx_cutoff &
       &     .or.sx_cutoff<0.0)  then
             ic=ic+1
             rho_table(1,ic)=jb
             rho_table(2,ic)=jb2
             rho_table(3,ic)=ik
             rho_VX_rho(ic) =tmp_sx(jb,jb2,ik)
           endif
         enddo      
       enddo      
     enddo      
     !
     if (i_qp==1) then
       call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
       io_err=ioRHO_RHO_VX(0,ID)
       call io_control(ACTION=OP_WR_CL,SEC=(/2/),ID=ID)
       io_err=ioRHO_RHO_VX(i_qp,ID)
     else
       call io_control(ACTION=OP_WR_CL,SEC=(/i_qp+1/),ID=ID)
       io_err=ioRHO_RHO_VX(i_qp,ID)
     endif
     !
   endif
   !
   call live_timing(steps=1)
#endif
   !
 enddo
 !
 call live_timing()
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,local_rhotw)
 !
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
 !
#if defined _RT
 if (l_rho_IO) then
   !
   COM_=REP
   if (.not.all(QP_DB_done)) COM_=NONE
   call io_control(ACTION=OP_WR_CL,COM=COM_,SEC=(/1/),ID=ID)
   io_err=ioRHO_RHO_VX(0,ID)
   !
   deallocate(QP_DB_done)
   call oscillators_IO_free()
   call WF_free()
   !
   if(Sx_cutoff>0.0.and.Sx_cutoff<1.) then
     write (dump_ch,'(a,f10.1,a)') '[RHO] Exchange elements reduced by :', &
   &     (1.-real(n_stored_states)/real(n_tot_states))*100.,'%'
     call msg('s',dump_ch)
   endif
   !
   return
 endif
 !
#else
 !
 call pp_redux_wait(QP_Vnl_xc)
 !
#endif
 !
end subroutine
