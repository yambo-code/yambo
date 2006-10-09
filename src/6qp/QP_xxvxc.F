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
 subroutine QP_xxvxc(E,k,Xk,q)
!=============================
!
! V_xc and the S_x 
!
 use pars,          ONLY:SP,schlen,DP,pi,HARTREE
 use memory_m,      ONLY:mem_est
 use drivers,       ONLY:Finite_Tel
 use com,           ONLY:msg,depth,gen_fmt
 use timing,        ONLY:live_timing
 use stderr,        ONLY:string_pack
 use electrons,     ONLY:levels,n_spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use QP_m,          ONLY:QP_Sx,QP_ng_Sx,QP_table,QP_nb,QP_Vxc,QP_n_states,&
&                        QP_Sx_low_band,QP_xc_FUNCTIONAL
 use D_lattice,     ONLY:DL_vol
 use R_lattice,     ONLY:qindx_S,bz_samp
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:wf,wf_state,wf_load,wf_ng,WF_free
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use xc_functionals,ONLY:V_xc,xc_string,GS_xc_KIND,GS_xc_FUNCTIONAL
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
!
!ws
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer          ::i1,i2,iq,ib,ik,ifft
#if defined DP
 complex(DP):: zdotc,zdotu
#else 
 complex(SP):: cdotc,cdotu
#endif
 complex(DP)::dsx
 character(schlen)       ::ch,ch_spin,local_fmt
!
!Vxc
 complex(SP),allocatable ::c_vec(:)
!DEBUG >
! real(SP),allocatable    ::rho(:),vxc(:)
! real(4), external       ::vxcca
!DEBUG <
!
!QP list
 integer           :: i4(4)
 integer, external :: qp_state_extract
!
!IO
 integer           :: ID,io_err
 integer, external :: ioXXVXC
!
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
!---------------------------------------------------------------------
 if (depth>0) then
   call section('=','EX(change)S(elf-energy) and Vxc potential')
 else
   call section('*','EX(change)S(elf-energy) and Vxc potential')
 endif
 if (QP_ng_Sx==0) QP_ng_Sx=wf_ng

 call QP_table_setup(E)
!
! Alloc
!===================
 allocate(QP_Sx(QP_n_states),QP_Vxc(QP_n_states))
 call mem_est("QP_Sx QP_Vxc",(/QP_n_states,QP_n_states/))
 QP_Sx=(0.,0.)
 QP_Vxc=(0.,0.)
!
! I/O
!=====
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioXXVXC(ID)
 if (io_err==0) goto 1
!
! QP_table -> report
!===================
 if (QP_Sx_low_band>E%nbm) QP_Sx_low_band=1
 call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
 call msg('rn','[EXS] Bands range :',(/QP_Sx_low_band,E%nbm/))

 i4=(/1,1,0,0/)
 do while(qp_state_extract(i4)>0)
   write (ch,'(4(a,i3.3))') 'QP @ K ',i4(1),' - ',i4(2),' : b ',i4(3),' - ',i4(4)
   call msg('r',trim(ch))
 enddo
 call msg('r','')
!
!WFs
!====
 call wf_load(QP_ng_Sx,maxval(qindx_S(:,:,2)),&
&             (/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-Sxx/Rho')
!
 allocate(isc%gamp(1,QP_ng_Sx),isc%rhotw(QP_ng_Sx),iscp%rhotw(QP_ng_Sx))
 isc%qs   =1
 isc%ngrho=QP_ng_Sx
!
 call scatterGamp(isc,'x')

 call par_indexes(px,(/q%nbz,E%nbm/),(/1,QP_Sx_low_band/))
 call pp_redux_wait
 call live_timing('EXS',px%stps(myid+1)*QP_n_states)
 do iq=1,q%nbz
   isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
   if (isc%qs(2)/=isc%iqref) call scatterGamp(isc,'x')
   do i1=1,QP_n_states
     isc%is=(/QP_table(i1,1),QP_table(i1,3),1,1/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iq,1),:),1/)
     iscp%is=(/QP_table(i1,2),QP_table(i1,3),1,1/)
     isc%qs(1)=qindx_S(QP_table(i1,3),iq,2)
     iscp%qs=isc%qs
     do ib=QP_Sx_low_band,E%nbm
       if (.not.px%i2p(iq,ib)) cycle
       isc%os(1)=ib
       iscp%os=isc%os
       call live_timing(steps=1)
       call scatterBamp(isc)
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       isc%rhotw=-2.*pi*isc%rhotw*conjg(iscp%rhotw)*E%f(ib,isc%os(2),isc%os(4))
       dsx=(0.d0,0.d0)
#if defined DP
       dsx=zdotu(QP_ng_Sx,isc%rhotw,1,isc%gamp(1,:),1)
#else 
       dsx=cdotu(QP_ng_Sx,isc%rhotw,1,isc%gamp(1,:),1)
#endif
       QP_Sx(i1)=QP_Sx(i1)+dsx
     enddo
   enddo
 enddo
 call live_timing()
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw)
 call pp_redux_wait(QP_Sx)
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
!-----------------------------------------------------------------------------
! Vxc
!=====
 allocate(V_xc(fft_size),c_vec(fft_size))
 call xc_lda_driver(E,Xk,GS_xc_KIND,GS_xc_FUNCTIONAL,1)
 QP_xc_FUNCTIONAL=trim(xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))

!DEBUG >
! allocate(rho(fft_size),vxc(fft_size))
! call el_density(en,Xk,rho,.false.)
! do i1=1,fft_size ! V_xc
!   vxc(i1)=vxcca(rho(i1)*real(fft_size)/DL_vol)
! enddo
!DEBUG <

 do i1=1,QP_n_states
   ifft=wf_state(QP_table(i1,1),QP_table(i1,3),1)
   c_vec(:)=wf(:,ifft)*V_xc(:)
#if defined DP
   QP_Vxc(i1)=zdotc(fft_size,wf(:,ifft),1,c_vec,1)
#else 
   QP_Vxc(i1)=cdotc(fft_size,wf(:,ifft),1,c_vec,1)
#endif
 enddo
 deallocate(V_xc,c_vec)
 call WF_free()
!DEBUG >
! deallocate(rho,vxc,c_vec)
!DEBUG <
!
1 i2=QP_table(1,3)
 write (ch,'(a,i3.3,a)') 'K [',i2,'] (iku):'
 call msg('nr',trim(ch),k%pt(i2,:))
 ch=string_pack('(2(a,',gen_fmt( reshape(QP_table(:,:2),(/2*QP_n_states/)) ),&
&                  ',2a,',gen_fmt( reshape(QP_table(:,:2),(/2*QP_n_states/)) ) )
 local_fmt=string_pack(ch,',2a,2(',&
&                 gen_fmt( r_v= reshape(real(QP_Sx)*HARTREE,(/QP_n_states/)) ) ,')))')

 do i1=1,QP_n_states
   if (QP_table(i1,3)/=i2) then
     i2=QP_table(i1,3)
     write (ch,'(a,i3.3,a)') 'K [',i2,'] (iku):'
     call msg('r',trim(ch),k%pt(i2,:))
   endif

   ch_spin=' '
   if (n_spin==2.and.QP_table(i1,4)==1) ch_spin='(up)'
   if (n_spin==2.and.QP_table(i1,4)==2) ch_spin='(dn)'

   write (ch,trim(local_fmt)) &
&   '<',QP_table(i1,1),trim(ch_spin),'|S_x|',QP_table(i1,2),trim(ch_spin),'> = ',&
&   real(QP_Sx(i1)*HARTREE),aimag(QP_Sx(i1)*HARTREE),&
&   '<',QP_table(i1,1),trim(ch_spin),'|V_xc|',QP_table(i1,2),trim(ch_spin),'> = ',&
&   real(QP_Vxc(i1)*HARTREE),aimag(QP_Vxc(i1)*HARTREE)

!   write (ch,trim(local_fmt)) &
!&   '<',QP_table(i1,1),'|S_x|,QP_table(i1,2),'> = ',&
!&   real(QP_Sx(i1)*HARTREE),aimag(QP_Sx(i1)*HARTREE),&
!&   '<',QP_table(i1,1),'|V_xc|',QP_table(i1,2),'> = ',&
!&   real(QP_Vxc(i1)*HARTREE),aimag(QP_Vxc(i1)*HARTREE)


   if (i1/=QP_n_states) call msg('r',trim(ch))
   if (i1==QP_n_states) call msg('rn',trim(ch))
 enddo
! I/O
!=====
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=ioXXVXC(ID)
 endif
 call pp_redux_wait
!
 end subroutine
!
