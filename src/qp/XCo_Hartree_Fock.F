!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
 use drivers,       ONLY:l_sc_run,l_sc_exx,l_collisions_IO
 use com,           ONLY:msg
 use timing,        ONLY:live_timing
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels,spin_occ,spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use QP_m,          ONLY:QP_Vnl_xc,QP_ng_Sx,QP_table,QP_nb,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
 use wave_func,     ONLY:WF_load
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 type(ggwinfo)    ::isc,iscp
 type(pp_indexes) ::px
 integer                 :: i2,iq,ib,ibp,jb,ik,i_qp
 integer                 :: Sx_lower_band,Sx_upper_band
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
 Sx_lower_band=1
 Sx_upper_band=E%nbm
 !
 !
 call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
 !
 if (.not.l_collisions_IO) then
   !
   ! QP_table -> report
   !
   call msg('r','')
   i4=(/1,1,0,0/)
   do while(QP_state_extract(i4)>0)
     write (ch,'(4(a,i3.3))') 'QP @ K ',i4(1),' - ',i4(2),' : b ',i4(3),' - ',i4(4)
     call msg('r',trim(ch))
   enddo
   call msg('r','')
   !
 endif
 !
 !
 ! WFs
 !
 call WF_load(QP_ng_Sx,maxval(qindx_S(:,:,2)),(/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-HF/Rho')
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
 call par_indexes(px,(/q%nbz,E%nbm/))
 call pp_redux_wait
 !
 ch='EXS'
 !
 if (l_sc_exx) then
   call live_timing(trim(ch),px%n_of_elements(myid+1)*E%nbf*&
        &(maxval(QP_table(:,2))-E%nbf)*maxval(QP_table(:,3)))
 else  
   call live_timing(trim(ch),px%n_of_elements(myid+1)*QP_n_states)
 endif
 !
 do i_qp=1,QP_n_states
   !
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   !
   !    
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
     isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iq,1),:),spin(QP_table(i_qp,:))/)
     iscp%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     !
     isc%qs(1)=qindx_S(QP_table(i_qp,3),iq,2)
     iscp%qs=isc%qs
     !
     !
     do jb=Sx_lower_band,Sx_upper_band
       !      
       if (.not.px%element_2D(iq,jb)) cycle
       isc%os(1)=jb
       iscp%os=isc%os
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
       QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)+DP_Sx
       !
       call live_timing(steps=1)
       !
     enddo
     !
   enddo
   !
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
 call pp_redux_wait(QP_Vnl_xc)
 !
end subroutine
