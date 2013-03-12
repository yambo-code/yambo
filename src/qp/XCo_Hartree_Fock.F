!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 use collision,     ONLY:ggwinfo,collision_reset
 use QP_m,          ONLY:QP_Vnl_xc,QP_ng_Sx,QP_table,QP_nb,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
 use wave_func,     ONLY:WF_load
#if defined _SC
 use SC,            ONLY:it_now,SC_bands
 use collisions_IO, ONLY:TEMP_COLLISIONS_v
 use fragments,     ONLY:fragment_status
#endif
 use WF_distribute, ONLY:WF_states_setup
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 !Work Space
 !
 type(ggwinfo)    ::isc,iscp
 type(PP_indexes) ::px
 integer                 :: i2,iq,ib,ibp,jb,ik,i_qp
 integer                 :: Sx_lower_band,Sx_upper_band
#if defined _RT
 integer                 :: jb2,ik_minus_q,COM_
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
 call PP_indexes_reset(px)
 !
 Sx_lower_band=1
 Sx_upper_band=E%nbm
 !
#if defined _SC
 !
 ! COLLISIONS 
 !
 if(l_collisions_IO) then
   Sx_upper_band=SC_bands(2)
   Sx_lower_band=SC_bands(1)
 endif
 !
#endif
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
 ! Quasi-particle list
 ! (all processor have all qp-states)
 !
 call WF_states_setup(qp_to_load=(/1,QP_n_states/),par_loop=.FALSE.)
 !
 ! Exchange Scattering states are distributed
 ! 
 call WF_states_setup(b_to_load=(/Sx_lower_band,Sx_upper_band/),qp_to_load=(/1,QP_n_states/),q_to_load=(/1,q%nbz/),k=k)
 !
 ! States required by the density
 !
 call WF_states_setup(b_to_load=(/1,E%nbm/),k_to_load=(/1,Xk%nibz/))
 !
 ! Report
 !
 call WF_states_setup(b_to_load=(/1,max(E%nbm,QP_nb)/),report=.TRUE.)
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
#if defined _RT
 call PARALLEL_index(px,(/QP_n_states/))
#else
 call PARALLEL_index(px,(/q%nbz,E%nbm/))
#endif
 call PP_redux_wait
 !
 ch='EXS'
#if defined _SC
 if (l_sc_run)        ch='HF@it'//trim(intc(it_now))
 if (l_collisions_IO) ch='HF collisions'
#endif
 !
#if defined _RT
 call live_timing(trim(ch),px%n_of_elements(myid+1))
#else
 if (l_sc_exx) then
   call live_timing(trim(ch),px%n_of_elements(myid+1)*E%nbf*&
        &(maxval(QP_table(:,2))-E%nbf)*maxval(QP_table(:,3)))
 else  
   call live_timing(trim(ch),px%n_of_elements(myid+1)*QP_n_states)
 endif
#endif
 !
 do i_qp=1,QP_n_states
   !
#if defined _RT
   if (.not.px%element_1D(i_qp)) cycle
   !        
   if (fragment_status(i_qp)/=-1)  then
     call live_timing(steps=1)
     cycle
   endif
   !
   TEMP_COLLISIONS_v=(0.,0.)
   !
#endif
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   !
#if defined _SC  
   !
   ! In Exact-Exchange only vc matrix elements so ... no cc' no vv' 
   ! (cv is not permitted by the order in QP_table)
   !                   (c)         (v)
   if (l_sc_exx.and.(ib>E%nbf.or.ibp<=E%nbf)) cycle
#endif
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
#if defined _RT
     ik_minus_q=isc%os(2)
#endif
     !
     do jb=Sx_lower_band,Sx_upper_band
       !      
#if !defined _RT
       if (.not.px%element_2D(iq,jb)) cycle
#endif
       isc%os(1)=jb
       iscp%os=isc%os
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
       do jb2=Sx_lower_band,SC_bands(2)
         !      
         iscp%os(1)=jb2
         if (any(isc%is/=iscp%is).or.any(isc%os/=iscp%os)) then
            call scatterBamp(iscp)
         else
            iscp%rhotw=isc%rhotw
         endif
         !
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
         TEMP_COLLISIONS_v(jb,jb2,ik_minus_q)=TEMP_COLLISIONS_v(jb,jb2,ik_minus_q)+DP_Sx
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
   if(l_collisions_IO) call COLLISIONS_IO_store(i_qp)
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
 call PP_indexes_reset(px)
 !
#if !defined _RT
 call PP_redux_wait(QP_Vnl_xc)
#endif
 !
end subroutine
