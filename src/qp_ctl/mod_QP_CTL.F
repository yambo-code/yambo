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
module QP_CTL_m
 !
 use pars,  ONLY:SP,schlen,IP
 use units, ONLY:HA2EV
 !
 implicit none
 !
 ! QP Pars
 !
 !?fnQP_db= " E W Z < db-pp.qp "     # [?] QP database.
 !% ?fnQP_E
 !  0 |  0 | 0 |               # [?] QP parameters (E).
 !% 
 !% ?fnQP_W
 !  0 |  0 | 0 | 0 |           # [?] QP parameters (W).
 !%
 !?fbQP_Z= (  0.00000 ,  0.00000 )  # [?] QP parameters (Z).
 !  
 ! 1(X) 2(K) 3(G) 
 !
 type QP_ctl_t
   !
   ! Convention is first c then v
   !
   integer    :: interp_neigh
   real(SP)   :: db_scissor
   real(SP)   :: fit_scissor
   real(SP)   :: E(4)
   real(SP)   :: W(6)
   real(SP)   :: E_err(2)
   real(SP)   :: W_err(2)
   complex(SP):: Z
   character(schlen):: db
   character(schlen):: short_descr
 end type
 !
 contains
   !
   subroutine reset_QP_ctl(QP_ctl)
     type(QP_ctl_t)::QP_ctl
     QP_ctl%db="none"
     QP_ctl%E=(/0.,1.,0.,1./)
     QP_ctl%W=0.
     QP_ctl%Z=(1.,0.)
     QP_ctl%E_err=0.
     QP_ctl%W_err=0.
     QP_ctl%db_scissor=0.
     QP_ctl%fit_scissor=0.
   end subroutine
   !
   subroutine QP_apply(band_range,en,k,object,msg_fmt,qp_impose,QP_ctl_impose)
     !
     ! This routine manages the external qp corrections.
     ! The task is tricky, as YAMBO has three different energy
     ! types. Moreover the Fermi level is decided on the basis
     ! of the X energies (supposed to be uniformly spread).
     !
     ! scheme
     !::::::::
     ! |_ qp_load_DB
     ! |      |_ qp_apply_DB_interpolation
     ! |      |_ qp_fit_DB_values
     ! |_ qp_apply_global_stretch
     ! |_ (if X or K) fermi_level
     !
     ! Eid
     !::::::
     ! 1 : X (object = "X")
     ! 2 : BSK (object = "K")
     ! 3 : G (object = "G")
     !
     use memory_m,      ONLY:mem_est
     use QP_m,          ONLY:QP_t,QP_reset,QP_ctl_E,QP_ctl_Wc,QP_ctl_Wv,QP_ctl_Z,&
&                            QP_ctl_applied,QP_ctl_interp_neigh,QP_ctl_db
     use com,           ONLY:msg,depth
     use electrons,     ONLY:levels,E_reset,n_sp_pol,&
&                            BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_nbands,BZ_RIM_tot_nkpts
     use R_lattice,     ONLY:bz_samp,nXkibz
     use parser_m,      ONLY:parser
     use global_XC,     ONLY:setup_global_XC,MORE_SCISSOR,MORE_SCIS_AND_STRE,MORE_STRETCH,&
&                            K_kind,K_xc_functional,MORE_NONE,G_kind,G_xc_functional,&
&                            X_kind,X_xc_functional,QP_DB_kind,SE_NONE
     integer       ::band_range(2)
     type(levels)  ::en
     type(bz_samp) ::k
     character(*)  ::msg_fmt
     character(1)  ::object
     type(QP_t),       optional::qp_impose
     type(QP_ctl_t),   optional::QP_ctl_impose(n_sp_pol)
     !
     ! Work Space
     !
     type(levels)     ::Fermi_en
     type(QP_t)       ::qp
     type(QP_ctl_t)   ::QP_ctl_from_DB(n_sp_pol),QP_ctl_input_or_impose(n_sp_pol)
     integer          ::DB_corrected(en%nb,en%nk,n_sp_pol),ik_bz,ik_ibz,ib,i1,&
&                       ik_r,SE_MORE,i_spin,QP_load_DB_err
     character(schlen)::ch
     character(1)     ::what
     character(11)    ::spin_ch(2)
     logical          ::is_def(4),GFs_from_DB,W_def(2)
     integer          ::Eid
     integer, allocatable :: RIM_k_done(:)
     integer, external    :: QP_load_DB
     !
     ! Resets
     !
     if (object=="X") Eid=1
     if (object=="K") Eid=2
     if (object=="G") Eid=3
     !
     call QP_reset(qp)
     do i_spin=1,n_sp_pol
       call reset_QP_ctl(QP_ctl_from_DB(i_spin))
       call reset_QP_ctl(QP_ctl_input_or_impose(i_spin))
     enddo
     call E_Reset(Fermi_en)
     !
     ! If I'm not imposing a fit (QP_ctl_impose) of QP (qp_impose)
     ! return if the field are not present in the input file
     !
     if (.not.present(QP_ctl_impose).and..not.present(qp_impose)) then
       if (Eid==1) what='X'
       if (Eid==2) what='K'
       if (Eid==3) what='G'
       call parser(what//'fnQP_E',is_def(1))
       call parser(what//'fnQP_up_E',is_def(2))
       call parser(what//'fnQP_dn_E',is_def(3))
       call parser(what//'fnQPdb',is_def(4))
       call parser(what//'fnQP_Wv',W_def(1))
       call parser(what//'fnQP_Wc',W_def(2))
       if (.not.any((/is_def,W_def/))) return
     endif
     !
     ! Allocation of qp_done table to keep track of the corrections
     ! done
     !
     if (.not.associated(en%QP_corrected)) then
       allocate(en%QP_corrected(en%nb,en%nk,n_sp_pol))
       call mem_est("E-QP_corrected",(/size(en%QP_corrected)/),(/IP/))
       en%QP_corrected=0
     endif 
     !
     DB_corrected=0
     QP_ctl_applied=.false.
     !
     ! Description initialization
     !
     spin_ch=(/'',''/)
     if (n_sp_pol==2) spin_ch=(/'(spin down)','  (spin up)'/)
     !
     do i_spin=1,n_sp_pol
       QP_ctl_from_DB(i_spin)%short_descr='[QP@'//what//trim(spin_ch(i_spin))//']'
       QP_ctl_input_or_impose(i_spin)%short_descr='[QP@'//what//trim(spin_ch(i_spin))//']'
       ch='External QP corrections ('//what//')'
       if (present(qp_impose)) ch='Internal QP corrections ('//what//')'
     enddo
     !
     if (depth>0) then
       call section('=',trim(ch))
     else 
       call section('*',trim(ch))
     endif
     !
     ! Interpolation neighbours is always input file controlled
     !
     QP_ctl_input_or_impose(:)%interp_neigh=QP_ctl_interp_neigh(Eid)
     QP_ctl_from_DB(:)%interp_neigh=QP_ctl_interp_neigh(Eid)
     !
     ! NOW I HAVE DIFFERENT OPTIONS ... 
     !
     ! 1] I am using the input file specifed QP corrections (contained in the qp_impose type)
     !
     if (.not.present(qp_impose).and..not.present(QP_ctl_impose)) then
       !
       QP_ctl_from_DB(:)%db=QP_ctl_db(Eid)
       QP_load_DB_err=QP_load_DB(band_range,en,k,qp,QP_ctl_from_DB,msg_fmt,DB_corrected,GFs_from_DB)
       !
       ! Update QP db kind in the global_XC kinds 
       !
       if (QP_DB_kind/=SE_NONE.and.QP_load_DB_err>0) then
         if (Eid==1) X_kind=QP_DB_kind
         if (Eid==2) K_kind=QP_DB_kind
         if (Eid==3) G_kind=QP_DB_kind
       endif
       !
       ! Now I have to include the input file QP parameters in QP_ctl
       ! being careful that the scissor must go in QP_ctl_input_or_impose%E(1)
       !
       do i_spin=1,n_sp_pol
         QP_ctl_input_or_impose(i_spin)%E=(/QP_ctl_E(Eid,1,i_spin)/HA2EV,&
&                                           QP_ctl_E(Eid,2,i_spin),0._SP,QP_ctl_E(Eid,3,i_spin)/)
       enddo
       !
       ! Then update scissor & stretching in the global_XC kinds 
       !
       SE_MORE=MORE_NONE
       if (any(QP_ctl_E(Eid,1,:)/=0.))   SE_MORE=MORE_SCISSOR
       if (any(QP_ctl_E(Eid,2:3,:)/=1.)) SE_MORE=MORE_STRETCH
       if (any(QP_ctl_E(Eid,1,:)/=0.).and.any(QP_ctl_E(Eid,2:3,:)/=1.)) SE_MORE=MORE_SCIS_AND_STRE
       !
       if (SE_MORE>0.and.Eid==1) call setup_global_XC(what,X_kind,SE_MORE,X_xc_functional)
       if (SE_MORE>0.and.Eid==2) call setup_global_XC(what,K_kind,SE_MORE,K_xc_functional)
       if (SE_MORE>0.and.Eid==3) call setup_global_XC(what,G_kind,SE_MORE,G_xc_functional)
       !
       QP_ctl_db(Eid)=trim(QP_ctl_from_DB(1)%db)
       !
       ! Note the units of QP_ctl_W:
       !
       !   QP_ctl_Wc/v(Eid,1,:) is in ev
       !   QP_ctl_Wc/v(Eid,2,:) is adim
       !   QP_ctl_Wc/v(Eid,3,:) is ev^{-1}
       !
       do i_spin=1,n_sp_pol
         QP_ctl_input_or_impose(i_spin)%W(1:3)=(/QP_ctl_Wc(Eid,1,i_spin)/HA2EV,&
&                                                QP_ctl_Wc(Eid,2,i_spin),QP_ctl_Wc(Eid,3,i_spin)*HA2EV/)
         QP_ctl_input_or_impose(i_spin)%W(4:6)=(/QP_ctl_Wv(Eid,1,i_spin)/HA2EV,&
&                                                QP_ctl_Wv(Eid,2,i_spin),QP_ctl_Wv(Eid,3,i_spin)*HA2EV/)
         QP_ctl_input_or_impose(i_spin)%Z=QP_ctl_Z(Eid,i_spin)
       enddo
       !
     endif
     !
     !
     ! 3] I am using an externally defined FIT parameters
     !
     if (present(QP_ctl_impose)) then
       do i_spin=1,n_sp_pol
         QP_ctl_input_or_impose(i_spin)%E=QP_ctl_impose(i_spin)%E
         QP_ctl_input_or_impose(i_spin)%W=QP_ctl_impose(i_spin)%W
         QP_ctl_input_or_impose(i_spin)%Z=QP_ctl_impose(i_spin)%Z
       enddo
     endif
     !
     ! Finally I apply the generalized stretch
     !
     call QP_apply_global_stretch(band_range,en,QP_ctl_from_DB,QP_ctl_input_or_impose,DB_corrected)
     !
     ! Fermi Level Updates
     !=====================
     !
     ! X/K -> Fermi Level Update
     !
     if ((Eid<3.and.associated(en%Eo)).and..not.associated(en%GreenF)) then
       !
       if (associated(en%E_RIM)) then
         !
         allocate(RIM_k_done(BZ_RIM_tot_nkpts))
         RIM_k_done=0
         !
         do ik_bz=1,k%nbz
           !
           ik_ibz=k%sstar(ik_bz,1)
           !
           do i1=1,BZ_RIM_nkpt(ik_bz)
             ik_r=BZ_RIM_table(ik_bz,i1)
             !
             if (RIM_k_done(ik_r)==1) cycle
             !
             forall (ib=1:BZ_RIM_nbands,i_spin=1:n_sp_pol) &
&                                        en%E_RIM(ib,ik_r,i_spin)=en%E_RIM(ib,ik_r,i_spin)+&
&                                        en%E(ib,ik_ibz,i_spin)-en%Eo(ib,ik_ibz,i_spin)
             RIM_k_done(ik_r)=1
           enddo
           !
         enddo
         !
         deallocate(RIM_k_done)
         !
       endif
       !
       call OCCUPATIONS_Fermi(en,k,1)
       !
     endif
     !
     ! G -> Fermi Level Update... but using the levels cooresponding to
     ! the K-points of the X grid !
     !
     if ((Eid==3.and.associated(en%Eo)).and..not.associated(en%GreenF)) then
       !
       Fermi_en%nb=en%nb
       Fermi_en%nk=nXkibz
       !
       allocate(Fermi_en%E(Fermi_en%nb,Fermi_en%nk,n_sp_pol))
       Fermi_en%E(:,:,:)=en%E(:,:nXkibz,:)
       !
       call OCCUPATIONS_Fermi(Fermi_en,k,1)
       call OCCUPATIONS_Extend(Fermi_en,en)
       !
       nullify(Fermi_en%E)
     endif
     !
     ! CLEAN
     !
     call QP_reset(qp)
     call E_Reset(Fermi_en)
     !
     if (.not.QP_ctl_applied) return
     !
     ! Reporting
     !
     call msg('r', '[QP] Fermi level variation [ev]:',en%Efermi(1)*HA2EV)
     call msg('r',   '[QP] Last Filled/Metallic band :',(/en%nbf,en%nbm/))
     if (en%nbf==en%nbm) then
       if (n_sp_pol==1) then
         call msg('r','[QP] Ind. Gap Correction  [ev]:',&
&                  (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                  -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HA2EV)
         !
         !
       endif
       !
     endif
     !
     end subroutine
     !
end module
