!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 ! Note about linewidths sign. Both at zero and finite temperature the sign follows the
 ! T-orderd convention (W>0 E<0, W<0 E>0). At finite temperature this is the consequence of
 ! the Wick rotation done to bring the theory on the real-axis.
 !
 ! This convention about sign is imposed (when reading DB's made at finite Temperature)
 ! in QP_apply_global_stretch and  QP_apply_DB_interpolation
 !
 use pars,          ONLY:SP,schlen
 use drivers,       ONLY:l_sc_run
 use units,         ONLY:HA2EV
 use QP_m,          ONLY:QP_t,QP_reset,QP_ctl_E,QP_ctl_Wc,QP_ctl_Wv,QP_ctl_Z,&
&                        QP_ctl_applied,QP_ctl_interp_neigh,QP_ctl_db,QP_ctl_Wc_E_ref,QP_ctl_Wv_E_ref
 use com,           ONLY:msg,depth
 use electrons,     ONLY:levels,n_sp_pol,E_duplicate,E_reset,&
&                        BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_nbands,BZ_DbGd_tot_nkpts
 use D_lattice,     ONLY:input_Tel_is_negative
 use R_lattice,     ONLY:bz_samp
 use parser_m,      ONLY:parser
 use global_XC,     ONLY:setup_global_XC,QP_MORE_SCISSOR,QP_MORE_SCIS_AND_STRE,QP_MORE_STRETCH,&
&                        K_kind,K_xc_functional,QP_MORE_NONE,G_kind,G_xc_functional,&
&                        X_kind,X_xc_functional,QP_DB_kind,QP_SE_NONE
 use QP_CTL_m,      ONLY:QP_ctl_t,reset_QP_ctl
 !
#include<memory.h>
 !
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
&                       ik_r,QP_SE_MORE,i_spin,QP_load_DB_err,nbf_m_SAVE(2)
 character(schlen)::ch
 character(1)     ::what
 character(11)    ::spin_ch(2)
 logical          ::is_def(4),GFs_from_DB,W_def(2)
 integer          ::Eid
 integer, allocatable :: DbGd_k_done(:)
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
 call E_reset(Fermi_en)
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
 if (.not.allocated(en%QP_corrected)) then
   YAMBO_ALLOC(en%QP_corrected,(en%nb,en%nk,n_sp_pol))
   en%QP_corrected=0
 endif 
 !
 DB_corrected=0
 QP_ctl_applied=.false.
 !
 ! Description initialization
 !
 spin_ch=(/'',''/)
 if (n_sp_pol==2) spin_ch=(/'  (spin up)','(spin down)'/)
 !
 do i_spin=1,n_sp_pol
   QP_ctl_from_DB(i_spin)%short_descr='[QP@'//what//trim(spin_ch(i_spin))//']'
   QP_ctl_input_or_impose(i_spin)%short_descr='[QP@'//what//trim(spin_ch(i_spin))//']'
   ch='External QP corrections ('//what//')'
   if (present(qp_impose)) ch='Internal QP corrections ('//what//')'
 enddo
 !
 ! Sectioning
 !
 if (depth>0 ) call section('=',trim(ch))
 if (depth==0) call section('+',trim(ch))
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
   if (QP_DB_kind/=QP_SE_NONE.and.QP_load_DB_err>0) then
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
&                                       QP_ctl_E(Eid,2,i_spin),0._SP,QP_ctl_E(Eid,3,i_spin)/)
   enddo
   !
   ! Then update scissor & stretching in the global_XC kinds 
   !
   QP_SE_MORE=QP_MORE_NONE
   if (any(QP_ctl_E(Eid,1,:)  /=0._SP)) QP_SE_MORE=QP_MORE_SCISSOR
   if (any(QP_ctl_E(Eid,2:3,:)/=1._SP)) QP_SE_MORE=QP_MORE_STRETCH
   if (any(QP_ctl_E(Eid,1,:)  /=0._SP).and.any(QP_ctl_E(Eid,2:3,:)/=1._SP)) QP_SE_MORE=QP_MORE_SCIS_AND_STRE
   !
   if (QP_SE_MORE>0.and.Eid==1) call setup_global_XC(what,X_kind,QP_SE_MORE,X_xc_functional)
   if (QP_SE_MORE>0.and.Eid==2) call setup_global_XC(what,K_kind,QP_SE_MORE,K_xc_functional)
   if (QP_SE_MORE>0.and.Eid==3) call setup_global_XC(what,G_kind,QP_SE_MORE,G_xc_functional)
   !
   QP_ctl_db(Eid)=trim(QP_ctl_from_DB(1)%db)
   !
   ! Now I move to the input file provided stretching and generalized QP parameters. 
   ! Those are contained in the QP_ctl* fields.
   !
   ! Note the units of QP_ctl_W:
   !
   !   QP_ctl_Wc/v(Eid,1,:) is in ev
   !   QP_ctl_Wc/v(Eid,2,:) is in ev
   !   QP_ctl_Wc/v(Eid,3,:) is ev^{-1}
   !
   do i_spin=1,n_sp_pol
     QP_ctl_input_or_impose(i_spin)%W(1:3)=(/QP_ctl_Wc(Eid,1,i_spin)/HA2EV,&
&                                            QP_ctl_Wc(Eid,2,i_spin)/HA2EV,QP_ctl_Wc(Eid,3,i_spin)*HA2EV/)
     QP_ctl_input_or_impose(i_spin)%W_E_ref(:)=(/QP_ctl_Wc_E_ref(Eid,i_spin)+en%E_CBm,&
&                                                QP_ctl_Wv_E_ref(Eid,i_spin)+en%E_VBM/)
     QP_ctl_input_or_impose(i_spin)%W(4:6)=(/QP_ctl_Wv(Eid,1,i_spin)/HA2EV,&
&                                            QP_ctl_Wv(Eid,2,i_spin)/HA2EV,QP_ctl_Wv(Eid,3,i_spin)*HA2EV/)
     QP_ctl_input_or_impose(i_spin)%Z=QP_ctl_Z(Eid,i_spin)
   enddo
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! 2] I am imposing a qp_impose that contains qp corrections
 !
 if (present(qp_impose)) QP_load_DB_err=QP_load_DB(band_range,en,k,qp_impose,&
&                                                QP_ctl_from_DB,msg_fmt,DB_corrected,GFs_from_DB)
 !
 ! GPL_EXCLUDE_END
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
 ! Finally I apply the generalized stretch obtained from the procedures 1,2 and 3.
 !
 call QP_apply_global_stretch(band_range,en,k,QP_ctl_from_DB,QP_ctl_input_or_impose,DB_corrected,.FALSE.)
 !
 ! Extend the QP corrections to the RIM energies
 !
 if (allocated(en%E_DbGd).and.allocated(en%Eo)) then
   !
   YAMBO_ALLOC(DbGd_k_done,(BZ_DbGd_tot_nkpts))
   DbGd_k_done=0
   !
   do ik_bz=1,k%nbz
     !
     ik_ibz=k%sstar(ik_bz,1)
     !
     do i1=1,BZ_DbGd_nkpt(ik_bz)
       ik_r=BZ_DbGd_table(ik_bz,i1,1)
       !
       if (DbGd_k_done(ik_r)==1) cycle
       !
       forall (ib=1:BZ_DbGd_nbands,i_spin=1:n_sp_pol) &
&                                        en%E_DbGd(ib,ik_r,i_spin)=en%E_DbGd(ib,ik_r,i_spin)+&
&                                        en%E(ib,ik_ibz,i_spin)-en%Eo(ib,ik_ibz,i_spin)
       DbGd_k_done(ik_r)=1
     enddo
     !
   enddo
   !
   YAMBO_FREE(DbGd_k_done)
   !
 endif
 !
 ! Fermi Level Updates
 !=====================
 !
 ! X/K -> Fermi Level Update
 !
 if ((Eid<3.and.allocated(en%Eo).and..not.allocated(en%GreenF)).or.allocated(en%E_DbGd)) then
   !
   if (input_Tel_is_negative) nbf_m_SAVE=(/en%nbf,en%nbm/)
   !
   if (.not.allocated(en%fo) .and. .not. l_sc_run) call OCCUPATIONS_Fermi(en,k,1)
   !
   if (input_Tel_is_negative) then
     en%nbf=nbf_m_SAVE(1)
     en%nbm=nbf_m_SAVE(2)
   endif
   !
 endif
 !
 ! G -> Fermi Level Update... but using the levels cooresponding to
 ! the K-points of the X grid !
 !
 if (Eid==3.and.allocated(en%Eo).and..not.allocated(en%GreenF).and..not.allocated(en%E_DbGd)) then
   !
   call E_duplicate(en,Fermi_en)
   !
   call OCCUPATIONS_Fermi(Fermi_en,k,1)
   !
   ! In OCCUPATIONS_Fermi the Fermi_en the %E and CBm and VMB are coherent. 
   ! Instead the en%E are out-of-date and are not shifted in OCCUPATIONS_Extend.
   ! This line is to prevent that CBm and VMB are not the same as en%E
   !
   en%E(:,:,:)=en%E(:,:,:)-Fermi_en%E_Fermi
   !
   call OCCUPATIONS_Extend(Fermi_en,en)
   !
   YAMBO_FREE(Fermi_en%E)
   !
 endif
 !
 ! CLEAN
 !
 call QP_reset(qp)
 call E_reset(Fermi_en)
 !
 if (.not.QP_ctl_applied) return
 !
 ! Reporting
 !
 call msg('r', '[QP] Fermi level variation [ev]:',en%E_Fermi*HA2EV)
 call msg('r', '[QP] Last Filled/Metallic band :',(/en%nbf,en%nbm/))
 !
 if (en%nbf==en%nbm) then
   !
   if (n_sp_pol==1) then
     !
     call msg('r','[QP] Ind. Gap Correction  [ev]:',&
&                  (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                  -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HA2EV)
     !
   else
     !
     call msg('r','[QP] Ind. Gap Correction  (up) [ev]:',&
&                  (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                  -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HA2EV)
     !
     call msg('r','                        (down) [ev]:',&
&                  (minval(en%E(en%nbf+1,:,2))-maxval(en%E(en%nbf,:,2)) &
&                  -minval(en%Eo(en%nbf+1,:,2))+maxval(en%Eo(en%nbf,:,2)) )*HA2EV)
     !
   endif
   !
 endif
 !
 end subroutine
