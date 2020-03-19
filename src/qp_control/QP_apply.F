!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine QP_apply(bands,en,k,object,msg_fmt,qp_impose,QP_ctl_impose,main_section)
 !
 ! This routine manages the external qp corrections.
 ! The task is tricky, as YAMBO has three different energy
 ! types. Moreover the Fermi level is decided on the basis
 ! of the X energies (supposed to be uniformly spread).
 !
 ! Objects
 !:::::::::
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
 use drivers,       ONLY:l_sc_run
 use units,         ONLY:HA2EV
 use QP_m,          ONLY:QP_t
 use interfaces,    ONLY:OCCUPATIONS_Fermi
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol,E_duplicate,E_reset
 use D_lattice,     ONLY:input_Tel_is_negative
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_actions_reset,n_QP_actions,QP_action,QP_CTL_copy,&
&                        QP_ctl_ID,QP_ctl_t,QP_ctl_msg_fmt,QP_ctl_xc_STRING,QP_ctl_what,&
&                        At_least_one_band_corrected
 use global_XC,     ONLY:QP_SE_NONE,setup_global_XC
 !
#include<memory.h>
 !
 integer       ::bands(2)
 type(levels)  ::en
 type(bz_samp) ::k
 character(*)  ::msg_fmt
 character(1)  ::object
 logical,        optional::main_section
 type(QP_t),     optional::qp_impose
 type(QP_ctl_t), optional::QP_ctl_impose(n_sp_pol)
 !
 ! Work Space
 !
 type(levels)     ::Fermi_en
 integer          ::i_spin,nbf_m_SAVE(2),i_a
 character(2)     ::sec_mode
 !
 ! Resets
 !========
 call QP_actions_reset( )
 call E_reset(Fermi_en)
 QP_ctl_msg_fmt=msg_fmt
 QP_ctl_xc_STRING=" "
 At_least_one_band_corrected=.FALSE.
 !
 ! CTLs
 !======
 ! ... Input file driven
 call QP_apply_dump_user_CTLs( en, object )
 !
 ! ... externally defined
 if (present(QP_ctl_impose)) then
   n_QP_actions=n_QP_actions+1
   QP_action(n_QP_actions)%active=.TRUE.
   do i_spin=1,n_sp_pol
     call QP_CTL_copy(QP_ctl_impose(i_spin),QP_action(n_QP_actions)%CTL(i_spin))
   enddo
 endif
 !
 ! I am imposing a qp_impose that contains qp corrections
 !                ===========
 if (present(qp_impose)) then
   n_QP_actions=n_QP_actions+1
   QP_action(n_QP_actions)%active=.TRUE.
   QP_action(n_QP_actions)%desc="Internally defined QP database"
 endif
 !
 ! Is there anything to do?
 !
 if ( .not.any( QP_action(:)%active ) ) return
 !
 ! Sectioning
 !=============
 sec_mode="+P"
 if (present(main_section)) then
   if (main_section) sec_mode="*"
 endif
 call section(trim(sec_mode),"External/Internal QP corrections")
 !
 !==============
 ! QP's section
 !==============
 if (present(qp_impose)) call QP_apply_QP(n_QP_actions,bands,en,k,qp_impose)
 !
 !=========
 ! Actions
 !=========
 !
 do i_a=1,n_QP_actions
   !
   if (.not.QP_action(i_a)%active) cycle
   !
   ! QP_load_DB
   ! |
   ! .- loads the QP db
   ! .- calls QP_apply_QP
   !    |
   !    . INTERPOLATES
   !    . Fit
   !
   if (len_trim(QP_action(i_a)%CTL(1)%db)>0.and.&
&          trim(QP_action(i_a)%CTL(1)%db)/="none") call QP_load_DB(i_a,bands,en,k)
   !
   if (.not.QP_action(i_a)%active) cycle
   !
   call msg("sr","[QP_apply] Action to be applied",trim(QP_action(i_a)%desc))
   !
   ! QP_apply_the_ACTION
   ! |
   ! .- applies just the fit
   !
   call QP_apply_the_ACTION(i_a,bands,en,k)
   !
 enddo
 !
 if ( .not.any( QP_action(:)%active ) ) then 
   !
   call section('-P'," ")
   !
   return
   !
 endif
 !
 call setup_global_XC(trim(QP_ctl_what),QP_SE_NONE,STRING=QP_ctl_xc_STRING)
 !
 ! Energy corrected?
 !
 if (.not.At_least_one_band_corrected) then
   !
   call E_reset(Fermi_en)
   call QP_actions_reset( )
   !
   call section('-P'," ")
   !
   return
   !
 endif
 !
 ! Extend the QP corrections to the RIM energies
 !========
 if (allocated(en%E_DbGd)) then
   !
   call section('p'," Extending the correction to the DbGd")
   !
   call QP_apply_to_the_DbGd(bands,en,k)
   !
 endif
 !
 call section('p'," QP corrections report")
 !==========================================
 !
 ! Fermi Level Updates
 !=====================
 !
 ! X/K -> Fermi Level Update
 !
 if ((QP_ctl_ID<3.and.allocated(en%Eo).and..not.allocated(en%GreenF)).or.allocated(en%E_DbGd)) then
   !
   if (input_Tel_is_negative) nbf_m_SAVE=(/en%nbf,en%nbm/)
   !
   if (.not.allocated(en%fo) .and. .not. l_sc_run) then
     if (QP_ctl_ID==1) call OCCUPATIONS_Fermi(en,k,OBJ="X+QP")
     if (QP_ctl_ID==2) call OCCUPATIONS_Fermi(en,k,OBJ="K+QP")
   endif
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
 if (QP_ctl_ID==3.and.allocated(en%Eo).and..not.allocated(en%GreenF).and..not.allocated(en%E_DbGd)) then
   !
   call E_duplicate(en,Fermi_en)
   !
   call OCCUPATIONS_Fermi(Fermi_en,k,mode="OCCUPATIONS")
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
 ! Energies report
 !
 call REPORT_Energies(en%E,k,k,en%nb,(/1,k%nibz/),'Eqp',.FALSE.,.TRUE.)
 !
 ! CLEAN
 !
 call E_reset(Fermi_en)
 call QP_actions_reset( )
 !
 if (en%nbf==en%nbm) then
   !
   call QP_apply_gap_report(en,"built-in")
   if (allocated(en%E_DbGd)) call QP_apply_gap_report(en,"DbGd")
   !
 endif
 !
 call msg('r','')
 !
 call section('-P'," ")
 !
 contains
   !
   subroutine QP_apply_gap_report(E,title)
    use pars, ONLY:SP
    character(*)  ::title
    type(levels)  ::E
    ! Work space
    real(SP)      ::Ego(2),Eg(2)
    integer       ::i_spin
    character(100)::space,msg_ch
    space(1:100)=" "
    do i_spin=1,n_sp_pol
      Ego(i_spin)=minval(E%Eo(E%nbf+1,:,i_spin))-maxval(E%Eo(E%nbf,:,i_spin))
      if (title=="DbGd") then
        Eg(i_spin)=minval(E%E_DbGd(E%nbf+1,:,i_spin))-maxval(E%E_DbGd(E%nbf,:,i_spin))
      else
        Eg(i_spin)=minval(E%E(E%nbf+1,:,i_spin))-maxval(E%E(E%nbf,:,i_spin))
      endif
      !
      msg_ch='['//title//'] Minimum Indirect Gap        [ev]'
      if (n_sp_pol==2) then
        if (i_spin==1) msg_ch='['//title//'] Minimum Indirect Gap         (up)[ev]'
        if (i_spin==2) msg_ch='['//title//']                            (down)[ev]'
      endif
      call msg('r',trim(msg_ch),Ego(i_spin)*HA2EV)
      !
      msg_ch='['//title//'] Minimum Ind Gap Correction  [ev]'
      if (n_sp_pol==2) then
        if (i_spin==1) msg_ch='['//title//'] Minimum Ind Gap Correction   (up)[ev]'
        if (i_spin==2) msg_ch='['//title//']                            (down)[ev]'
      endif
      call msg('r',trim(msg_ch),(Eg(i_spin)-Ego(i_spin))*HA2EV)
      !
    enddo
   end subroutine
   !
 end subroutine
