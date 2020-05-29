!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 use electrons,     ONLY:levels,n_sp_pol
 use D_lattice,     ONLY:input_Tel_is_negative
 use R_lattice,     ONLY:bz_samp,nkibz,nXkibz
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
 integer          ::i_spin,nbf_m_SAVE(2),i_a
 character(2)     ::sec_mode
 !
 ! Resets
 !========
 call QP_actions_reset( )
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
   call QP_actions_reset( )
   !
   call section('-P'," ")
   !
   return
   !
 endif
 !
 ! The extension to the Fine Grid has been removed from here and should be included in 
 ! QP_transfer, inside QP_apply_interpolate_QP
 !
 call section('p'," QP corrections report")
 !==========================================
 !
 ! Fermi Level Updates
 !=====================
 !
 if (input_Tel_is_negative) nbf_m_SAVE=(/en%nbf,en%nbm/)
 !
 if (.not.allocated(en%fo) .and. .not. l_sc_run) then
   if (QP_ctl_ID==1) call OCCUPATIONS_Fermi(en,k,OBJ="X+QP")
   if (QP_ctl_ID==2) call OCCUPATIONS_Fermi(en,k,OBJ="K+QP")
   if (QP_ctl_ID==3) then
     call OCCUPATIONS_Fermi(en,k,OBJ="G+QP")
     if (nkibz/=nXkibz) call warning("G and X k-grids have different number of k-points. G Fermi level potentially not alligned")
   endif
 endif
 !
 if (input_Tel_is_negative) then
   en%nbf=nbf_m_SAVE(1)
   en%nbm=nbf_m_SAVE(2)
 endif
 !
 ! Energies report
 !-----------------
 !
 if (en%nbf==en%nbm) then
   !
   call QP_apply_gap_report(en,"coarse-grid")
   if (allocated(en%E_FineGd)) call QP_apply_gap_report(en,"FineGd")
   call msg('r','')
   !
 endif
 !
 call REPORT_Energies(en%E,k,k,en%nb,(/1,k%nibz/),'Eqp',.FALSE.,.TRUE.)
 !
 ! CLEAN
 !
 call QP_actions_reset( )
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
      if (title=="FineGd") then
        Eg(i_spin)=minval(E%E_FineGd(E%nbf+1,:,i_spin))-maxval(E%E_FineGd(E%nbf,:,i_spin))
      else
        Eg(i_spin)=minval(E%E(E%nbf+1,:,i_spin))-maxval(E%E(E%nbf,:,i_spin))
      endif
      !
      msg_ch='['//title//'] Minimum Bare Gap        [ev]'
      if (n_sp_pol==2) then
        if (i_spin==1) msg_ch='['//title//'] Minimum Bare Gap         (up)[ev]'
        if (i_spin==2) msg_ch='['//title//']                        (down)[ev]'
      endif
      call msg('r',trim(msg_ch),Ego(i_spin)*HA2EV)
      !
      msg_ch='['//title//'] Minimum Gap Correction  [ev]'
      if (n_sp_pol==2) then
        if (i_spin==1) msg_ch='['//title//'] Minimum Gap Correction   (up)[ev]'
        if (i_spin==2) msg_ch='['//title//']                        (down)[ev]'
      endif
      call msg('r',trim(msg_ch),(Eg(i_spin)-Ego(i_spin))*HA2EV)
      !
    enddo
   end subroutine
   !
 end subroutine
