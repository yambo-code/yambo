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
subroutine QP_apply(bands,en,k,object,msg_fmt,qp_impose,QP_ctl_impose)
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
 use com,           ONLY:msg,depth
 use electrons,     ONLY:levels,n_sp_pol,E_duplicate,E_reset,&
&                        BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_nbands,BZ_DbGd_tot_nkpts
 use D_lattice,     ONLY:input_Tel_is_negative
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_actions_reset,n_QP_actions,QP_action,QP_CTL_copy,&
&                        QP_ctl_ID,QP_ctl_t,QP_ctl_msg_fmt,QP_ctl_xc_STRING,QP_ctl_what
 use global_XC,     ONLY:QP_SE_NONE,setup_global_XC
 !
#include<memory.h>
 !
 integer       ::bands(2)
 type(levels)  ::en
 type(bz_samp) ::k
 character(*)  ::msg_fmt
 character(1)  ::object
 type(QP_t),     optional::qp_impose
 type(QP_ctl_t), optional::QP_ctl_impose(n_sp_pol)
 !
 ! Work Space
 !
 type(levels)     ::Fermi_en
 integer          ::ik_bz,ik_ibz,ib,i1,ik_r,i_spin,nbf_m_SAVE(2),i_a
 logical          ::l_E_done
 integer, allocatable :: DbGd_k_done(:)
 !
 ! Resets
 !========
 call QP_actions_reset( )
 call E_reset(Fermi_en)
 QP_ctl_msg_fmt=msg_fmt
 QP_ctl_xc_STRING=" "
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
 if (depth>0 ) call section('=',"External/Internal QP corrections")
 if (depth==0) call section('+',"External/Internal QP corrections")
 !
 !==============
 ! QP's section
 !==============
 if (present(qp_impose)) then
   call QP_apply_QP(bands,en,k,qp_impose)
 endif
 !
 !=========
 ! Actions
 !=========
 !
 l_E_done=.FALSE.
 !
 do i_a=1,n_QP_actions
   !
   if (.not.QP_action(i_a)%active) cycle
   !
   if (len_trim(QP_action(i_a)%CTL(1)%db)>0.and.&
&          trim(QP_action(i_a)%CTL(1)%db)/="none") call QP_load_DB(i_a,bands,en,k)
   !
   if (.not.QP_action(i_a)%active) cycle
   !
   call QP_apply_the_ACTION(i_a,bands,en,k,l_E_done)
   !
 enddo
 !
 if ( .not.any( QP_action(:)%active ) ) return
 !
 call setup_global_XC(trim(QP_ctl_what),QP_SE_NONE,STRING=QP_ctl_xc_STRING)
 !
 ! Energy corrected?
 !
 if (.not.l_E_done) then
   !
   call E_reset(Fermi_en)
   YAMBO_FREE(DbGd_k_done)
   call QP_actions_reset( )
   !
   return
   !
 endif
 !
 ! Extend the QP corrections to the RIM energies
 !========
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
       !
       ik_r=BZ_DbGd_table(ik_bz,i1,1)
       !
       if (DbGd_k_done(ik_r)==1) cycle
       !
       do ib=1,BZ_DbGd_nbands
         do i_spin=1,n_sp_pol
           en%E_DbGd(ib,ik_r,1)=en%E_DbGd(ib,ik_r,1)+ (en%E(ib,ik_ibz,i_spin)-en%Eo(ib,ik_ibz,i_spin))
         enddo
       enddo
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
 if ((QP_ctl_ID<3.and.allocated(en%Eo).and..not.allocated(en%GreenF)).or.allocated(en%E_DbGd)) then
   !
   if (input_Tel_is_negative) nbf_m_SAVE=(/en%nbf,en%nbm/)
   !
   if (.not.allocated(en%fo) .and. .not. l_sc_run) then
     if (QP_ctl_ID==1) call OCCUPATIONS_Fermi(en,k,OBJ="X")
     if (QP_ctl_ID==2) call OCCUPATIONS_Fermi(en,k,OBJ="K")
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
 ! CLEAN
 !
 call E_reset(Fermi_en)
 YAMBO_FREE(DbGd_k_done)
 call QP_actions_reset( )
 !
 if (en%nbf==en%nbm) then
   !
   if (n_sp_pol==1) then
     !
     call msg('r','[QP apply] Ind. Gap Correction   [ev]:',&
&                  (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                  -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HA2EV)
     !
   else
     !
     call msg('r','[QP apply] Ind. Gap Correction  (up) [ev]:',&
&                  (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                  -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HA2EV)
     !
     call msg('r','                              (down) [ev]:',&
&                  (minval(en%E(en%nbf+1,:,2))-maxval(en%E(en%nbf,:,2)) &
&                  -minval(en%Eo(en%nbf+1,:,2))+maxval(en%Eo(en%nbf,:,2)) )*HA2EV)
     !
   endif
   !
 endif
 !
 end subroutine
