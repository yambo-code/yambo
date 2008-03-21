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
module QP_CTL_m
 !
 use pars,  ONLY:SP,schlen,IP,HARTREE
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
     implicit none
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
   subroutine QP_apply(band_range,en,k,Eid,msg_fmt,qp_impose,QP_ctl_impose)
     !
     ! This routine manages the external qp corrections.
     ! The task is tricky, as YAMBO has three different energy
     ! types. Moreover the Fermi level is decided on the basis
     ! of the X energies (supposed to be uniformly spread).
     !
     ! scheme
     !::::::::
     ! |_ qp_apply_db
     ! |      |_ qp_apply_interpolate 
     ! |      |_ qp_apply_fit 
     ! |_ qp_apply_stretch
     ! |_ (if X or K) fermi_level
     !
     ! Eid
     !::::::
     ! 1 : X
     ! 2 : BSK
     ! 3 : G
     !
     use memory_m,      ONLY:mem_est
     use QP_m,          ONLY:QP_t,QP_reset,QP_ctl_E,QP_ctl_W,QP_ctl_Z,&
&                            QP_ctl_applied,QP_ctl_interp_neigh,QP_ctl_db
     use D_lattice,     ONLY:Tel
     use com,           ONLY:msg,depth
     use electrons,     ONLY:levels,E_reset,n_sp_pol,&
&                            BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_nbands,BZ_RIM_tot_nkpts
     use R_lattice,     ONLY:bz_samp,nXkibz
     use parser_m,      ONLY:parser
     use global_XC,     ONLY:setup_global_XC,MORE_SCISSOR,MORE_SCIS_AND_STRE,MORE_STRETCH,&
&                            K_kind,K_xc_functional,MORE_NONE,G_kind,G_xc_functional,&
&                            X_kind,X_xc_functional,QP_DB_kind,SE_NONE
     implicit none
     integer       ::Eid,band_range(2)
     type(levels)  ::en
     type(bz_samp) ::k
     character(*)  ::msg_fmt
     type(QP_t),       optional::qp_impose
     type(QP_ctl_t),   optional::QP_ctl_impose
     !
     ! Work Space
     !
     type(levels)     ::Fermi_en
     type(QP_t)       ::qp
     type(QP_ctl_t)   ::QP_ctl_from_DB,QP_ctl_input_or_impose
     integer          ::DB_corrected(en%nb,en%nk),ik_bz,ik_ibz,ib,i1,ik_r,SE_MORE
     character(schlen)::ch
     character(1)     ::what
     logical          ::is_def(2)
     integer, allocatable :: RIM_k_done(:)
     !
     ! Resets
     !
     call QP_reset(qp)
     call reset_QP_ctl(QP_ctl_from_DB)
     call reset_QP_ctl(QP_ctl_input_or_impose)
     call E_Reset(Fermi_en)
     !
     ! If I'm not imposing a fit (QP_ctl_impose) of QP (qp_impose)
     ! return if the field are not present in the input file
     !
     if (.not.present(QP_ctl_impose).and..not.present(qp_impose)) then
       select case(Eid)
         case(1)
           call parser('XfnQP_E',is_def(1))
           call parser('XfnQPdb',is_def(2))
           what='X'
         case(2)
           call parser('KfnQP_E',is_def(1))
           call parser('KfnQPdb',is_def(2))
           what='K'
         case(3)
           call parser('GfnQP_E',is_def(1))
           call parser('GfnQPdb',is_def(2))
           what='G'
       end select
       if (.not.any((/is_def/))) return
     endif
     !
     ! Allocation of qp_done table to keep track of the corrections
     ! done
     !
     if (.not.associated(en%qp_done)) then
       allocate(en%qp_done(en%nb,en%nk))
       call mem_est("E-qp_done",(/size(en%qp_done)/),(/IP/))
       en%qp_done=0
     endif 
     !
     DB_corrected=0
     QP_ctl_applied=.false.
     !
     ! Description initialization
     !
     if (Eid==1) then
       QP_ctl_from_DB%short_descr='[QP@X]'
       QP_ctl_input_or_impose%short_descr='[QP@X]'
       ch='External QP corrections (X)'
     endif
     if (Eid==2) then
       QP_ctl_from_DB%short_descr='[QP@K]'
       QP_ctl_input_or_impose%short_descr='[QP@K]'
       ch='External QP corrections (K)'
     endif
     if (Eid==3) then
       QP_ctl_from_DB%short_descr='[QP@G]'
       QP_ctl_input_or_impose%short_descr='[QP@G]'
       ch='External QP corrections (G)'
       if (present(qp_impose)) ch='Internal QP corrections (G)'
     endif
     !
     ! Section 
     !
     if (depth>0) then
       call section('=',trim(ch))
     else 
       call section('*',trim(ch))
     endif
     !
     ! Interpolation neighbours is always input file controlled
     !
     QP_ctl_input_or_impose%interp_neigh=QP_ctl_interp_neigh(Eid)
     QP_ctl_from_DB%interp_neigh=QP_ctl_interp_neigh(Eid)
     !
     ! NOW I HAVE DIFFERENT OPTIONS ... 
     !
     ! 1] I am using the input file QP parameters
     !
     if (.not.present(qp_impose).and..not.present(QP_ctl_impose)) then
       !
       ! GPL_EXCLUDE_START
       !
       QP_ctl_from_DB%db=QP_ctl_db(Eid)
       call QP_apply_db(band_range,en,k,qp,QP_ctl_from_DB,msg_fmt,DB_corrected)
       !
       ! GPL_EXCLUDE_END
       !
       ! Update QP db kind in the global_XC kinds 
       !
       if (QP_DB_kind/=SE_NONE) then
         if (Eid==1) X_kind=QP_DB_kind
         if (Eid==2) K_kind=QP_DB_kind
         if (Eid==3) G_kind=QP_DB_kind
       endif
       !
       ! Now I have to include the input file QP parameters in QP_ctl
       ! being careful that the scissor must go in QP_ctl_input_or_impose%E(1)
       !
       QP_ctl_input_or_impose%E=(/QP_ctl_E(Eid,1)/HARTREE,QP_ctl_E(Eid,2),0._SP,QP_ctl_E(Eid,3)/)
       !
       ! Then update scissor & stretching in the global_XC kinds 
       !
       SE_MORE=MORE_NONE
       if (QP_ctl_E(Eid,1)/=0.) SE_MORE=MORE_SCISSOR
       if (any(QP_ctl_E(Eid,2:3)/=1.)) SE_MORE=MORE_STRETCH
       if (QP_ctl_E(Eid,1)/=0..and.any(QP_ctl_E(Eid,2:3)/=1.)) SE_MORE=MORE_SCIS_AND_STRE
       !
       if (SE_MORE>0.and.Eid==1) call setup_global_XC(what,X_kind,SE_MORE,X_xc_functional)
       if (SE_MORE>0.and.Eid==2) call setup_global_XC(what,K_kind,SE_MORE,K_xc_functional)
       if (SE_MORE>0.and.Eid==3) call setup_global_XC(what,G_kind,SE_MORE,G_xc_functional)
       !
       QP_ctl_db(Eid)=trim(QP_ctl_from_DB%db)
       !
       ! Note the QP_ctl_W(Eid,1/3) is in ev^{-1} while QP_ctl_W(Eid,2/4) is in
       !  ev^{-2}
       !
       QP_ctl_input_or_impose%W=(/0._SP,QP_ctl_W(Eid,1)/HARTREE,QP_ctl_W(Eid,2)/HARTREE**2.,&
&                     0._SP,QP_ctl_W(Eid,3)/HARTREE,QP_ctl_W(Eid,4)/HARTREE**2./)
       QP_ctl_input_or_impose%Z=QP_ctl_Z(Eid)
       !
     endif
     !
     ! GPL_EXCLUDE_START
     !
     ! 2] I am imposing a qp_impose that contains qp corrections
     !
     if (present(qp_impose)) call QP_apply_db(band_range,en,k,qp_impose,&
&                                             QP_ctl_from_DB,msg_fmt,DB_corrected)
     !
     ! GPL_EXCLUDE_END
     !
     ! 3] I am using an externally defined FIT parameters
     !
     if (present(QP_ctl_impose)) then
       QP_ctl_input_or_impose%E=QP_ctl_impose%E
       QP_ctl_input_or_impose%W=QP_ctl_impose%W
       QP_ctl_input_or_impose%Z=QP_ctl_impose%Z
     endif
     !
     ! Finally I apply the generalized stretch
     !
     call QP_apply_stretch(band_range,en,QP_ctl_from_DB,QP_ctl_input_or_impose,DB_corrected)
     !
     ! X/K -> Fermi Level Update
     !
     if (Eid<3) then
       !
       ! GPL_EXCLUDE_START
       !
       if (associated(en%E_RIM).and.associated(en%Eo)) then
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
             forall (ib=1:BZ_RIM_nbands) en%E_RIM(ib,ik_r,1)=en%E_RIM(ib,ik_r,1)+&
&                                        en%E(ib,ik_ibz,1)-en%Eo(ib,ik_ibz,1)
             RIM_k_done(ik_r)=1
           enddo
           !
         enddo
         !
         deallocate(RIM_k_done)
         !
       endif
       !
       ! GPL_EXCLUDE_END
       !
       call fermi_level(en,k,Tel,1)
       !
     endif
     !
     ! G -> Fermi Level Update... but using the levels cooresponding to
     ! the K-points of the X grid !
     !
     if (Eid==3) then
       Fermi_en%nb=en%nb
       Fermi_en%nk=nXkibz
       allocate(Fermi_en%E(Fermi_en%nb,Fermi_en%nk,n_sp_pol))
       Fermi_en%E(:,:,:)=en%E(:,:nXkibz,:)
       call fermi_level(Fermi_en,k,Tel,1)
       call extend_occupations(Fermi_en,en)
       nullify(Fermi_en%E)
     endif
     !
     if (.not.QP_ctl_applied) return
     !
     ! Reporting
     !
     call msg('r', '[QP] Fermi level variation [ev]:',en%Efermi(1)*HARTREE)
     call msg('r', '[QP] Last Filled/Metallic band :',(/en%nbf,en%nbm/))
     if (en%nbf==en%nbm) then
       call msg('r','[QP] Ind. Gap Correction  [ev]:',&
&                (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&                -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HARTREE)
     endif
     !
     end subroutine
     !
end module
