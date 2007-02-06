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
 module QP_CTL
!
   use pars,  ONLY:SP,schlen,IP,HARTREE
!
! QP Pars
!
!?fnQP_db= "EWZ<db-pp.qp"     # [?] QP database.
!% ?fnQP_E
!  0 |  0 | 0 |               # [?] QP parameters (E).
!% 
!% ?fnQP_W
!  0 |  0 | 0 | 0 |           # [?] QP parameters (W).
!%
!?fbQP_Z= (  0.00000 ,  0.00000 )  eV  # [?] QP parameters (Z).
!  
! 1(X) 2(K) 3(G) 
!
   type qp_fit_ctl
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

 contains

 subroutine reset_qp_fit(qp_fit)
!==========================================================================
 implicit none
 type(qp_fit_ctl)::qp_fit
 qp_fit%db="none"
 qp_fit%E=(/0.,1.,0.,1./)
 qp_fit%W=0.
 qp_fit%Z=(1.,0.)
 qp_fit%E_err=0.
 qp_fit%W_err=0.
 qp_fit%db_scissor=0.
 qp_fit%fit_scissor=0.
 end subroutine

 subroutine QP_apply(band_range,en,k,Eid,msg_fmt,qp_impose,qpfit_impose)
!=======================================================================
!
! This routine manages the external qp corrections.
! The task is tricky, as SELF has three different energy
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
&                        QP_ctl_applied,QP_ctl_interp_neigh,QP_ctl_db
 use D_lattice,     ONLY:Tel
 use com,           ONLY:msg,depth
 use electrons,     ONLY:levels,E_reset,n_spin
 use R_lattice,     ONLY:bz_samp,nXkibz
 use parser_m,      ONLY:parser
 implicit none
 integer       ::Eid,band_range(2)
 type(levels)  ::en
 type(bz_samp) ::k
 character(*)  ::msg_fmt
 type(QP_t),       optional::qp_impose
 type(qp_fit_ctl), optional::qpfit_impose
!ws
 type(levels)     ::Fermi_en
 type(QP_t)       ::qp
 type(qp_fit_ctl) ::qpfit_rest,qpfit_free
 integer          ::DB_corrected(en%nb,en%nk)
 character(schlen)::ch
 logical          ::is_def(2)
!
 call QP_reset(qp)
 call reset_qp_fit(qpfit_rest)
 call reset_qp_fit(qpfit_free)
 call E_Reset(Fermi_en)
!
! If I'm not imposing a fit (qpfit_impose) of QP (qp_impose)
! return if the field are not present in the input file
!
 if (.not.present(qpfit_impose).and..not.present(qp_impose)) then
   select case(Eid)
    case(1)
     call parser('XfnQP_E',is_def(1))
     call parser('XfnQPdb',is_def(2))
    case(2)
     call parser('KfnQP_E',is_def(1))
     call parser('KfnQPdb',is_def(2))
    case(3)
     call parser('GfnQP_E',is_def(1))
     call parser('GfnQPdb',is_def(2))
   end select
   if (.not.any((/is_def/))) return
 endif
!
 if (.not.associated(en%qp_done)) then
   allocate(en%qp_done(en%nb,en%nk))
   call mem_est("E-qp_done",(/size(en%qp_done)/),(/IP/))
   en%qp_done=0
 endif 
 DB_corrected=0
 QP_ctl_applied=.false.
 if (Eid==1) then
   qpfit_rest%short_descr='[QP@X]'
   qpfit_free%short_descr='[QP@X]'
   ch='External QP corrections (X)'
 endif
 if (Eid==2) then
   qpfit_rest%short_descr='[QP@K]'
   qpfit_free%short_descr='[QP@K]'
   ch='External QP corrections (K)'
 endif
 if (Eid==3) then
   qpfit_rest%short_descr='[QP@G]'
   qpfit_free%short_descr='[QP@G]'
   ch='External QP corrections (G)'
   if (present(qp_impose)) ch='Internal QP corrections (G)'
 endif
!
! Section report
!
 if (depth>0) then
   call section('=',trim(ch))
 else 
   call section('*',trim(ch))
 endif
!
! Interpolation  neighbours is always input file controlled
!
 qpfit_free%interp_neigh=QP_ctl_interp_neigh(Eid)
 qpfit_rest%interp_neigh=QP_ctl_interp_neigh(Eid)
!
! Now I have different options ... 
!==================================
!
! 1] I am using the input file QP parameters
!
 if (.not.present(qp_impose).and..not.present(qpfit_impose)) then
   qpfit_rest%db=QP_ctl_db(Eid)
   call QP_apply_db(band_range,en,k,qp,qpfit_rest,msg_fmt,DB_corrected)
   if (trim(qpfit_rest%db)=="none") QP_ctl_db(Eid)=trim(qpfit_rest%db)
!
! Now I have to include the input file QP parameters in qpfit_free
! being careful that the scissor must go in qpfit%E(1)
!
   qpfit_free%E=(/QP_ctl_E(Eid,1)/HARTREE,QP_ctl_E(Eid,2),0._SP,QP_ctl_E(Eid,3)/)
   qpfit_free%W(3:)=QP_ctl_W(Eid,:) 
   qpfit_free%Z=QP_ctl_Z(Eid)
!
 endif
!
! 2] I am imposing an qp_impose that contains qp corrections
!
 if (present(qp_impose)) call &
&   QP_apply_db(band_range,en,k,qp_impose,qpfit_rest,msg_fmt,DB_corrected)
!
! 3] I am using an externally defined FIT parameters
!
 if (present(qpfit_impose)) then
   qpfit_free%E=qpfit_impose%E
   qpfit_free%W=qpfit_impose%W
   qpfit_free%Z=qpfit_impose%Z
 endif
!
! Finally I apply the generalized stretch
!
 call QP_apply_stretch(band_range,en,qpfit_rest,qpfit_free,DB_corrected)

 if (.not.QP_ctl_applied) return
!
! X/K -> Fermi Level Update
!==========================
 if (Eid<3) call fermi_level(en,k,Tel,1)
!
! G -> Fermi Level Update... but using the levels cooresponding to
! the K-points of the X grid !
!=================================================================
 if (Eid==3) then
   Fermi_en%nb=en%nb
   Fermi_en%nk=nXkibz
   allocate(Fermi_en%E(Fermi_en%nb,Fermi_en%nk,n_spin))
   Fermi_en%E(:,:,:)=en%E(:,:nXkibz,:)
   call fermi_level(Fermi_en,k,Tel,1)
   call extend_occupations(Fermi_en,en)
   nullify(Fermi_en%E)
 endif
!
 call msg('r', '[QP] Fermi level variation [ev]:',en%Efermi(1)*HARTREE)
 call msg('r', '[QP] Last Filled/Metallic band :',(/en%nbf,en%nbm/))
 if (en%nbf==en%nbm) then
   call msg('r','[QP] Gap Correction        [ev]:',&
&            (minval(en%E(en%nbf+1,:,1))-maxval(en%E(en%nbf,:,1)) &
&            -minval(en%Eo(en%nbf+1,:,1))+maxval(en%Eo(en%nbf,:,1)) )*HARTREE)
 endif
!
 end subroutine

 end module
