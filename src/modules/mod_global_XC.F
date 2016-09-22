!
!        Copyright (C) 2000-2016 the YAMBO team
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
module global_XC
 !
 use pars,      ONLY:lchlen,SP,RZERO
 !
 implicit none
 !
 ! general notice:
 !
 ! Yambo uses integers to store locally the xc specs. These integers
 ! are transformed in strings when setup_global_XC is called.
 ! setup_global_XC is called in
 !
 !  mod_QP_ctl: when using external QP corrections
 !  setup: when using SC energies/WF
 !
 ! The strings obtained are then written in the corresponding DB or veryfied
 ! against the value in the DB. Note that in the case of WFs only the loaded_WF_xc_string
 ! is set in SC_initial_WF_setup. All other strings are eventually read from the DB's
 !
 ! Energies (linked to the three energy types) ...
 !
 !  ... Integers ...
 !
 integer     , public :: G_kind,G_xc_functional,G_SE_more,G_perturbation
 integer     , public :: X_kind,X_xc_functional,X_SE_more,X_perturbation
 integer     , public :: K_kind,K_xc_functional,K_SE_more,K_perturbation
 !
 ! ... and Strings... 
 !
 character(lchlen) , public :: G_E_xc_string
 character(lchlen) , public :: X_E_xc_string(4)
 character(lchlen) , public :: K_E_xc_string
 !
 ! Wavefunctions ...
 !
 ! ... integers ...
 !
 integer     , public :: WF_kind,WF_xc_functional,WF_perturbation
 real(SP)    , public :: WF_exx_FRACTION, WF_exx_SCREENING
 !
 character(lchlen) , public :: loaded_WF_xc_string  
 !
 ! ... and Strings
 ! ... these are read from the DB (when writing, instead, all these string are set equal to  loaded_WF_xc_string)
 !
 character(lchlen) , public :: G_WF_xc_string
 character(lchlen) , public :: X_WF_xc_string(4)
 character(lchlen) , public :: K_WF_xc_string
 character(lchlen) , public :: Dipole_WF_xc_string ! Dipole strengths DB uses its own field (so no integer associated)
 !
 ! QP database
 !
 integer     , public :: QP_DB_kind
 !
 ! Self Energies KINDS
 !
 ! Only diagonal term in the hamiltonian basis-set
 integer, public, parameter :: &
   QP_SE_NONE          = 400, &
   QP_SE_COHSEX        = 401, &
   QP_SE_GoWo_PPA      = 402, &
   QP_SE_GoWo          = 403, &
   QP_SE_GWo_PPA       = 404, &
   QP_SE_GWo           = 405, &
   QP_SE_POLARON       = 406, &
   QP_SE_COH           = 409, &
   QP_SE_SEX           = 410 
 !
 ! More Self-Energies
 integer, public, parameter :: &
   QP_MORE_NONE          = 501, &
   QP_MORE_SCISSOR       = 502, &
   QP_MORE_STRETCH       = 503, &
   QP_MORE_SCIS_AND_STRE = 504, &
   QP_MORE_INTERPOLATION = 505
 !
 ! Perturbations
 integer, public, parameter :: &
   EXT_NONE             = 601, &
   EXT_B_LANDAU         = 602, &
   EXT_B_PAULI          = 603, &
   EXT_B_MAGNETISM      = 604
 !
 ! Scattering Self-Energies (to be renamed)
 integer, public, parameter :: &
   QP_SE_GW_NEQ        = 407, &
   QP_SE_GW_QED        = 408

   !
 contains
   !
   subroutine  setup_global_XC(what,kind,se_more,xc_functional,perturbation,exx_fraction,exx_screening)
     character(*)           :: what          
     integer                :: kind          
     integer,      optional :: se_more       
     integer,      optional :: xc_functional 
     integer,      optional :: perturbation 
     real(SP),     optional :: exx_fraction
     real(SP),     optional :: exx_screening
     !
     ! Work Space
     !
     integer :: local_se_more,local_xc_functional,local_perturbation
     real(SP):: local_exx_fraction, local_exx_screening
     !
     character(lchlen), external :: global_XC_string
     !
     local_se_more=QP_MORE_NONE
     local_xc_functional=0
     local_perturbation=EXT_NONE
     local_exx_fraction = RZERO
     if (present(se_more)) local_se_more=se_more
     if (present(xc_functional)) local_xc_functional=xc_functional
     if (present(perturbation)) local_perturbation=perturbation
     if (present(exx_fraction)) local_exx_fraction=exx_fraction
     if (present(exx_screening)) local_exx_screening=exx_screening
     !
     if (what=='E'.or.what=='preset'.or.what=='G') then
       G_kind=kind
       G_SE_more=local_se_more
       G_xc_functional=local_xc_functional
       G_perturbation=local_perturbation
       G_E_xc_string=global_XC_string(kind,local_se_more,local_xc_functional,local_perturbation)
     endif
     !
     if (what=='X'.or.what=='preset') then
       X_kind=kind
       X_SE_more=local_se_more
       X_xc_functional=local_xc_functional
       X_perturbation=local_perturbation
       X_E_xc_string=global_XC_string(kind,local_se_more,local_xc_functional,local_perturbation)
     endif
     !
     if (what=='K'.or.what=='preset') then
       K_kind=kind
       K_SE_more=local_se_more
       K_xc_functional=local_xc_functional
       K_perturbation=local_perturbation
       K_E_xc_string=global_XC_string(kind,local_se_more,local_xc_functional,local_perturbation)
     endif
     !
     if (what=='WF'.or.what=='preset') then
       WF_kind=kind
       WF_xc_functional=local_xc_functional
       WF_exx_FRACTION =local_exx_fraction
       WF_exx_SCREENING =local_exx_screening
       !
       ! loaded_WF_xc_string represents the xc kind of the WF that are loaded trough wf_load.
       ! G/X/K/Dipole_WF_xc_string, instead, may be overwritten by the value written in the databases.
       !
       loaded_WF_xc_string=global_XC_string(kind,QP_MORE_NONE,local_xc_functional,local_perturbation)
       !
       if (what=='preset') G_WF_xc_string     =loaded_WF_xc_string
       if (what=='preset') X_WF_xc_string     =loaded_WF_xc_string
       if (what=='preset') K_WF_xc_string     =loaded_WF_xc_string
       if (what=='preset') Dipole_WF_xc_string=loaded_WF_xc_string
       !
     endif
     !
     if (what=='preset') QP_DB_kind=QP_SE_none
     !
   end subroutine
   !
end module 
