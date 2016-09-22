!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
character(lchlen) function global_XC_string(kind,se_more,xc_functional,perturbation)
 !
 use xc_functionals,  ONLY:xc_string,XC_CORRELATION
 use global_XC
#if defined _SC
 use hamiltonian
#endif
 !
 integer           :: kind          ! <300 (LDA's GGA's ...) >300 See above
 integer           :: se_more       ! scissor's and more
 integer           :: xc_functional ! In case kind < 300 gives DFT functional
 integer           :: perturbation
 ! 
 integer           :: local_kind
 character(1)      :: ch_plus
 character(lchlen) :: ch
 global_XC_string=' '
 do local_kind=kind-1000,kind,1000
   select case (local_kind)
#if defined _SC
     case(-1)
       global_XC_string='IP'
     case(H_SE_Hartree) !=1000
       global_XC_string='Hartree'
     case(1:300)
       global_XC_string=trim(xc_string(xc_functional))
     case(H_SE_EXX)
       global_XC_string='SC Exact Exchange'
     case(H_SE_EXXC)
       global_XC_string='SC Exact Exchange +'//trim(xc_string(xc_functional))
     case(H_SE_FOCK)
       global_XC_string='SC Fock'
     case(H_SE_COH)
       global_XC_string='SC COH'
     case(H_SE_SEX)
       global_XC_string='SC SEX'
     case(H_SE_COHSEX)
       global_XC_string='SC COHSEX'
#endif
     case(QP_SE_COH)
       global_XC_string='COH'
     case(QP_SE_SEX)
       global_XC_string='SEX'
     case(QP_SE_COHSEX)
       global_XC_string='COHSEX'
     case(QP_SE_GoWo_PPA)
       global_XC_string='GoWo Plasmon Pole appox.'
     case(QP_SE_GoWo)
       global_XC_string='GoWo on real axis'
     case(QP_SE_GWo_PPA)
       global_XC_string='GWo Plasmon Pole appox.'
     case(QP_SE_GWo)
       global_XC_string='GWo on real axis'
     case(QP_SE_POLARON)
       global_XC_string='Polarons'
     case(QP_SE_GW_NEQ)
       global_XC_string='OUT-of-EQ GW'
     case(QP_SE_GW_QED)
       global_XC_string='QED-based GW'
#if defined _SC
     case default
       if(trim(global_XC_string)/="".and.local_kind>1000) global_XC_string='SC HARTREE +'//trim(global_XC_string)
#endif
   end select
 enddo
 !
 ch_plus='+'
 if (kind==0) ch_plus=' '
 !
 if (se_more/=QP_MORE_NONE) then
   ch=global_XC_string
   select case (se_more)
     case(QP_MORE_SCISSOR)
       global_XC_string=trim(ch)//trim(ch_plus)//'Scissor'
     case(QP_MORE_STRETCH)
       global_XC_string=trim(ch)//trim(ch_plus)//'Stretching'
     case(QP_MORE_SCIS_AND_STRE)
       global_XC_string=trim(ch)//trim(ch_plus)//'Scissor & Stretching'
     case(QP_MORE_INTERPOLATION)
       global_XC_string=trim(ch)//trim(ch_plus)//'Interpolation'
   end select
 endif
 !
 if (perturbation/=EXT_NONE) then
   ch=global_XC_string
   select case (perturbation)
     case(EXT_B_PAULI)
       global_XC_string=trim(ch)//trim(ch_plus)//'Pauli Magnetism'
     case(EXT_B_LANDAU)
       global_XC_string=trim(ch)//trim(ch_plus)//'Landau Magnetism'
     case(EXT_B_MAGNETISM)
       global_XC_string=trim(ch)//trim(ch_plus)//'Magnetism'
   end select
 endif
 !
end function
