!
!        Copyright (C) 2000-2017 Claudio Attaccalite
!
! Authors (see AUTHORS file for details): CA
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
subroutine OCCUPATIONS_Quasi_Fermi(E,E_fermi_h,E_fermi_e,K,mode)
 !
 ! Mode = 0 
 !  Update the Fermi energy
 !  Output : E%E_fermi
 !
 ! Mode = 1 -> Mode 0 +
 !  Define E%f and reports variations in Ef/nbf/nbm
 !  Output : E%nbf,E%nbm
 !  E%E are shifted 
 !
 ! Mode > 1 -> Mode 1 +
 !  Full report of system electronic character
 !
 use pars,          ONLY:SP,DP
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,nel_cond,nel,E_duplicate,E_reset,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use IO_int,        ONLY:IO_and_Messaging_switch
 use com,           ONLY:msg
 use parser_m,      ONLY:parser
 !
 implicit none
 !
 type(levels) ::E
 type(bz_samp)::K
 integer      ::mode
 real(SP), intent(out) :: E_fermi_h,E_fermi_e
 !
 ! Work Space
 !
 type(levels) :: E_plus,E_minus
 real(SP) :: nel_save
 integer  :: nbf_save,nbm_save
 integer  :: nval 
 !
 call  OCCUPATIONS_Fermi(E,K,mode)
 !
 if(nel_cond==0._SP) return
 !
 nel_save=nel
 !
 call E_reset(E_plus)
 call E_reset(E_minus)
 call E_duplicate(E,E_plus)
 call E_duplicate(E,E_minus)
 !
 if(any(E%nbf/=E%nbm)) call error("Quasi-Fermi distribution only for insulators/semiconductors")
 !
 if(n_sp_pol/=1) call error("Quasi-Fermi distribution not implemented for spin-polarized systems")
 !
 nval=E%nbf(1)
 nel =nel+nel_cond
 !
 call IO_and_Messaging_switch("-report -log -io_in -io_out")
 call OCCUPATIONS_Fermi(E_plus,K,mode)
 !
 nbm_save=E_plus%nbm(1)
 !
 nel =nel_save
 nel =nel-nel_cond
 !
 call OCCUPATIONS_Fermi(E_minus,K,mode)
 call IO_and_Messaging_switch("+report +log +io_in +io_out")
 !
 nbf_save=E_minus%nbf(1)
 !
 call REPORT_Occupations(E_minus,'E_minus')
 call REPORT_Occupations(E_plus,'E_plus')
 !
 call OCCUPATIONS_Merge(E,E_minus,E_plus,K,mode) ! Generate a Quasi-Fermi distribution in E using E_minus and E_plus
 !
 nel  =nel_save
 E%nbf=nbf_save
 E%nbm=nbm_save
 !
 E_fermi_h=E_minus%E_Fermi
 E_fermi_e=E_plus%E_Fermi
 !
 call msg('rsn','Number of metalic bands : ',E%nbm(1))
 call msg('rsn','Number of filled  bands : ',E%nbf(1))
 call msg('rsn','Number of valence bands : ',nval)
 !
 call REPORT_Occupations(E,'E')
 !
 call E_reset(E_plus)
 call E_reset(E_minus)
 !
end subroutine
