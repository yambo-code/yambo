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
subroutine OCCUPATIONS_Merge(E,E_minus,E_plus,K)
 !
 use pars,          ONLY:SP,DP
 use electrons,     ONLY:levels,default_nel
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp
 !
 implicit none
 !
 type(levels)  :: E,E_minus,E_plus
 type(bz_samp) :: K
 !
 ! Work Space
 !
 integer :: nval
 !
 nval=nint(default_nel/2._SP) !nel/2._SP ! number of valence bands 
 !
 E%nbm=E_plus%nbm   ! from negative charged distribution
 E%nbf=E_minus%nbf  ! from positive charged distribution
 !
 ! I do not change E_fermi so the top valence of the un-excited system
 ! is set to zero
 !
 if(.not.allocated(E%f)) return 
 !
 E%f(:nval,:,:)  =E_minus%f(:nval,:,:)
 E%f(nval+1:,:,:)=E_plus%f(nval+1:,:,:)
 !
 ! Check number of electorns
 !
 call msg("sr","Number of electrons in Quasi-Fermi: ",N_electrons(E,k))
 !
 contains 
   !  
   real(SP) function N_electrons(E_in,K_in)
     !  
     use electrons,     ONLY:n_sp_pol,spin_occ
     !
     implicit none
     !
     type(levels)  :: E_in
     type(bz_samp) :: K_in
     !
     ! Work Space
     !
     integer  :: ib,ik,is
     !
     N_electrons=0._SP
     !
     do ib=1,E_in%nb
       do ik=1,E_in%nk
         do is=1,n_sp_pol
           N_electrons=N_electrons+E_in%f(ib,ik,is)*K_in%weights(ik)
         enddo
       enddo
     enddo
     !
   end function N_electrons
 !
end subroutine
