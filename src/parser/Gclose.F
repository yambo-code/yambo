!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine Gclose(ng,what)
 !
 use pars,        ONLY:SP
 use units,       ONLY:HA2EV
 use R_lattice,   ONLY:n_g_shells,E_of_shell,ng_in_shell
 implicit none
 integer :: ng
 ! 
 ! Work Space
 !
 real(SP):: ng_r
 character(*) :: what
 !
 if (n_g_shells==0) return
 !
 ng_r=real(ng)
 select case (trim(what))
   case ('tHa')
     ng=int(G2E(G2G(ng)))
   case ('tmHa')
     ng=int(1000.*G2E(G2G(ng)))
   case ('tRy')
     ng=int(2.*G2E(G2G(ng)))
   case ('tmRy')
     ng=int(2.*1000.*G2E(G2G(ng)))
   case ('teV')
     ng=int(HA2EV*G2E(G2G(ng)))
   case ('fHa')
     ng=G2G(E2G(ng_r))
   case ('fmHa')
     ng=E2G(real(ng)/1000._SP)
   case ('fRy')
     ng=E2G(real(ng)/2._SP)
   case ('fmRy')
     ng=E2G(real(ng)/2._SP/1000._SP)
   case ('feV')
     ng=E2G(real(ng)/HA2EV)
   case ('tRL','fRL')
     ng=G2G(ng)
 end select
 !
contains
  !
  integer function G2G(ng2close)
    integer :: ng2close,i1
    G2G=ng2close
    if (ng2close>ng_in_shell(n_g_shells)) G2G=ng_in_shell(n_g_shells)
    do i1=1,n_g_shells
      if (ng2close==ng_in_shell(i1)) return
      if (ng2close<ng_in_shell(i1)) then
        G2G=ng_in_shell(i1)
        exit 
      endif
    enddo
  end function
  !
  integer function E2G(Energy)
    integer :: i1
    real(SP):: Energy
    if (Energy<0.) E2G=ng_in_shell(n_g_shells)
    if (Energy>E_of_shell(n_g_shells)) E2G=ng_in_shell(n_g_shells)
    do i1=1,n_g_shells
      if (Energy<=E_of_shell(i1)) then
        E2G=ng_in_shell(i1)
        return
      endif
    enddo
  end function
  !
  real function G2E(ng2close)
    integer :: ng2close,i1
    do i1=1,n_g_shells
      if (ng2close==ng_in_shell(i1)) G2E=E_of_shell(i1)
    enddo
  end function
  !
end subroutine

