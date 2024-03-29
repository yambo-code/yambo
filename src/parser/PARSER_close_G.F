!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARSER_close_G(ng,what)
 !
 use pars,        ONLY:SP
 use units,       ONLY:HA2EV
 use R_lattice,   ONLY:n_g_shells
 implicit none
 integer :: ng
 integer,  external  :: G2G,E2G
 real(SP), external  :: G2E
 ! 
 ! Work Space
 !
 real(SP):: ng_r
 character(*) :: what
 !
 if (n_g_shells==0.and.      (trim(what)=="fRL".or.trim(what)=="tRL") ) return
 if (n_g_shells==0.and. .not.(trim(what)=="fRL".or.trim(what)=="tRL") ) then
   call error(' Initialization not yet done. Use RL units in input or run initialization')
 endif
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
end subroutine
!
integer function G2G(ng2close)
  use R_lattice,   ONLY:n_g_shells,ng_in_shell
  implicit none
  integer, intent(in) :: ng2close
  integer :: i1
  !
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
  use pars,        ONLY:SP
  use R_lattice,   ONLY:n_g_shells,E_of_shell,ng_in_shell
  implicit none
  real(SP), intent(in) :: Energy
  integer :: i1
  !
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
function G2E(ng2close)
  use pars,        ONLY:SP
  use R_lattice,   ONLY:n_g_shells,E_of_shell,ng_in_shell
  implicit none
  integer, intent(in) :: ng2close
  real(SP) :: G2E
  integer :: i1
  do i1=1,n_g_shells
    if (ng2close==ng_in_shell(i1)) G2E=E_of_shell(i1)
  enddo
end function
