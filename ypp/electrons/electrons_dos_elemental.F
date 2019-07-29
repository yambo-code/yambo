! 
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine electrons_dos_elemental(k,E,bands,el_dos,dos_E,&
&                                  WF_fac,PDOS_fac,USE_the_DbGd,QUIET,FORCE_occ)
 !
 use pars,           ONLY:SP,rZERO
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp 
 use YPP,            ONLY:DOS_broadening,DOS_E_step,DOS_E_steps
 use interfaces,     ONLY:PARALLEL_index,el_density_of_states
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait,PP_indexes_reset
 use IO_m,           ONLY:IO_and_Messaging_switch
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(bz_samp),     intent(in)  :: k
 type(levels),      intent(in)  :: E
 integer,           intent(in)  :: bands(2)
 real(SP),        intent(inout) :: el_dos(:,:),dos_E(:)
 real(SP), pointer, intent(in)  :: WF_fac(:,:,:)
 real(SP), pointer, intent(in)  :: PDOS_fac(:,:,:)
 logical,  optional, intent(in) :: USE_the_DbGd,QUIET,FORCE_occ
 !
 ! Work Space
 ! 
 integer               :: i_E
 logical               :: LIVE_t_,USE_occ
 type(PP_indexes)      :: px
 !
 ! Use occupations?
 !==================
 !
 call parser('DOSUsesOcc',USE_occ)
 if (present(FORCE_occ)) USE_occ=FORCE_occ 
 !
 ! Parallel setup and live_timing
 !================================
 !
 LIVE_t_=.TRUE.
 if (present(QUIET)) LIVE_t_=.not.QUIET
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/DOS_E_steps/))
 !
 if (.not.LIVE_t_) call IO_and_Messaging_switch("-log")
 if (     LIVE_t_) call live_timing('DOS',px%n_of_elements(myid+1))
 !
 el_dos=rZERO
 !
 do i_E=1,DOS_E_steps
   !
   if (.not.px%element_1D(i_E)) cycle
   !
   call el_density_of_states(k,E,dos_E(i_E),DOS_broadening,bands,el_dos(i_E,:),  & ! standard
&                              USE_the_DbGd=USE_the_DbGd,USE_occupations=USE_occ,& ! optionals
&                              WF_fac=WF_fac,PDOS_fac=PDOS_fac)                    ! optional pointers
   !
   if (LIVE_t_) call live_timing(steps=1)
   !
 enddo
 !
 if (LIVE_t_) call live_timing( )
 !
 call IO_and_Messaging_switch("+log")
 !
 call PP_redux_wait(el_dos)
 !
end subroutine
