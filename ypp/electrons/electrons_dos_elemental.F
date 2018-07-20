! 
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine electrons_dos_elemental(k,E,bands,el_dos,USE_the_DbGd,WF_fac,QUIET,FORCE_occ)
 !
 use pars,           ONLY:SP,rZERO
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp 
 use YPP,            ONLY:DOS_broadening,DOS_E_range,DOS_E_step
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
 real(SP),        intent(inout) :: el_dos(:,:)
 logical,  optional, intent(in) :: USE_the_DbGd,QUIET,FORCE_occ
 real(SP), optional, intent(in) :: WF_fac(bands(1):bands(2),k%nibz,2)
 !
 ! Work Space
 ! 
 real(SP)              :: dos_E
 integer               :: i_E,steps(2)
 logical               :: LIVE_t_,USE_occ
 type(PP_indexes)      :: px
 !
 ! Use occupations?
 !==================
 !
 call parser('DOSUsesOcc',USE_occ)
 if (present(FORCE_occ)) then
   USE_occ=FORCE_occ
 endif
 !
 ! Energy Range
 !================================
 !
 steps=shape(el_dos)
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(E%E(bands(1):bands(2),:,:))-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(E%E(bands(1):bands(2),:,:))+5._SP*DOS_broadening
 endif
 DOS_E_step=(DOS_E_range(2)-DOS_E_range(1))/steps(1)
 !
 ! Parallel setup and live_timing
 !================================
 !
 LIVE_t_=.TRUE.
 if (present(QUIET)) then
   LIVE_t_=.not.QUIET
   if (QUIET) call IO_and_Messaging_switch("-log")
 endif
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/steps(1)/))
 !
 if (LIVE_t_) call live_timing('DOS',px%n_of_elements(myid+1))
 !
 el_dos=rZERO
 !
 do i_E=1,steps(1)
   !
   if (.not.px%element_1D(i_E)) cycle
   !
   dos_E=DOS_E_range(1)+i_E*DOS_E_step
   !
   if (present(WF_fac)) then
     if (USE_the_DbGd) then
       call el_density_of_states(k,E,dos_E,DOS_broadening,bands,el_dos(i_E,:),USE_the_DbGd=.TRUE.,&
&                                WF_fac=WF_fac,USE_occupations=USE_occ)
     else
       call el_density_of_states(k,E,dos_E,DOS_broadening,bands,el_dos(i_E,:),USE_the_DbGd=.FALSE.,&
&                                WF_fac=WF_fac,USE_occupations=USE_occ)
     endif
   else
     if (USE_the_DbGd) then
       call el_density_of_states(k,E,dos_E,DOS_broadening,bands,el_dos(i_E,:),USE_the_DbGd=.TRUE.,&
&                                USE_occupations=USE_occ)
     else
       call el_density_of_states(k,E,dos_E,DOS_broadening,bands,el_dos(i_E,:),USE_the_DbGd=.FALSE.,&
&                                USE_occupations=USE_occ)
     endif
   endif
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
