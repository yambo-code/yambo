!
!        Copyright (C) 2000-2022 the YAMBO team
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
#if defined _RT || defined _NL
!
subroutine Afield_activate(i1)
     use stderr,         ONLY:intc
     !
     implicit none
     !
     integer, intent(in)     ::i1
     !
     character(6)::field
     field='Field'//trim(intc(i1))
     call initactivate(1,field//'_Freq')
     call initactivate(1,field//'_NFreqs')
     call initactivate(1,field//'_DFreq')
     call initactivate(1,field//'_Int')
     call initactivate(1,field//'_Width')
     call initactivate(1,field//'_Chirp')
     call initactivate(1,field//'_FWHM')
     call initactivate(1,field//'_kind')
     call initactivate(1,field//'_pol')
     call initactivate(1,field//'_Dir')
     call initactivate(1,field//'_Dir_circ')
     call initactivate(1,field//'_Tstart')
end subroutine
!
!
subroutine Afield(defs,i_field)
   !
   use it_tools,      ONLY:it
   use pars,          ONLY:SP
   use drivers,       ONLY:infile_editing
   use C_driver,      ONLY:code_bin
   use fields,        ONLY:Efield,n_ext_fields
   use units,         ONLY:AU2KWCMm2
   use parser_m,      ONLY:parser
   use stderr,        ONLY:intc
   use it_m,          ONLY:E_unit,I_unit,Time_unit,V_real_time, &
   &                       initmode,initdefs
   !
   implicit none
   !
   integer, intent(in):: i_field
   type(initdefs)     :: defs
   !
   ! Workspace
   !
   character(10)      :: name
   logical            :: is_def
   !
   name='Field'//trim(intc(i_field))
   !
   call it(defs,trim(name)//'_Freq',            '[RT '//trim(name)//'] Frequency',&
&          Efield(i_field)%frequency,unit=E_unit)
   call it(defs,trim(name)//'_NFreqs',          '[RT '//trim(name)//'] Frequency',&
&          Efield(i_field)%n_frequencies)
   call it(defs,trim(name)//'_DFreq',           '[RT '//trim(name)//'] Frequency step',&
&          Efield(i_field)%W_step,unit=E_unit,verb_level=V_real_time)
   call it(defs,trim(name)//'_Int',             '[RT '//trim(name)//'] Intensity',&
&          Efield(i_field)%intensity,unit=I_unit)
   call it(defs,trim(name)//'_Width',           '[RT '//trim(name)//'] Width',&
&          Efield(i_field)%width,unit=Time_unit(1))
   call it(defs,trim(name)//'_Chirp',           '[RT '//trim(name)//'] Width',&
&          Efield(i_field)%chirp,unit=Time_unit(1),verb_level=V_real_time)
   call it(defs,trim(name)//'_FWHM',            '[RT '//trim(name)//'] Full Width at Half Maximum (overwrite width if set)',&
&          Efield(i_field)%FWHM,unit=Time_unit(1),verb_level=V_real_time)
   call it(defs,trim(name)//'_kind',            '[RT '//trim(name)//'] Kind(SIN|COS|RES|ANTIRES|GAUSS|DELTA|QSSIN)',&
&          Efield(i_field)%ef_name)
   call it(defs,trim(name)//'_pol',             '[RT '//trim(name)//'] Pol(linear|circular)',&
&          Efield(i_field)%ef_pol)
   call it(defs,trim(name)//'_Dir',             '[RT '//trim(name)//'] Versor',&
&          Efield(i_field)%versor)
   call it(defs,trim(name)//'_Dir_circ',        '[RT '//trim(name)//'] Versor_circ',&
&          Efield(i_field)%versor_circ,verb_level=V_real_time)
   call it(defs,trim(name)//'_Tstart',          '[RT '//trim(name)//'] Initial Time',&
&          Efield(i_field)%t_initial,unit=Time_unit(1))
   !
   if (initmode==4.and..not.infile_editing) then
     !
     call parser(trim(name)//'_FWHM',is_def)
     if (     is_def) Efield(i_field)%width=Efield(i_field)%FWHM/(2._SP*sqrt(2._SP*log(2._SP)))
     if (.not.is_def) Efield(i_field)%FWHM=Efield(i_field)%width*(2._SP*sqrt(2._SP*log(2._SP)))
     !
     if (trim(Efield(i_field)%ef_name)/='none'.and.index(code_bin,'ypp')==0) n_ext_fields=n_ext_fields+1
     !
   endif
   !
end subroutine
!
#endif
