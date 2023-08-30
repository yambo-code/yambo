!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARSER_dump_the_additional_lines(ou)
 !
 use com,        ONLY:repfile_unit,inpfile_unit
 use it_m,       ONLY:infile,nADDITIONAL_flines,ADDITIONAL_flines
 use drivers,    ONLY:infile_editing
 use parallel_m, ONLY:master_cpu
 !
 implicit none
 !
 integer :: ou
 !
 ! Work Space
 !
 integer :: i1
 !
 if (infile_editing.and.ou==inpfile_unit) open(unit=ou,file=trim(infile),position='append')
 do i1=1,nADDITIONAL_flines
   if (master_cpu) then
      if (ou==inpfile_unit) write (ou,'(a)') trim(ADDITIONAL_flines(i1))
      if (ou==repfile_unit) write (ou,'(2a)') ' | ',trim(ADDITIONAL_flines(i1))
    endif
    if (ou/=repfile_unit.and.ou/=inpfile_unit) write (ou,'(2a)') '# | ',trim(ADDITIONAL_flines(i1))
 enddo 
 if (infile_editing) close(ou)
 !
end subroutine
