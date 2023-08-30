!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_input_file()
 !
 use pars,           ONLY:lchlen
 use drivers,        ONLY:infile_editing
 use it_m,           ONLY:infile
 use com,            ONLY:com_path,jobstr
 use IO_m,           ONLY:cp_file
 !
 implicit none
 !
 character(lchlen) :: infile_copy
 integer           :: io_err
 !
 character(lchlen), external :: filename
 !
 if (len_trim(com_path)==0) return
 if (infile_editing) return
 if (trim(com_path)==".") return
 !
 call IO_make_directories("COM")
 infile_copy=trim(com_path)//"/"//trim(filename(infile))
 if (trim(jobstr)/=''.and.index(infile,trim(jobstr))==0) then
   infile_copy=trim(com_path)//"/"//trim(filename(infile))//"_"//trim(jobstr)
 endif
 call cp_file(trim(infile),trim(infile_copy),io_err)
 infile=infile_copy 
 !
end subroutine
