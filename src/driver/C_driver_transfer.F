!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine C_driver_transfer()
 !
 use C_driver,       ONLY:code_version,code_revision,code_hash,&
&                         code_libraries,get_version,get_libraries,get_running_tool,&
&                         get_running_project,code_bin,code_tool,code_project
 !
 implicit none
 !
 code_tool=" "
 call get_running_tool(code_tool)
 code_project=" "
 call get_running_project(code_project)
 code_bin=trim(code_tool)
 if (len_trim(code_project)>0) code_bin=trim(code_tool)//"_"//trim(code_project)
 code_libraries=" "
 call get_libraries(code_libraries)
 code_hash=" "
 call get_version(code_version(1),code_version(2),code_version(3),code_revision,code_hash)
 !
end subroutine
