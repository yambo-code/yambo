!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine C_driver_transfer()
 !
 use C_driver,       ONLY:code_version,code_revision,code_hash,code_commit_short,&
&                         code_branch,code_tag,code_state,code_provenance_source,&
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
 code_commit_short=" "
 code_branch=" "
 code_tag=" "
 code_state=" "
 code_provenance_source=" "
 call get_version(code_version(1),code_version(2),code_version(3),code_revision,&
&                 code_hash,len(code_hash),code_commit_short,len(code_commit_short),&
&                 code_branch,len(code_branch),code_tag,len(code_tag),&
&                 code_state,len(code_state),code_provenance_source,len(code_provenance_source))
 !
end subroutine
