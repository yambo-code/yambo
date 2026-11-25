!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module C_driver
 !
 implicit none
 !
 integer      :: code_version(3)
 integer      :: code_GPL_revision
 integer      :: code_revision
 character(50):: code_bin
 character(50):: code_tool
 character(50):: code_project
 character(50):: code_hash
 character(50):: code_commit_short
 character(128):: code_branch
 character(128):: code_tag
 character(16):: code_state
 character(32):: code_provenance_source
 character(50):: code_libraries
 !
 interface  
   !
   subroutine get_version(version,subversion,patchlevel,revision,hash,hash_len,&
&                         commit_short,commit_short_len,ref,ref_len,tag,tag_len,&
&                         state,state_len,source,source_len)
     integer     , intent(out):: version
     integer     , intent(out):: subversion
     integer     , intent(out):: patchlevel
     integer     , intent(out):: revision
     character(*), intent(out):: hash
     integer,      intent(in) :: hash_len
     character(*), intent(out):: commit_short
     integer,      intent(in) :: commit_short_len
     character(*), intent(out):: ref
     integer,      intent(in) :: ref_len
     character(*), intent(out):: tag
     integer,      intent(in) :: tag_len
     character(*), intent(out):: state
     integer,      intent(in) :: state_len
     character(*), intent(out):: source
     integer,      intent(in) :: source_len
   end subroutine
   !
   subroutine get_libraries(libraries)
     character(*), intent(out):: libraries
   end subroutine
   !
   subroutine get_running_project(code_project)
     character(*), intent(out):: code_project
   end subroutine
   !
   subroutine get_running_tool(code_tool)
     character(*), intent(out):: code_tool
   end subroutine
   !
   subroutine get_runlevel(component,component_id,runlevel_id)
     character(*), intent(out):: component
     integer,      intent(in) :: component_id
     integer,      intent(in) :: runlevel_id
   end subroutine
   !
 end interface
 !
end module C_driver
