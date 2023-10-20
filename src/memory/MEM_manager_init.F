!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine MEM_manager_init(what,adding,Sz,i_SHELF,i_COMPONENT,i_SAVE,SHELF,TRUNK,just_started)
 !
 use pars,         ONLY:schlen,IPL
 use drivers,      ONLY:infile_editing
 use com,          ONLY:repfile,secnm
 use C_driver,     ONLY:code_bin
 use memory,       ONLY:MEM_treshold,MEM_shelf,MEM_jump_treshold,MEM_LIBs,MEMs,&
&                       MEM_SAVE_treshold
 !
 implicit none
 !
 character(*),     intent(in)  :: what
 logical,          intent(in)  :: adding
 integer(IPL),     intent(in)  :: Sz
 integer,          intent(out) :: i_SHELF,i_COMPONENT,i_SAVE
 character(schlen),intent(out) :: SHELF,TRUNK
 logical,          intent(out) :: just_started
 !
 ! Work Space
 !
 integer :: i_LIB
 integer, external :: MEM_LIB_finder,MEM_shelf_finder,MEM_save_finder,MEM_component_finder
 !
 ! Setup
 !=======
 MEM_SAVE_treshold=10 *MEM_treshold
 MEM_jump_treshold=100*MEM_treshold
 !
 i_SHELF      =0
 i_COMPONENT  =0
 i_SAVE       =0
 SHELF        =""
 TRUNK        =""
 just_started=.FALSE.
 !
#if defined _MEM_CHECK
 !
 just_started=infile_editing
 if (index(code_bin,"ypp")==0.and.len_trim(repfile)==0) just_started=.TRUE.
 !
 ! Search the element
 !====================
 !
 !...in the library (here I associate to what a SHELF)
 TRUNK=what
 if (index(what,"%")>0) TRUNK=what(index(what,"%"):len(what))
 i_LIB=MEM_LIB_finder(trim(TRUNK))
 !
 if (i_LIB>0) then
   SHELF=MEM_shelf(MEM_LIBs(i_LIB)%shelf)
 else if (index(what,"%")>0) then
   SHELF="["//trim(TRUNK)//"]"
 else
   SHELF=what
 endif
 !...here i search the SHELF & the COMPONENT in the already allocated objects (to add/remove an element)
 i_SHELF=MEM_shelf_finder(trim(SHELF))
 i_COMPONENT=0
 if (i_SHELF>0) then
   SHELF=MEMs(i_SHELF)%shelf
   i_COMPONENT=MEM_component_finder(i_SHELF,what,Sz,trim(secnm),adding)
 endif
 i_SAVE =MEM_save_finder(trim(SHELF))
 !
#endif
 !
end subroutine MEM_manager_init
