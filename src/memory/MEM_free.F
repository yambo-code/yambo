!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine MEM_free(what,Sz)
 !
 use pars,         ONLY:schlen,DP,IPL,IP
 use units,        ONLY:Kilobyte
 use openmp,       ONLY:master_thread
 use memory,       ONLY:TOT_MEM_Kb,MEM_element_copy,N_MEM_elements,MEMs
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer(IPL), intent(in) :: Sz
 ! 
 ! Defined in the init
 !
 logical           :: just_started
 integer           :: i_SAVE,i_COMPONENT,i_SHELF
 character(schlen) :: SHELF,TRUNK
 !
 ! Work space
 !
 logical :: verbose=.false.
 real(DP):: LARGE_wk
 integer :: i_mem,i_el,MEM_now_Kb
 integer :: where
 !
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 if(Sz==-1 .and. verbose) then
   call warning("De-allocation attempt of "//what//" which was not allocated.")
   return
 endif
 !
 call MEM_manager_init(what,.false.,Sz,i_SHELF,i_COMPONENT,i_SAVE,SHELF,TRUNK,just_started)
 !
 if (.not. (i_SHELF/=0.and.i_COMPONENT/=0) ) return
 !
 LARGE_wk=real(Sz,DP)*real(MEMs(i_SHELF)%kind(i_COMPONENT),DP)/Kilobyte
 where=int(MEMs(i_SHELF)%where(i_COMPONENT),IP)
 MEM_now_Kb=int(LARGE_wk)
 TOT_MEM_Kb(where)=TOT_MEM_Kb(where)-MEM_now_Kb
 !
 if (MEM_now_Kb==0) MEM_now_Kb=1
 !
 MEMs(i_SHELF)%use = MEMs(i_SHELF)%use-MEM_now_Kb
 MEMs(i_SHELF)%size(i_COMPONENT) = MEMs(i_SHELF)%size(i_COMPONENT)-Sz
 !
 if ( MEMs(i_SHELF)%use <= 0 ) then
   !
   do i_mem=i_SHELF,N_MEM_elements
     call MEM_element_copy(MEMs(i_mem+1),MEMs(i_mem))
   enddo    
   !
   N_MEM_elements=N_MEM_elements-1
   !
 else if (MEMs(i_SHELF)%size(i_COMPONENT) <= 0) then
   !
   do i_el=i_COMPONENT,MEMs(i_SHELF)%N-1
     MEMs(i_SHELF)%name(i_el)=MEMs(i_SHELF)%name(i_el+1)
     MEMs(i_SHELF)%desc(i_el)=MEMs(i_SHELF)%desc(i_el+1)
     MEMs(i_SHELF)%kind(i_el)=MEMs(i_SHELF)%kind(i_el+1)
     MEMs(i_SHELF)%size(i_el)=MEMs(i_SHELF)%size(i_el+1)
     MEMs(i_SHELF)%where(i_el)=MEMs(i_SHELF)%where(i_el+1)
   enddo    
   !
   MEMs(i_SHELF)%N=MEMs(i_SHELF)%N-1
   !
 endif
 !
 if(.not.just_started) call MEM_manager_messages(what,.false.,MEM_now_Kb,where)
 !
#endif
 !
end subroutine MEM_free
