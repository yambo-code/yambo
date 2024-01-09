!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): HM DS
!
subroutine io_BS_PAR_free(ID_head,ID,mode,l_kernel_complete)
 !
 ! close BSE netcdf parallel file
 !
 use pars,          ONLY:SP
 use parallel_int,  ONLY:PP_wait
 use BS,            ONLY:l_BSE_kernel_complete,BS_K_has_been_calculated_loaded,BS_K_cutoff
 use IO_m,          ONLY:read_is_on,write_is_on,io_BS_K
 use IO_int,        ONLY:io_disconnect
 !
 implicit none
 !
 integer,      intent(in)  :: ID,ID_head
 logical,      intent(in)  :: l_kernel_complete
 character(*), intent(in)  :: mode
 !
 integer              :: io_BS
 integer, external    :: variables_BS_more
 !
 call PP_wait()
 !
 if(trim(mode)=="compressed_head".or.trim(mode)=="full") then
   if(write_is_on(ID)) then
     BS_K_has_been_calculated_loaded=l_kernel_complete
     l_BSE_kernel_complete          =l_kernel_complete.and.(io_BS_K.or.BS_K_cutoff>0._SP)
   endif
   if(write_is_on(ID_head).or.read_is_on(ID_head)) then
     io_BS=variables_BS_more(ID_head,'end')
     call io_disconnect(ID_head)
   endif
   if(read_is_on(ID)) then
     BS_K_has_been_calculated_loaded=l_kernel_complete
     l_BSE_kernel_complete          =l_kernel_complete.and.(io_BS_K.or.BS_K_cutoff>0._SP)
   endif
 endif
 !
 if(.not.io_BS_K .and. trim(mode)=="full" )      return
 !
 if(trim(mode)=="compressed_mat".or.trim(mode)=="full") call io_disconnect(ID)
 !
end subroutine
