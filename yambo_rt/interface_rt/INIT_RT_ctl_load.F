!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine INIT_RT_ctl_load(defs,i_type)
 !
 ! Note that this routine is called in 'load' mode BEFORE
 ! the DB1 is read, so before n_sp_pol is set to the correct value.
 ! 
 ! Thus we must call it on the no spin, spin + and spin - cases 
 ! because the code does not know if there is
 ! spin unless the DB1 is read. 
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,V_real_time
 use RT_control,    ONLY:RT_ctl_db,RT_ctl_interp_neigh,N_RT_ctl_controls
 !
 implicit none
 integer, intent(in) :: i_type
 type(initdefs)      :: defs
 ! 
 ! Work Space
 ! 
 character(1)      ::prefix(N_RT_ctl_controls)
 character(7)      ::description_ch(N_RT_ctl_controls)
 !
 prefix=(/'X','K','G'/)
 description_ch(1)='Xd'
 description_ch(2)='BSK BSS'
 description_ch(3)='G'
 !
 call it(defs,prefix(i_type)//'fnRTdb',&
&        '[EXTRT '//trim(description_ch(i_type))//'] Database',&
&        RT_ctl_db(i_type),verb_level=V_real_time)
 !
 call it(defs,prefix(i_type)//'fnRT_N',&
&        '[EXTRT '//trim(description_ch(i_type))//'] Interpolation neighbours',&
&        RT_ctl_interp_neigh(i_type),verb_level=V_real_time)
 ! 
end subroutine
