!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine YPP_NL_SET_defaults()
 !
 use YPP_real_time,  ONLY:Pump_path
 !
 call YPP_SET_defaults_rt()
 !
 Pump_path     ="none"
 !
end subroutine YPP_NL_SET_defaults