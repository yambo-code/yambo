!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine WF_free(WF,keep_fft,keep_states_to_load)
 !
 use wave_func,  ONLY:WAVEs
 use pseudo,     ONLY:pp_is_uspp,PP_uspp_free
 use cuda_m,     ONLY:have_cuda
 !
#include<memory.h>
 !
 type(WAVEs)       :: WF
 logical, optional :: keep_fft 
 logical, optional :: keep_states_to_load
 !
 ! Work-space
 !
 logical           :: keep_fft_
 logical           :: keep_states_to_load_
 !
 keep_fft_=.false.
 if (present(keep_fft)) keep_fft_=keep_fft
 !
 keep_states_to_load_=.false.
 if (present(keep_states_to_load)) keep_states_to_load_=keep_states_to_load
 !
 ! USPP
 if (pp_is_uspp) call PP_uspp_free()
 !
 if (have_cuda) then
   if (.not. ( allocated(WF%c) .eqv. allocated(WF%c_d) ) ) &
&    call error("[WF] inconsistent alloc of GPU wfcs")
 endif
 !
 if (.not.allocated(WF%c)) return
 !
 YAMBO_FREE(WF%c)
 YAMBO_FREE(WF%c_d)
 YAMBO_FREE(WF%state)
 YAMBO_FREE(WF%index)
 !
 if(.not.keep_fft_.and.WF%space=="R") call fft_free()
 !
 WF%k=0
 WF%b=0
 WF%sp_pol=0
 WF%space=' '
 !
 if (keep_states_to_load_) return
 !
 WF%to_load=.TRUE.
 !
end subroutine
