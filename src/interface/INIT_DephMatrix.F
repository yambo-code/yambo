!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_DephMatrix()
 !
 ! Dephasing Matrix -> input file
 !
 use pars,           ONLY:SP
 use parser_m,       ONLY:PARSER_array
 use real_time,      ONLY:RT_INPUT_Deph_limits
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer :: n_fields
 ! 
 call PARSER_array(mode="REAL dimensions",VAR="DephMatrix",N=n_fields,M=4)
 !
 if (n_fields>0) then
   YAMBO_ALLOC(RT_INPUT_Deph_limits,(n_fields,4))
   call PARSER_array(mode="read",VAR="DephMatrix",N=n_fields,M=4,R_v=RT_INPUT_Deph_limits)
   call PARSER_array(mode="write",VAR="DephMatrix",&
&                    DESC='[RT] Dephasing Matrix (bnd1 | bnd2 | kpt | dephasing(fs) |)',&
&                    N=n_fields,M=4,R_v=RT_INPUT_Deph_limits,Kn=(/"i","i","i","r"/))
 else
   call PARSER_array(mode="write",VAR="DephMatrix",&
&                    DESC='[RT] Dephasing Matrix (bnd1 | bnd2 | kpt | dephasing(fs) |)',&
&                    N=1,M=4,R_v=reshape((/1._SP,1._SP,1._SP,-1._SP/),(/1,4/)),Kn=(/"i","i","i","r"/))
 endif
 !
end subroutine
