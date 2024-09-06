!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine Runge_Kutta_integrators(PRINT_me)
 ! 
 use pars,           ONLY:SP,lchlen
 use real_time,      ONLY:RK_integrators,RK_integrator,RK_integrator_alloc
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 logical :: PRINT_me
 !
 ! Workspace
 !
 integer             :: is=0,ir,ic
 character(lchlen)   :: row
 !
 if (PRINT_me) then
   !
   call msg("nr","Butcher`s table")
   !
   ! Butcher table 
   !===============
   do ir=1,RK_integrator%N
     write (row,'(f4.1,a)') RK_integrator%c(ir),'|'
     do ic=1,ir
       write (row,'(a,10f4.1)') trim(row)//" ",RK_integrator%a(ir,ic)
     enddo
     call msg("r",row)
   enddo
   row=" "
   do ir=1,RK_integrator%N+1
     write (row,'(2a)') trim(row),"-----"
   enddo
   call msg("r",row)
   row=" "
   do ic=1,RK_integrator%N
     write (row,'(a,10f4.1)') trim(row)//" ",RK_integrator%b(ic)
   enddo
   call msg("rn","     "//row)
   return
 endif
 !
 call RK_integrator_alloc(RK_integrators(1),1,"EULER","Runge-Kutta RK1")
 RK_integrators(1)%b=1._SP
 !
 call RK_integrator_alloc(RK_integrators(2),2,"EMP","Runge-Kutta RK22 or Explicit Mid-Point")
 RK_integrators(2)%b=(/0._SP,1._SP/)
 RK_integrators(2)%c=(/0._SP,1._SP/2._SP/)
 RK_integrators(2)%a(2,1)=1._SP/2._SP
 !
 call RK_integrator_alloc(RK_integrators(3),2,"RK2","Runge-Kutta RK21 or Second order Heun")
 RK_integrators(3)%b=1._SP/2._SP
 RK_integrators(3)%c=(/0._SP,1._SP/)
 RK_integrators(3)%a(2,1)=1._SP
 !
 call RK_integrator_alloc(RK_integrators(4),3,"RK3","Runge-Kutta RK32")
 RK_integrators(4)%b=(/1._SP/6._SP,2._SP/3._SP,1_SP/6._SP/)
 RK_integrators(4)%c=(/0._SP,1._SP/2._SP,1._SP/)
 RK_integrators(4)%a(2,1)=1._SP/2._SP
 RK_integrators(4)%a(3,:)=(/-1._SP,2._SP,0._SP/)
 !
 call RK_integrator_alloc(RK_integrators(5),3,"H3","Runge-Kutta RK31 or Third order Heun")
 RK_integrators(5)%b=(/1._SP/4._SP,0._SP,3_SP/4._SP/)
 RK_integrators(5)%c=(/0._SP,1._SP/3._SP,2._SP/3._SP/)
 RK_integrators(5)%a(2,1)=1._SP/3._SP
 RK_integrators(5)%a(3,:)=(/0._SP,2._SP/3._SP,0._SP/)
 !
 call RK_integrator_alloc(RK_integrators(6),4,"RK42","Runge-Kutta RK42")
 RK_integrators(6)%b=(/1._SP/6._SP,0._SP,2._SP/3._SP,1_SP/6._SP/)
 RK_integrators(6)%c=(/0._SP,1._SP/4._SP,1._SP/2._SP,1._SP/)
 RK_integrators(6)%a(2,1) =  1._SP/4._SP
 RK_integrators(6)%a(3,:2)=(/0._SP,1._SP/2._SP/)
 RK_integrators(6)%a(4,:3)=(/1._SP,-2._SP,2._SP/)
 !
 call RK_integrator_alloc(RK_integrators(7),4,"RK41","Runge-Kutta RK41")
 RK_integrators(7)%b=(/1._SP/6._SP,1._SP/3._SP,1._SP/3._SP,1_SP/6._SP/)
 RK_integrators(7)%c=(/0._SP,1._SP/2._SP,1._SP/2._SP,1._SP/)
 RK_integrators(7)%a(2,1) =  1._SP/2._SP
 RK_integrators(7)%a(3,:2)=(/0._SP,1._SP/2._SP/)
 RK_integrators(7)%a(4,:3)=(/0._SP,0._SP,1._SP/)
 !
end subroutine Runge_Kutta_integrators
