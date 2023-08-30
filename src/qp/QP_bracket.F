!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function DYSON_bracket(x1,f1,x2,f2)
 !
 use pars,ONLY: SP
 !
 implicit none
 real(SP)    :: f1,f2,x1,x2
 !
 ! Work Space
 !
 real(SP),parameter :: precision=1.E-5,factor=1.6
 !
 DYSON_bracket=0
 if (f1*f2<precision) return
 if (abs(f1)<abs(f2)) then
   x1=x1+factor*(x1-x2)
   DYSON_bracket=1
 else
   x2=x2+factor*(x2-x1)
   DYSON_bracket=2
 endif
 !
end function
