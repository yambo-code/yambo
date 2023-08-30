!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine Convolve(f,g,f_times_g,N,step,Eo)
 !
 ! f(x) = \sum_i f_i delta(x-i * step-Eo)
 ! g(x) = \sum_i g_i delta(x-i * step-Eo)
 !
 ! Eo=No*step
 !
 ! fxg(x) =  \sum_{k,j} f_(k-j-No) g_j delta(x-k * step-Eo)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: f(N),g(N),step,Eo
 complex(SP), intent(out) :: f_times_g(N)
 !
 ! Work Space
 !
 integer      :: K,J,L,Mo
 !
 Mo=nint(Eo/step)
 !
 f_times_G=(0.,0.)
 !
 do K=1,N
   do J=1,N
     L=K-J-Mo
     if (L<1.or.L>N) cycle
     f_times_g(K)=f_times_G(K)+f(L)*g(J)*step
   enddo
 enddo
 !
end subroutine
