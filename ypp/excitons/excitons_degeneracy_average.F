!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
subroutine excitons_degeneracy_average(N,BS_E_degs,R,C,VERB)
 !
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use BS_solvers,    ONLY:BSS_n_eig
 !
 implicit none
 !
 integer                 :: N,BS_E_degs(BSS_n_eig)
 real(SP)   , optional   :: R(N)
 complex(SP), optional   :: C(N)
 logical,     optional   :: VERB
 !
 ! Work space
 !
 integer     :: i_l,n_deg,i_deg
 real(SP)    :: R_save(N)
 complex(SP) :: C_save(N)
 !
 do i_l=1,N
   !
   if (BS_E_degs(i_l)/=i_l) cycle
   !
   n_deg=count(BS_E_degs==i_l)
   !
   if (present(VERB)) then
     if (n_deg>1.and.VERB) call msg('s',' State '//trim(intc(i_l))//' Merged with states '//&
&                                        trim(intc(BS_E_degs(i_l)))//' -> '//&
&                                        trim(intc(BS_E_degs(i_l)+n_deg-1)))
   endif
   !
   do i_deg=i_l+1,i_l+n_deg-1
     if (present(R)) then
       R(i_l)=R(i_l)+R(i_deg)
     endif
     if (present(C)) then
       C(i_l)=C(i_l)+C(i_deg)
     endif
   enddo
   if (present(R)) then
     R_save(:N)=R(:N)
   endif
   if (present(C)) then
     C_save(:N)=C(:N)
   endif
   !
   do i_deg=BS_E_degs(i_l),BS_E_degs(i_l)+n_deg-1
     if (present(R)) then
       R(i_deg)=R_save(i_l)/real(n_deg)
     endif
     if (present(C)) then
       C(i_deg)=C_save(i_l)/real(n_deg)
     endif
   enddo
   !
 enddo
 !
end subroutine excitons_degeneracy_average
