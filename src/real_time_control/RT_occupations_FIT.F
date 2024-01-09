! 
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_occupations_FIT(T_fit,Ef_fit,Occ_bk)
 !
 ! Check if both the electrons and the holes are below a given temperature
 ! In case locks the occupations of states which are not expected to change
 ! any more
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_carriers
 use functions,      ONLY:Fermi_fnc_fit
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 real(SP), intent(in)    :: Occ_bk(RT_carriers%nb(1):RT_carriers%nb(2),RT_carriers%nk)   
 real(SP), intent(inout) :: T_fit(2),Ef_fit(2)
 !
 real(SP) :: Occ_rt(RT_carriers%nstates)
 integer  :: nfit,i_RT,ib,ik_RT
 real(SP) :: err_fit(4)
 !
 call timing('RT occupations fit',OPR='start')
 !
 do i_RT=1,RT_carriers%nstates
   ib   =RT_carriers%table(i_RT,1)
   ik_RT=RT_carriers%table(i_RT,3)
   Occ_rt(i_RT)=Occ_bk(ib,ik_RT)
 enddo
 !
 nfit=Fermi_fnc_fit(Occ_rt,RT_carriers%nstates,T_fit,Ef_fit,err_fit)
 !
 if(err_fit(1)>0.05) then
   T_fit(1) =-T_fit(1)
   Ef_fit(1)=0._SP
 endif
 if(err_fit(2)>0.05) then
   T_fit(2) =-T_fit(2)
   Ef_fit(2)=0._SP
 endif
 !
 call timing('RT occupations fit',OPR='stop')
 !
 return
 !
end subroutine RT_occupations_FIT
