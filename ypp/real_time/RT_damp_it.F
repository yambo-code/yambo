!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine RT_damp_it(damp_type,damp_factor,ft,lda)
 !
 use YPP_real_time,  ONLY:RT_time,RT_conf
 use pars,           ONLY:SP,DP
 use fields,         ONLY:Efield
 !
 implicit none
 !
 integer,      intent(in)      :: lda
 real(SP),     intent(in)      :: damp_factor
 complex(DP),  intent(inout)   :: ft(RT_conf%T_n_steps,lda)
 character(*), intent(in)      :: damp_type
 !                          
 ! Work Space 
 !
 integer  :: i_t,i_Probe
 real(SP) :: damp_function(RT_conf%T_n_steps),T_ref
 !
 i_Probe=1
 T_ref=Efield(i_Probe)%t_initial
 !
 select case ( trim(damp_type) )
   case("LORENTZIAN")  
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-abs((RT_time(i_t)-T_ref)*damp_factor))
   case("GAUSSIAN") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-(RT_time(i_t)-T_ref)**2*damp_factor**2)
   case("NONE") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = 1._SP
   case default
     call error(" Unknown damping! ")
     return
 end select
 !
 forall(i_t=1:RT_conf%T_n_steps)  ft(i_t,:)=ft(i_t,:)*damp_function(i_t)
 !
end subroutine RT_damp_it

