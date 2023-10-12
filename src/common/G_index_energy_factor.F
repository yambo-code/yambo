!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
integer function G_index_energy_factor(ig_in,factor)
 !
 use pars,         ONLY:SP
 use vec_operate,  ONLY:iku_v_norm
 use R_lattice,    ONLY:g_vec,ng_in_shell,n_g_shells,E_of_shell
 !
 implicit none
 !
 integer,  intent(in) :: ig_in
 real(SP), intent(in) :: factor
 !
 integer   :: ig_out,i_shell
 real(SP)  :: E_of_shell_g_in,E_of_shell_g_out,delta1,delta2
 !
 E_of_shell_g_in =iku_v_norm(g_vec(ig_in,:))**2/2._SP
 E_of_shell_g_out=factor*E_of_shell_g_in
 !
 ig_out=0
 do i_shell=2,n_g_shells
   delta1=(E_of_shell(i_shell)-E_of_shell(i_shell-1))/2._SP
   delta2=(E_of_shell(i_shell+1)-E_of_shell(i_shell))/2._SP
   if( .not. ( (E_of_shell(i_shell)-delta1)<E_of_shell_g_out .and. E_of_shell_g_out<(E_of_shell(i_shell)+delta2) ) ) cycle
   ig_out=ng_in_shell(i_shell)
   exit
 enddo
 !
 if(ig_out==0) call error(" Requested g-index is above maximum g-vector")
 !
 G_index_energy_factor=ig_out
 !
 return
 !
end function G_index_energy_factor
