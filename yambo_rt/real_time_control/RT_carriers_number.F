!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_carriers_number(en,k,Ncarr)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use RT_control,     ONLY:NE_i_start_time
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use real_time,      ONLY:RTibz,NE_i_time,RT_levels,RT_bands
 !
 implicit none
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 real(SP),      intent(out):: Ncarr(3)
 !
 ! Workspace
 !
 integer     :: ib,ik,ik_RT
 real(SP)    :: VALUE_tmp(2)
 !
 ! Electrons and holes number
 !
 Ncarr = 0._SP
 !
 do ik=1,en%nk
   do ib=RT_bands(1),RT_bands(2)
     VALUE_tmp=0._SP
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       if ( RT_levels%E(ib,ik_RT,1)<=0._SP ) then
         if (NE_i_time==NE_i_start_time) VALUE_tmp(1)=VALUE_tmp(1)+RT_ho_occ%N(ib,ik_RT)*RTibz%weights(ik_RT)
         if (NE_i_time/=NE_i_start_time) VALUE_tmp(1)=VALUE_tmp(1)-RT_el_occ%dN(ib,ik_RT)*RTibz%weights(ik_RT)
       else
         if (NE_i_time==NE_i_start_time) VALUE_tmp(2)=VALUE_tmp(2)+RT_el_occ%N(ib,ik_RT)*RTibz%weights(ik_RT)
         if (NE_i_time/=NE_i_start_time) VALUE_tmp(2)=VALUE_tmp(2)+RT_el_occ%dN(ib,ik_RT)*RTibz%weights(ik_RT)
       endif
     enddo
     Ncarr(2:3)=Ncarr(2:3)+VALUE_tmp
   enddo
 enddo
 !
 Ncarr(1)=Ncarr(2)-Ncarr(3)
 !
end subroutine RT_carriers_number
