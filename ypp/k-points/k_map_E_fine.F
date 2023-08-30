!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine k_map_E_fine(fg,E,k,MAP)
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:levels,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 !
 implicit none
 !
 type(bz_samp), intent(inout)      :: fg ! In the IBZ
 type(bz_samp), intent(inout)      :: k
 type(levels),  intent(inout)      :: E
 integer    ,   intent(inout)      :: MAP(fg%nibz)
 !
 ! Work Space
 !
 integer              :: i_fg,ik_near,ik,i_k_fg
 real(SP)             :: E_fg_ordered(E%FG%nb,E%FG%nk,n_sp_pol)
 !
 k%FGbare%FG_mode ="BARE"
 k%FGbare%N_mapped=0
 !
 i_fg=0
 do ik=1,k%nibz
   k%FGbare%k_range(ik,1)=i_fg+1
   do i_k_fg=1,fg%nibz
     ik_near=iabs(MAP(i_k_fg))
     if (ik==k%sstar(ik_near,1)) then 
       k%FGbare%N_mapped(ik)=k%FGbare%N_mapped(ik)+1
       i_fg=i_fg+1
       E_fg_ordered(:,i_fg,:)=E%FG%E(:,i_k_fg,:)
       k%FGbare%pt(i_fg,:)   =fg%pt(i_k_fg,:)
       k%FGbare%E_map(i_fg)  =i_fg
       k%FGbare%k_map(i_fg)  =ik
     endif
   enddo
   k%FGbare%k_range(ik,2)=i_fg
 enddo
 !
 if (i_fg/=fg%nibz) call error("Not all Fine Grid points have been mapped in the Coarse Grid BZ")
 !
 do i_fg=1,fg%nibz
   ik=k%FGbare%k_map(i_fg)
   k%FGbare%weights(i_fg)=k%weights(ik)/real(k%FGbare%N_mapped(ik))
 enddo
 !
 E%FG%E=E_fg_ordered
 fg%pt =k%FGbare%pt
 !
end subroutine
