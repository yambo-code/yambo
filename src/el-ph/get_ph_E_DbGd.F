!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): AC
!
!> @brief Return phonon energy in the double-grid
!
! @param[in]       q                   momenutm sampling
! @param[in]       il                  branch index
! @param[in]       iq_fg               index in the fine grid
! @param[in]       iq_bz               index in the BZ of the course grid
!
! @param[out]      ph_E                phonon-energy
!
function get_ph_E_DbGd(il,iq_fg,iq_bz,q)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use ELPH,          ONLY:PH_freqs,FAN_deltaE_treshold,PH_freqs_sq
 !
#include<memory.h>
 !
 integer,       intent(in)  :: il,iq_bz,iq_fg
 type(bz_samp), intent(in)  :: q
 !
 ! work space
 !
 integer  :: iE_fg
 real(SP) :: get_ph_E_DbGd
 !
 if(iq_fg==q%FGbz%k_range(iq_bz,1)) then
   !      
   ! the first point correspond to the point in the course grid
   !
   get_ph_E_DbGd=sqrt(abs(PH_freqs_sq(iq_bz,il)))
   !
   !  if iq==1 
   !  I read ph-energies calculated with matdyn.x 
   !  that includes the LO-TO splitting and the acustic sum rule
   !
   if(iq_bz==1) get_ph_E_DbGd=PH_freqs%FG%E(il,1,1)
   !
 else
   !      
   ! Double-grid energies
   !
   iE_fg  =q%FGbz%E_map(iq_fg)                             ! Map w(q) from BZ to IBZ
   get_ph_E_DbGd =PH_freqs%FG%E(il,iE_fg,1)                ! w(q_db) phonon energy in the D-Grid
   !
 endif
 !
end function get_ph_E_DbGd

