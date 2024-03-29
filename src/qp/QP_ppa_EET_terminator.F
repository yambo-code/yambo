!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AF
!
function QP_ppa_EET_terminator(W,E,vs,Ep,ig1,ig2,iq,cutoff0,cutoff1)
 !
 ! Terminator for the PPA self-energy using the zero order
 ! effective-energy technique(EET), 
 !
 ! Kind="BRS" => Berger, Reining, Sottile PRB 85, 085126 (2012)
 ! Kind="BG" =>  Bruneval-Gonze, PRB 78, 085125 (2008)
 ! 
 ! cutoff0:   minval(E%E(E%nbf+1,:,is))   
 !            (more or less the CBm)
 !            used in the 2012 BRS correction
 !
 ! cutoff1:   minval(E%E(QP_n_G_bands(2),:,is))
 !            (the minimum over kpt of the highest band)
 !            used in the BG 2008 correction
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:q_norm,q_pt,g_vec
 use electrons,     ONLY:levels,spin_occ
 use vec_operate,   ONLY:iku_v_norm
 use drivers,       ONLY:Finite_Tel
 use QP_m,          ONLY:GW_terminator_E,GW_terminator_Kind
 !
 implicit none
 !
 integer,       intent(in) :: iq,ig1,ig2
 real(SP),      intent(in) :: Ep,cutoff0,cutoff1
 complex(SP),   intent(in) :: W
 type(levels),  intent(in) :: E
 integer,       intent(in) :: vs(4)
 !
 complex(SP) :: QP_ppa_EET_terminator
 !
 ! Work space
 !
 real(SP) :: eet_energy,q_plus_G,q_plus_Gp
 complex(SP) :: lW
 !
 if (trim(GW_terminator_Kind)=="BRS") then
   !
   q_plus_G =iku_v_norm(q_pt(iq,:)+g_vec(ig1,:))
   q_plus_Gp=iku_v_norm(q_pt(iq,:)+g_vec(ig2,:))
   !
   if(iq==1.and.ig1==1) q_plus_G =q_norm(1)
   if(iq==1.and.ig2==1) q_plus_Gp=q_norm(1)
   !
   eet_energy=E%E(vs(1),vs(2),vs(4))+1./2.*(q_plus_G**2/2. + q_plus_Gp**2/2.)
   !
   ! If the pole is in the wrong range use the BG semi-empirical value of PRB 78, 085125 (2008)
   !
   if (eet_energy<cutoff0) eet_energy=cutoff1+1._SP
   !
 else if (trim(GW_terminator_Kind)=="BG") then
   !
   ! Use Bruneval-Gonze terminator PRB 78, 085125 (2008)
   !
   eet_energy=cutoff1+GW_terminator_E
   !
 endif
 !
 lW=W
 if (Finite_Tel) lW=conjg(W)
 !
 QP_ppa_EET_terminator=real(spin_occ)/(lW-eet_energy-Ep) 
 !
end function
