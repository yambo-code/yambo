!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_load_sc(defs)
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,G_unit,V_general,V_RL,&
&                        Time_unit,Bfield_unit,V_real_time,T_unit,V_nl_optics,V_io,V_sc
 use YPPm,          ONLY:V_value
 use SC,            ONLY:SC_neq_kind
 !
 implicit none
 !
 type(initdefs) :: defs
 !
 ! RunLevels...
 !==============
 !
 call INIT_load_ypp(defs)
 !
 call it('r',defs,'angular_m',    '[R] Canonical angular momentum')
 call it('r',defs,'position',     '[R] Average <r^n>, n given in the input')
 !
 ! Variables...
 !==============
 call it(defs,'SCneqKIND',  '[SC] Options are [contrained-occ/constrained-mu/matsubara]',SC_neq_kind,verb_level=V_sc)
 !
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization')
 ! 
 ! PLOT
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (c)ube/(g)nuplot/(x)crysden
 !
 call it('f',defs,'PhaseTrick','[MAG] Phase trick for a better diagonalization')
 call it(defs,'PotValue',  'REal/IMaginary part of the mean potential',V_value)
 !
end subroutine
