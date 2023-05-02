#if defined _SC
MAGNETIC_objects = MAG_initial_check.o MAG_Hamiltonian.o MAG_common_build_A.o MAG_A_phase.o
#endif
#if defined _RT || defined _SC
PSEUDO_objects = Pseudo_Hamiltonian.o Pseudo_KB_G_to_R_space.o Pseudo_KB_FFT.o Pseudo_KB_gauge_factor.o
#endif
#if defined _NL
NL_objects = Build_Overlaps_det_NEQ.o Build_tilde_vbands.o Build_W_operator.o Berry_polarization_NEQ.o 
#endif
#if defined _SC
ELECTRIC_objects = Build_Overlaps_det_NEQ.o Build_tilde_vbands.o Build_W_operator.o Berry_polarization_NEQ.o 
#endif
#if defined _SC || defined _RT || defined _NL
objs = Bare_Hamiltonian.o V_Hartree.o XC_additional_SC_potentials.o XC_potentials.o V_qp_basis_to_H.o V_real_space_to_H.o Vgrad_real_space_to_H.o \
       Check_symmetries.o WF_and_dipole_dimensions.o \
       ${MAGNETIC_objects} ${PSEUDO_objects} ${NL_objects} ${ELECTRIC_objects}
#endif
