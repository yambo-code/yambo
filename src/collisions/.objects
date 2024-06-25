#if defined _QED
QED_objs= SCATTERING_QED_transverse_matrix.o COLLISIONS_momentum.o
#endif
#if defined _SC
SC_objs = COLLISIONS_compose_sc.o
#endif
#if defined _RT
RT_objs = COLLISIONS_compose_rt.o
#endif
#if defined _NL
NL_objs = COLLISIONS_compose_nl.o COLLISIONS_compress.o
LSEX_objs = Build_LSEX_collisions.o LSEX_potential.o OSCLL_eval.o OSCLL_load.o \
            OSCLL_compose_collision.o OSCLL_compose_nl.o OSCLL_compose_vbands.o 
#endif
#if defined _QED || defined _SC || defined _RT || defined _NL 
objs= PLASMA_parallel_setup.o COLLISIONS_basic_operations.o \
      PLASMA_build_up.o PLASMA_tables_and_dimensions.o \
      COLLISIONS_alloc_and_free.o COLLISIONS_eval.o COLLISIONS_load.o \
      COLLISIONS_linearize_and_IO.o COLLISIONS_map_to_QP_table.o \
      COLLISIONS_NEQ_GW_static.o COLLISIONS_HXC.o SCATTERING_GW_kinematics.o \
      $(SC_objs) $(QED_objs) $(RT_objs) $(NL_objs) $(LSEX_objs)
#endif
