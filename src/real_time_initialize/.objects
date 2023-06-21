#if defined _RT && defined _ELPH
ELPH_objs = RT_ELPH_initialize.o 
#endif
#if defined _RT
RT_head_objs = RT_initialize.o RT_start_and_restart.o 
RT_foot_objs = RT_Field_Commensurable_Frequencies.o RT_Dephasing_Matrix.o RT_G_lesser_init.o RT_occupations_update.o
#endif
objs = $(RT_head_objs) RT_occupations_and_levels_init.o $(RT_foot_objs) $(ELPH_objs)
