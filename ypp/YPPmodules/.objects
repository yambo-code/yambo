#if defined _YPP_NL
RT_obj = mod_YPP_real_time.o
#endif
#if defined _YPP_RT 
RT_obj   = mod_YPP_real_time.o
ELPH_obj = mod_YPP_ELPH.o
#endif
#if defined _YPP_ELPH 
ELPH_obj = mod_YPP_ELPH.o
#endif
objs = mod_YPP.o mod_YPP_symm.o mod_YPP_interfaces.o $(RT_obj) $(ELPH_obj) YPP_SET_defaults.o
