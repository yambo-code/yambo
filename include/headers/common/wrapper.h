/*
  License-Identifier: GPL
 
  Copyright (C) 2019 The Yambo Team
 
  Authors (see AUTHORS file for details): DS
*/
/*
 C wrapper
*/ 
#if defined _C_US
 #define C_FUNC(name,NAME) name ## _
#else
 #define C_FUNC(name,NAME) name
#endif
/*
 F90 wrapper
*/
#if defined _FORTRAN_US
 #define F90_FUNC(name) name ## _
#else
 #define F90_FUNC(name) name
#endif
