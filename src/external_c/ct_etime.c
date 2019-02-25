/*
   Copyright (C) 1998-2004 ABINIT group (ZL,DCA,XG,GMR,MB)
   This file is distributed under the terms of the
   GNU General Public License, see ~ABINIT/Infos/copyright
   or http://www.gnu.org/copyleft/gpl.txt .
*/

#include "c_defs.h"

double F90_FUNC(etime,ETIME)(tt)
#if defined _DOUBLE
double tt[2];
#else
float tt[2];
#endif
{
 int who;
 struct rusage used;
 who = 0;
 getrusage(who,&used);
 tt[0] = used.ru_utime.tv_sec+((used.ru_utime.tv_usec)/1000000.);
 tt[1] = used.ru_stime.tv_sec+((used.ru_stime.tv_usec)/1000000.);
 return(tt[0]+tt[1]);
}
