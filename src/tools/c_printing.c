/*
  License-Identifier: GPL
 
  Copyright (C) 2006 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <string.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <wrapper.h>
#include <unistd.h>

int guess_winsize()
{
 int width;
 struct winsize ws;
 if (!isatty(2)) {width=-1;return width;}
 if( ioctl(STDERR_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0 )
  width = 79;
 else
  width = ws.ws_col - 1;
 return width;
};
int C_FUNC(win_size, WIN_SIZE)(int *win_width)
{
 *win_width = 0;
 *win_width = guess_winsize();
 return 0;
};
int C_FUNC(c_fprintf, C_FPRINTF)(char *lfmt, char *msg,char *rfmt, char *sfmt)
{
 if (strcmp(lfmt,"r")==0) fprintf(stderr,"\r");
 if (strcmp(lfmt,"n")==0) fprintf(stderr,"\n");
 if (strcmp(lfmt,"nn")==0) fprintf(stderr,"\n\n");
 fprintf(stderr,sfmt,msg);
 if (strcmp(rfmt,"n")==0) fprintf(stderr,"\n");
 if (strcmp(rfmt,"nn")==0) fprintf(stderr,"\n\n");
 fflush(stderr);
 return 0;
};
