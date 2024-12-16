/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
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
