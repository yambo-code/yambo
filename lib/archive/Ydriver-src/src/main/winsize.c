/*
         Copyright (C) 2000-2022 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): AM
  
  This file is distributed under the terms of the GNU 
  General Public License. You can redistribute it and/or 
  modify it under the terms of the GNU General Public 
  License as published by the Free Software Foundation; 
  either version 2, or (at your option) any later version.
 
  This program is distributed in the hope that it will 
  be useful, but WITHOUT ANY WARRANTY; without even the 
  implied warranty of MERCHANTABILITY or FITNESS FOR A 
  PARTICULAR PURPOSE.  See the GNU General Public License 
  for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
  MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
