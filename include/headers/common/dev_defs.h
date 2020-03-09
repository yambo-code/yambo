/*
        Copyright (C) 2000-2020 the YAMBO team
              http://www.yambo-code.org

 Authors (see AUTHORS file for details): AF
 
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


#ifdef __STDC__
#  define CAT(a,b) a##b 
#else
#  define PASTE(a) a
#  define CAT(a,b) PASTE(a)b
#endif

#ifdef _CUDA
#  define DEV_SUBNAME(x)        CAT(x,_gpu)
#  define DEV_SUBNAME_ALT(x)    CAT(x,_gpu)
#  define DEV_VARNAME(x)        CAT(x,_d)
#  define DEV_ATTRIBUTE         , device
#  define DEV_PINNED            , pinned
#else
#  define DEV_SUBNAME(x)        x
#  define DEV_SUBNAME_ALT(x)    CAT(x,_cpu)
#  define DEV_VARNAME(x)        x
#  define DEV_ATTRIBUTE
#  define DEV_PINNED
#endif

