/*
        Copyright (C) 2000-2019 the YAMBO team
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
 
#ifdef _CUDA
#  define DEV_SUBNAME(x)        x##_gpu
#  define DEV_SUBNAME_ALT(x)    x##_gpu
#  define DEV_VARNAME(x)        x##_d
#else
#  define DEV_SUBNAME(x)        x
#  define DEV_SUBNAME_ALT(x)    x##_cpu
#  define DEV_VARNAME(x)        x
#endif

