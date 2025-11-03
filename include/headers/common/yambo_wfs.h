/*
        Copyright (C) 2000-2020 the YAMBO team
              http://www.yambo-code.org

 Authors (see AUTHORS file for details): DS
 
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

#ifdef _GAMMA_ONLY
#  define WF_RSPACE   real
#else
#  define WF_RSPACE   complex
#endif


#ifdef _GAMMA_ONLY
#  define wfconjg(x)   x
#else
#  define wfconjg(x)   conjg(x)
#endif

#ifdef _GAMMA_ONLY
#  define wfaimag(x)   x
#else
#  define wfaimag(x)   aimag(x)
#endif

#ifdef _GAMMA_ONLY
#  define wfcmplx(x,y)  real(x,y)
#else
#  define wfcmplx(x,y)  cmplx(x,y)
#endif
