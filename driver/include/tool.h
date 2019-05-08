/*
         Copyright (C) 2000-2019 the YAMBO team
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

  tool & desc

*/
#if defined _TEST_MAIN
 #if defined _a2y_driver
 char *tool="a2y";
 #endif
 #if defined _ypp_driver
 char *tool="ypp";
 #endif
 #if defined _yambo_driver
 char *tool="yambo";
 #endif
 #if defined _c2y_driver
 char *tool="c2y";
 #endif
 #if defined _e2y_driver
 char *tool="e2y";
 #endif
 #if defined _p2y_driver
 char *tool="p2y";
 #endif
 char *tool_desc="Testing Driver";
#endif
#if defined _yambo 
 #define _YAMBO_MAIN
 char *tool="yambo";
 char *tool_desc="A shiny pot of fun and happiness [C.D.Hogan]";
#endif
#if defined _ypp
 #define _YPP_MAIN
 char *tool="ypp";
 char *tool_desc="Y(ambo) P(ost) P(rocessor)";
#endif
#if defined _a2y
 #define _A2Y_MAIN
 char *tool="a2y";
 char *tool_desc="A(binit) 2 Y(ambo) interface";
#endif
#if defined _c2y
 #define _C2Y_MAIN
 char *tool="c2y";
 char *tool_desc="C(pmd) 2 Y(ambo) interface";
#endif
#if defined _p2y
 #define _P2Y_MAIN
 char *tool="p2y";
 char *tool_desc="P(Wscf) 2 Y(ambo) interface";
#endif
#if defined _e2y
 #define _E2Y_MAIN
 char *tool="e2y";
 char *tool_desc="E(TSF) 2 Y(ambo) interface (0.6)";
#endif
