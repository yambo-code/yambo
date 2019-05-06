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
*/

/*
 PJ's
*/
#if defined _yambo || _ypp || _a2y || _c2y || _p2y || _e2y 
 char *pj="none";
#endif
#if defined _YPP_ELPH || _ELPH
 char *pj="ph";
#endif
#if defined _YPP_RT || _RT
 char *pj="rt";
#endif
#if defined _YPP_SC || _SC
 char *pj="sc";
#endif
#if defined _YPP_NL || _NL
 char *pj="nl";
#endif
#if defined _MAGNETIC
 char *pj="magnetic";
#endif
#if defined _ELECTRIC
 char *pj="electric";
#endif
#if defined _KERR
 char *pj="kerr";
#endif
#if defined _PL
 char *pj="pl";
#endif
#if defined _QED
 char *pj="qed";
#endif
#if defined _SCALAPACK
 char *pj="slk";
#endif
/*
 tool & desc
*/
#if defined _TEST_MAIN
 char *tool="driver";
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
 char *tool="a2y";
 char *tool_desc="A(binit) 2 Y(ambo) interface";
#endif
#if defined _c2y
 char *tool="c2y";
 char *tool_desc="C(pmd) 2 Y(ambo) interface";
#endif
#if defined _p2y
 char *tool="p2y";
 char *tool_desc="P(Wscf) 2 Y(ambo) interface";
#endif
#if defined _e2y
 char *tool="e2y";
 char *tool_desc="E(TSF) 2 Y(ambo) interface (0.6)";
#endif
