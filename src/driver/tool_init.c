/*
  License-Identifier: GPL

  Copyright (C) 2020 The Yambo Team

  Authors (see AUTHORS file for details): AM

*/

#include <driver.h>
#include <kind.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wrapper.h>
#if defined _yambo || defined _ypp
#include <editor.h>
#endif

#ifdef _yambo
const char *tool = "yambo";
const char *tool_desc = "A shiny pot of fun and happiness [C.D.Hogan]";
#elif defined _ypp
const char *tool = "ypp";
const char *tool_desc = "Y(ambo) P(ost)/(re) P(rocessor)";
#elif defined _a2y
const char *tool = "a2y";
const char *tool_desc = "A(binit) 2 Y(ambo) interface";
#elif defined _c2y
const char *tool = "c2y";
const char *tool_desc = "C(pmd) 2 Y(ambo) interface";
#elif defined _p2y
const char *tool = "p2y";
const char *tool_desc = "P(Wscf) 2 Y(ambo) interface";
#elif defined _e2y
const char *tool = "e2y";
const char *tool_desc = "E(TSF) 2 Y(ambo) interface (0.6)";
#else
const char *tool = "none";
const char *tool_desc = "Not a tool";
#endif

struct tool_struct tool_init()
{
    tool_struct t;
    t = versions();
#if defined _yambo || defined _ypp
    t.editor = editor;
#else
    t.editor = "vim";
#endif
    t.tool = tool;
    t.desc = tool_desc;
    /*
      Projects
    */
    char *pj = NULL;
#if defined _YPP_ELPH || defined _ELPH
    pj = "ph";
#endif
#if defined _YPP_RT || defined _RT
    pj = "rt";
#endif
#if defined _YPP_SC || defined _SC
    pj = "sc";
#endif
#if defined _YPP_NL || defined _NL
    pj = "nl";
#endif
#if defined _YPP_FL || defined _FL
    pj = "fl";
#endif
#if defined _QED
    pj = "qed";
#endif

    if (pj != NULL)
    {
        t.bin = malloc(strlen(tool) + strlen(pj) + 2);
        strcpy(t.bin, t.tool);
        t.pj = pj;
        strcat(t.bin, "_");
        strcat(t.bin, t.pj);
    }
    else
    {
        t.bin = malloc(strlen(tool) + 1);
        strcpy(t.bin, t.tool);
        pj = "";
        t.pj = pj;
    }
    if (pj == NULL)
    {
        pj = " ";
    }
    sprintf(t.version_string, "%i.%i.%i Revision %i Hash %s", t.version,
            t.subversion, t.patchlevel, t.revision, t.hash);
    return (t);
};
