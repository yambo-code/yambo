/*
  License-Identifier: GPL

  Copyright (C) 2020 The Yambo Team

  Authors (see AUTHORS file for details): AM
*/
#include <driver.h>
#include <kind.h>
#include <stdio.h>
#include <string.h>

void title_yambo(FILE *file_name, char *cmnt, struct tool_struct t)
{
    char *tool = running_tool();
    char *pj = running_project();
    char *libs = running_libraries();
    fprintf(file_name, "%s%s\n", cmnt, " ___ __  _____  __ __  _____   _____ ");
    fprintf(file_name, "%s%s\n", cmnt, "|   Y  ||  _  ||  Y  ||  _  \\ |  _  |");
    fprintf(file_name, "%s%s\n", cmnt, "|   |  ||. |  ||.    ||. |  / |. |  |");
    fprintf(file_name, "%s%s\n", cmnt, " \\   _/ |. _  ||.\\ / ||. _  \\ |. |  |");
    fprintf(file_name, "%s%s\n", cmnt, "  |: |  |: |  ||: |  ||: |   \\|: |  |");
    fprintf(file_name, "%s%s\n", cmnt, "  |::|  |:.|:.||:.|:.||::.   /|::.  |");
    fprintf(file_name, "%s%s\n", cmnt, "  `--\"  `-- --\"`-- --\"`-----\" `-----\"");
    fprintf(file_name, "%s '%s' \n", cmnt, t.desc);
    if (strlen(pj) > 0)
    {
        fprintf(file_name, "%s\n%s This is      : %s(%s)", cmnt, cmnt, tool,
                pj);
    }
    else
    {
        fprintf(file_name, "%s\n%s This is      : %s", cmnt, cmnt, tool);
    }
    fprintf(file_name, "%s\n%s Version      : %s ", cmnt, cmnt,
            t.version_string);
    fprintf(file_name, "%s\n%s Configuration: %s \n", cmnt, cmnt, libs);
};


void title_lumen(FILE *file_name, char *cmnt, struct tool_struct t)
{
    char *tool = running_tool();
    char *pj = running_project();
    char *libs = running_libraries();
    fprintf(file_name, "%s%s\n", cmnt, "  _ ");
    fprintf(file_name, "%s%s\n", cmnt, " | |");
    fprintf(file_name, "%s%s\n", cmnt, " | |     _   _ _ __ ___   ____  __ _");
    fprintf(file_name, "%s%s\n", cmnt, " | |    | | | | '_ ' _  \\/ _  \\/ _` \\");
    fprintf(file_name, "%s%s\n", cmnt, " | |    | | | | | | | | |  ___/ | | |");
    fprintf(file_name, "%s%s\n", cmnt, " | |___ | |_| | | | | | |  \\__| | | |");
    fprintf(file_name, "%s%s\n", cmnt, " \\_____/\\__,__|_| |_| |_/\\____|_| |_|");
    fprintf(file_name, "%s%s\n", cmnt, " ");
    fprintf(file_name, "%s '%s' \n", cmnt, t.desc);
    if (strlen(pj) > 0)
    {
        fprintf(file_name, "%s\n%s This is      : %s(%s)", cmnt, cmnt, tool,
                pj);
    }
    else
    {
        fprintf(file_name, "%s\n%s This is      : %s", cmnt, cmnt, tool);
    }
    fprintf(file_name, "%s\n%s Version      : %s ", cmnt, cmnt,
            t.version_string);
    fprintf(file_name, "%s\n%s Configuration: %s \n", cmnt, cmnt, libs);
};
