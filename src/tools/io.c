/*
 Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 02111-1307, USA.
*/

#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <wrapper.h>
#include <stdio.h>

void C_FUNC(imkdir, IMKDIR)
                 (char *name) 
{
        struct stat buf;
        if(!*name || stat(name, &buf) == 0) return;
        mkdir(name, 0775);
}
                 
void C_FUNC(ichdir, ICHDIR)
                 (char *name)
{
        chdir(name);
}
                 
void C_FUNC(irename, IRENAME)
                 (char *namein, char *nameout)
{       
        rename(namein,nameout);
}

void C_FUNC(iremove, IREMOVE)
                 (char *name)
{
        remove(name);
}

void C_FUNC(isystem, ISSYSTEM)
                 (char *name, int* ierr)
{
        *ierr=system(name);
}

void C_FUNC(igetcwd, IGETCWD)
                 (char* name, int* ln)
{
  getcwd(name,256);
  *ln=strlen(name);
}
 
void C_FUNC(igethname, IGETHNAME)
                 (char* name, int* ln)
{
  gethostname(name,256);
  *ln=strlen(name);
}

void C_FUNC(ifolder_list, IFOLDER_LIST)
                 (char* folder, char* list, int* ln)
{
 DIR *dir,*subdir;
 struct dirent *ent;
 char the_list[100000]={'\0'};
 char  PWD[256] = ".";
 *ln=0;
 getcwd(PWD,256);
 chdir(folder);
 dir = opendir (".");
 if (dir != NULL) {
   while ((ent = readdir (dir)) != NULL) 
    if ( (subdir = opendir (ent->d_name)) != NULL) {
      strcat(the_list," ");
      strcat(the_list,ent->d_name);
      closedir (subdir);
    }
   closedir (dir);
   *ln=strlen(the_list);
   sprintf(list,"%s",the_list);
 }
 chdir(PWD);
}

