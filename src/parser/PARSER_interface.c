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

#include <c_defs.h>
#include <ctype.h>
#include <math.h>
#include <parser.h>
#include <symbols.h>

/* Interface to the parsing routines */
int C_FUNC(iparse_init, IPARSE_INIT)
		 (char *in, char *out)
{ 
	return parse_init(in, out); 
}

void C_FUNC(iparse_end, IPARSE_END)
		 ()
{ 
	parse_end(); 
}

int C_FUNC(iparse_isdef, IPARSE_ISDEF)
		 (char *name)
{ 
	return parse_isdef(name); 
}

void C_FUNC(iparse_int, IPARSE_INT)
		 (char *name, int *def, int *res)
{ 
	*res = parse_int(name, *def); 
}

void C_FUNC(iparse_double, IPARSE_DOUBLE)
		 (char *name, double *def, double *res)
{
	*res = parse_double(name, *def); 
}

void C_FUNC(iparse_complex, IPARSE_COMPLEX)
		 (char *name, gsl_complex *def, gsl_complex *res)
{
	*res = parse_complex(name, *def); 
}

void C_FUNC(iparse_string, IPARSE_STRING)
		 (char *name, char *def, char *res)
{
	char *c = parse_string(name, def);
	int len = strlen(c);
	strcpy(res, c);
	res[len] = res[len + 1];
        /*fprintf(stderr, " %s %d %s",name,len,c);*/
}

static void parse_block_error(char *type, char *name, int l, int c){
	/*fprintf(stderr, "Error: block \"%s\" does not contain a %s in line %d and col %d",
					name, type, l, c);
	exit(1);*/
}

int C_FUNC(iparse_block_n, IPARSE_BLOCK_N)
		 (char *name)
{
	return parse_block_n(name);
}

void C_FUNC(iparse_block_int, IPARSE_BLOCK_INT)
		 (char *name, int *l, int *c, int *res)
{
	if(parse_block_int(name, *l, *c, res) != 0)
		parse_block_error("int", name, *l, *c);
}

void C_FUNC(iparse_block_double, IPARSE_BLOCK_DOUBLE)
		 (char *name, int *l, int *c, double *res)
{
	if(parse_block_double(name, *l, *c, res) != 0)
		parse_block_error("double", name, *l, *c);
}

void C_FUNC(iparse_block_complex, IPARSE_BLOCK_COMPLEX)
		 (char *name, int *l, int *c, gsl_complex *res)
{
	if(parse_block_complex(name, *l, *c, res) != 0)
		parse_block_error("complex", name, *l, *c);
}

void C_FUNC(iparse_block_string, IPARSE_BLOCK_STRING)
		 (char *name, int *l, int *c, char *res)
{
	char *s;
	int len;

	if(parse_block_string(name, *l, *c, &s) != 0)
		parse_block_error("string", name, *l, *c);
	else{
		len = strlen(s);
		strcpy(res, s);
		res[len] = res[len + 1];
	}
}
