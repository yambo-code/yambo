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

#ifndef _SYMBOLS_H
#define _SYMBOLS_H

#include <gsl_complex.h>

typedef struct sym_block_line{
	int n;
	char **fields;
}sym_block_line;

typedef struct sym_block{
	int n;
	sym_block_line *lines;
}sym_block;

typedef enum{
	S_CMPLX, S_STR, S_BLOCK, S_FNCT
}symrec_type;

/* Data type for links in the chain of symbols. */
typedef struct symrec{
	char *name;                  /* name of symbol */
	symrec_type type;            /* type of symbol: either VAR or FNCT */

	union {
		gsl_complex c;             /* value of a VAR */
		char *str;                 /* value of a STRING */
		sym_block *block;          /* to store blocks */
		gsl_complex (*fnctptr)();  /* value of a FNCT */
	} value;

	struct symrec *next;         /* link field */
} symrec;

/* The symbol table: a chain of struct symrec. */
extern symrec *sym_table;

symrec *putsym (char *sym_name, symrec_type sym_type);
symrec *getsym (char *sym_name);
int rmsym (char *sym_name);
void sym_init_table();
void sym_clear_reserved();
void sym_end_table();
void sym_output_table();
char *str_tolower(char *in);

#endif
