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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <parser.h>
#include <symbols.h>
#include <string.h>

static FILE *fout;

static char *str_trim(char *in)
{
	char *c, *s = in;

	for(c=s; isspace(*c); c++);
	for(; *c != '\0'; *s++=*c++);
	for(s--; isspace(*s); s--);
	*(s+1) = '\0';

	return in;
}

static int parse_get_line(FILE *f, char **s, int *length)
{
	int i, c;

	i = 0;
	do{
		c = getc(f);
		if(c == '#') /* skip comments */
			while(c!=EOF && c!='\n') c = getc(f);
		else if(c != EOF){
			if (i == *length - 1){
				*length *= 2;
				*s = (char *)realloc(*s, *length + 1);
			}
			(*s)[i++] = c;
		}
	}while(c != EOF && c != '\n');
	(*s)[i] = '\0';
	
	str_trim(*s);
	return c;
}

int parse_init(char *file_in, char *file_out)
{
	FILE *f;
	char *s;
	int c, length = 0;

	sym_init_table();

	if(strcmp(file_in, "-") == 0)
		f = stdin;
	else
		f = fopen(file_in, "r");

	if(strcmp(file_out, "-") == 0)
		fout = stdout;
	else{
		fout = fopen(file_out, "w");
		setvbuf(fout, NULL, _IONBF, 0);
	}
	/* fprintf(fout, "\n => Parser started\n\n");*/

	if(!f)
		return -1; /* error opening file */

	/* we now read in the file and parse */
	length = 40;
	s = (char *)malloc(length + 1);
	do{
		c = parse_get_line(f, &s, &length);
		if(*s){
			if(*s == '%'){ /* we have a block */
				*s = ' ';
				str_trim(s);
				if(getsym(s) != NULL){ /* error */
					/*fprintf(stderr, "%s \"%s\" %s", "Block", s, "already defined");*/
					do{ /* skip block */
						c = parse_get_line(f, &s, &length);
					}while(c != EOF && *s != '%');
				}else{ /* parse block */
					symrec *rec;
					rec = putsym(s, S_BLOCK);
					rec->value.block = (sym_block *)malloc(sizeof(sym_block));
					rec->value.block->n = 0;
					rec->value.block->lines = NULL;
					do{
						c = parse_get_line(f, &s, &length);
						if(*s && *s != '%'){
							char *s1, *tok;
							int l, col;

							l = rec->value.block->n;
							rec->value.block->n++;
							rec->value.block->lines = (sym_block_line *)
								realloc((void *)rec->value.block->lines, sizeof(sym_block_line)*(l+1));
							rec->value.block->lines[l].n = 0;
							rec->value.block->lines[l].fields = NULL;

							/* parse columns */
							for(s1 = s; (tok = strtok(s1, "|")) != NULL; s1 = NULL){
								char *tok2 = strdup(tok);
								str_trim(tok2);

								col = rec->value.block->lines[l].n;
								rec->value.block->lines[l].n++;
								rec->value.block->lines[l].fields = (char **)
									realloc((void *)rec->value.block->lines[l].fields, sizeof(char *)*(col+1));
								rec->value.block->lines[l].fields[col] = tok2;
							}
						}
					}while(c != EOF && *s != '%');
				}
			}else{ /* we can parse it np */
				parse_result c;
				parse_exp(s, &c);
			}
		}
	}while(c != EOF);

	free(s);
	if(f != stdin)
		fclose(f);

	sym_clear_reserved();

	return 0;
}

void parse_end()
{
	sym_end_table();
	/* fprintf(fout, "\n => Parser ended"); */
	if(fout != stdout)
		fclose(fout);
}

int parse_isdef(char *name)
{
	if(getsym(name) == NULL)
		return 0;
	return 1;
}

int parse_int(char *name, int def)
{
	symrec *ptr;
	int ret;

	ptr = getsym(name);	
	if(ptr && ptr->type == S_CMPLX){
		ret = (int)rint(GSL_REAL(ptr->value.c));
		/*fprintf(fout,"    [parser] %s = %d\n", name, ret);*/
	}else{
		ret = def;
		/*fprintf(fout, "    [parser] %s = %d\t[default value]\n", name, ret);*/
	}
	return ret;
}

double parse_double(char *name, double def)
{
	symrec *ptr;
	double ret;

	ptr = getsym(name);	
	if(ptr && ptr->type == S_CMPLX){
		ret = GSL_REAL(ptr->value.c);
		/*fprintf(fout, "    [parser] %s = %g\n", name, ret);*/
	}else{
		ret = def;
		/*fprintf(fout, "    [parser] %s = %g\t[default value]\n", name, ret);*/
	}
	return ret;
}

gsl_complex parse_complex(char *name, gsl_complex def)
{
	symrec *ptr;
	gsl_complex ret;

	ptr = getsym(name);	
	if(ptr && ptr->type == S_CMPLX){
		ret = ptr->value.c;
		/*fprintf(fout, "    [parser] %s = (%g, %g)\n", name, GSL_REAL(ret), GSL_IMAG(ret));*/
	}else{
		ret = def;
		/*fprintf(fout, "    [parser] %s = (%g, %g)\t[default value]\n", name, GSL_REAL(ret), GSL_IMAG(ret));*/
	}
	return ret;
}

char *parse_string(char *name, char *def)
{
	symrec *ptr;
	char *ret;

	ptr = getsym(name);	
	if(ptr && ptr->type == S_STR){
		ret = ptr->value.str;
		/*fprintf(fout, "    [parser] %s = \"%s\"\n", name, ret);*/
	}else{
		ret = def;
		/*fprintf(fout, "    [parser] %s = \"%s\"\t[default value]\n", name, ret);*/
	}
	return ret;
}

int parse_block_n(char *name)
{
	symrec *ptr;

	ptr = getsym(name);
	if(ptr && ptr->type == S_BLOCK){
		return ptr->value.block->n;
	}else
		return 0;
}

static int parse_block_work(char *name, int l, int col, parse_result *r)
{
	symrec *ptr;

	ptr = getsym(name);
	if(ptr && ptr->type == S_BLOCK){
		if(l < 0 || l >= ptr->value.block->n)
			return -2; /* dimension error */
		if(col < 0 || col >= ptr->value.block->lines[l].n)
			return -2;

		return parse_exp(ptr->value.block->lines[l].fields[col], r);
	}else
		return -1;
}

int parse_block_int(char *name, int l, int col, int *r)
{
	int o;
	parse_result pr;

	o = parse_block_work(name, l, col, &pr);

	if(o == 0 && pr.type == PR_CMPLX){
		*r = (int)rint(GSL_REAL(pr.value.c));
		/*fprintf(fout, "    [parser] %s(%d, %d) = %d\n", name, l, col, *r);*/
		return 0;
	}else
		return o;
}

int parse_block_double(char *name, int l, int col, double *r)
{
	int o;
	parse_result pr;

	o = parse_block_work(name, l, col, &pr);

	if(o == 0 && pr.type == PR_CMPLX){
		*r = GSL_REAL(pr.value.c);
		/*fprintf(fout, "    [parser] %s(%d, %d) = %g\n", name, l, col, *r);*/
		return 0;
	}else
		return o;
}

int parse_block_complex(char *name, int l, int col, gsl_complex *r)
{
	int o;
	parse_result pr;

	o = parse_block_work(name, l, col, &pr);

	if(o == 0 && pr.type == PR_CMPLX){
		*r = pr.value.c;
		/*fprintf(fout, "    [parser] %s(%d, %d) = (%g,%g)\n", name, l, col, GSL_REAL(*r), GSL_IMAG(*r));*/
		return 0;
	}else
		return o;
}

int parse_block_string(char *name, int l, int col, char **r)
{
	int o;
	parse_result pr;

	o = parse_block_work(name, l, col, &pr);

	if(o == 0 && pr.type == PR_STR){
		*r = pr.value.s;
		/*fprintf(fout, "    [parser] %s(%d, %d) = \"%s\"\n", name, l, col, *r);*/
		return 0;
	}else
		return o;
}
