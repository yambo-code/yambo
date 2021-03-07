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

/* This is essencially the example from bison */
%{
%}

%union {
	gsl_complex val;  /* For returning numbers.               */
	char *str;        /* For strings                          */
	symrec  *tptr;    /* For returning symbol-table pointers  */
}

%token <val>  NUM        /* Simple complex number   */
%token <str>  STR        /* For strings             */
%token <tptr> VAR FNCT   /* Variable and Function   */
%type  <val>  exp
%type  <str>  string

%right '='
%left '-' '+'
%left '*' '/'
%left NEG     /* Negation--unary minus */
%right '^'    /* Exponentiation        */

/* Grammar follows */

%%

input:   /* empty */
  | input line
;
     
line:
	'\n'
	| exp '\n'   { par_res.value.c = $1; par_res.type = PR_CMPLX; YYACCEPT;}
  | string '\n'{ par_res.value.s = $1; par_res.type = PR_STR;   YYACCEPT;}
  | error '\n' { yyerrok; YYABORT;}
;
     
exp: NUM               { $$ = $1;                         }
  | VAR                { $$ = $1->value.c;                }
  | VAR '=' exp        { $$ = $3; $1->value.c = $3; $1->type = S_CMPLX;}
  | FNCT '(' exp ')'   { $$ = (*($1->value.fnctptr))($3); }
  | exp '+' exp        { $$ = gsl_complex_add($1, $3);    }
  | exp '-' exp        { $$ = gsl_complex_sub($1, $3);    }
  | exp '*' exp        { $$ = gsl_complex_mul($1, $3);    }
  | exp '/' exp        { $$ = gsl_complex_div($1, $3);    }
  | '-' exp  %prec NEG { $$ = gsl_complex_negative($2);   }
  | exp '^' exp        { $$ = gsl_complex_pow($1, $3);    }
  | '(' exp ',' exp ')'{ GSL_SET_COMPLEX (&$$, GSL_REAL($2), GSL_REAL($4)); }
  | '(' exp ')'        { $$ = $2;                         }

string: STR            { $$ = $1; }
  | VAR '=' STR        { $$ = $3; $1->value.str = $3; $1->type = S_STR; }
;
%%
