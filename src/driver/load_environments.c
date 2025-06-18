/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <kind.h>

int load_environments(char* file_name)
{
 FILE *fp;
 char str[100];
 char* pch;
 char* token;
 char* var;
 char* value;
 fp = fopen(file_name, "r");
 if (fp) {
  while(fgets(str, 100, fp)) {
    pch=strchr(str,'#');
    if (!pch) {
      /* get the first token */
      token=strtok(str," ");
      /* walk through other tokens */
      if ( token != NULL ) 
      {
        token = strtok(NULL," ");
        var=token;
        token = strtok(NULL," ");
        value=token;
        /* printf( " %s %s %s \n", var, value, token ); */
      }
      setenv(var,value,1);
    }
  }
  return 0;
 }else{
  fp = fopen(file_name, "w+");
  fputs("#\n",fp);
  fputs("# Edit it and use with -E during runtime\n#\n",fp);
  fputs("# CPU section (just edit, do not remove fields)\n",fp);
  fputs("setenv YAMBO_X_q_0_CPU 1.1.1.1\n",fp);
  fputs("setenv YAMBO_X_finite_q_CPU 1.1.1.1.1\n",fp);
  fputs("setenv YAMBO_X_all_q_CPU 1.1.1.1.1\n",fp);
  fputs("setenv YAMBO_BS_CPU 1.1.1\n",fp);
  fputs("setenv YAMBO_SE_CPU 1.1.1\n",fp);
  fputs("setenv YAMBO_RT_CPU 1.1.1.1\n",fp);
  fputs("# Scalapack section (leave unchanged if you wish)\n",fp);
  fputs("setenv YAMBO_X_q_0_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_X_finite_q_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_X_all_q_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_BS_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_BS_nCPU_LinAlg_DIAGO 1\n",fp);
  fputs("# ROLEs section (leave unchanged if you wish)\n",fp);
  fputs("setenv YAMBO_X_q_0_ROLEs g.k.c.v\n",fp);
  fputs("setenv YAMBO_X_finite_q_ROLEs q.g.k.c.v\n",fp);
  fputs("setenv YAMBO_X_all_q_ROLEs q.g.k.c.v\n",fp);
  fputs("setenv YAMBO_BS_ROLEs k.eh.t\n",fp);
  fputs("setenv YAMBO_SE_ROLEs q.qp.b\n",fp);
  fputs("setenv YAMBO_RT_ROLEs k.b.q.qp\n",fp);
  fclose(fp);
  return 1;
 }
};
