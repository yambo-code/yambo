/*  Zachary Levine   7 Dec 1994 Copyright

    Simple ANSI c routine which returns cpu time in seconds.
    Suitable for a call by a Fortran program.
    Usage (fortran): call cclock(cpu) for double precision cpu.
    Results are implementation-dependent with no particular guarantee
    of accuracy in the results.

*/

#include <time.h>

#if defined _irix || defined _ultrix || defined _dec_alpha
void cclock_ (cpu)
#elif defined _T3E || defined _T3Efhi
void CCLOCK (cpu)         /* Apparently MUST be uppercase for Cray */
#else
void cclock (cpu)         /* OK for ibm, hp with sppuxOS, ... */
#endif

double* cpu;
{
 *cpu = ((double) clock()) / CLOCKS_PER_SEC;
}
