"""Patch Yambo's bundled libraries for cross-compilation; run in the source."""
from pathlib import Path
import re
import subprocess

# IOTK's configure runs test programs, which cannot work on the x86 login
# node. Check the kinds it needs at compile time and fill in its header
# template with the conservative __IOTK_SAFEST settings.
Path('iotk-kinds.f90').write_text('''program kinds
  integer, parameter :: i = 1 / merge(1,0,kind(1)==4)
  integer, parameter :: l = 1 / merge(1,0,kind(.true.)==4)
  integer, parameter :: r = 1 / merge(1,0,kind(1.0)==4)
  integer, parameter :: d = 1 / merge(1,0,selected_real_kind(14,200)==8)
end program
''')
subprocess.check_call(['mpifrtpx', '-c', 'iotk-kinds.f90', '-o', 'iotk-kinds.o'])
iotk = Path('lib/iotk')
values = {'IOTK_MAX_RANK_LINE': '#define __IOTK_MAX_RANK 4',
          'IOTK_SAFEST_LINE': '#define __IOTK_SAFEST',
          'IOTK_INTEGER1': '#define __IOTK_INTEGER1 4',
          'IOTK_LOGICAL1': '#define __IOTK_LOGICAL1 4',
          'IOTK_REAL1': '#define __IOTK_REAL1 4',
          'IOTK_REAL2': '#define __IOTK_REAL2 8'}
header = (iotk / 'iotk/include/iotk_config.h.in').read_text()
(iotk / 'iotk/include/iotk_config.h').write_text(
    re.sub(r'@([A-Z_0-9]+)@', lambda m: values.get(m.group(1), ''), header))
(iotk / 'make.sys').write_text((iotk / 'make_iotk.inc').read_text())
(iotk / 'configured.stamp').touch()

# DevXlib's configure needs the cross-compilation flags and every compiler
# variable (its first probe reads FC, before its own F90/MPIF90 logic).
for name in ['Makefile.loc', 'Makefile']:
    p = Path('lib/devxlib') / name
    s = p.read_text()
    start = s.index('CONFFLAGS=')
    end = s.index('#\n# MAIN target', start)
    p.write_text(s[:start] +
        'CONFFLAGS=--build=x86_64-pc-linux-gnu --host=aarch64-unknown-linux-gnu '
        '--prefix=$(LIBPATH) $(devxlib_flgs) \\\n'
        ' --with-blas-libs="$(BLAS_LIBS)" --with-lapack-libs="$(LAPACK_LIBS)" \\\n'
        ' FC="$(fc)" F77="$(f77)" F90="$(fc)" MPIF90="$(fc)" \\\n'
        ' F90FLAGS="$(fcflags)" FCFLAGS="$(fcflags)" FFLAGS="$(fflags)" \\\n'
        ' CC="$(cc)" CFLAGS="$(cflags)"\n' + s[end:])

# Ydiago passes all of CFLAGS to cpp and no flags to the Fortran compiler.
p = Path('lib/ydiago/Ydiago-0.4.1/src/Makefile')
s = p.read_text()
s = s.replace('$(CPP) -E -P $(CFLAGS)', '$(CPP) -E -P $(filter -D% -I%,$(CFLAGS))')
s = s.replace('$(FC) -c $<', '$(FC) $(FCFLAGS) -c $<')
p.write_text(s)
