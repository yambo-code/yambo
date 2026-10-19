#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=08:00:00
#SBATCH --job-name=kern_cmp
#SBATCH --output=log/kern_cmp.o%A
#SBATCH --error=log/kern_cmp.e%A

# A exciton (q1) with Yambo's own TDDFT kernels next to the BSE and the exact exported kernel.
# Run from the directory where xq_dispersion.sh ran (SAVE, cohsex/ndb.em1s*, g0w0/ndb.QP,
# kx_xq/, xq_dispersion.dat, xq_dispersion_omega.dat), with the SAME BANDS/NG/GINP/QP.
#
#  transition space (BSE solver, eigenvalues):  SEX (reference), Hartree (RPA), ALDA
#  G space (Dyson equation, eps2 peak):         IP, RPA, LRC, PF (EMP, RBO, CUR, JGM)
#  exact kernel: static and self-consistent energies of q1 from xq_dispersion.dat
#  MDR kernel (Marini-Del Sole-Rubio, BSEChiOut= "mdr", needs the yambo build with it):
#                exported on the same grid, then Casida static and with fxc(w)
#
#  LRC is run with the alpha of the exact kernel at w = 0 and at the exciton (read from
#  kx_xq), and with the list ALPHAS to find the alpha that gives the BSE binding.
#  Yambo's LRC is fxc_00 = +LRC_alpha/|q|^2, so these runs use LRC_alpha = -alpha.
#  G-space ALDA is left out: in a slab it is dominated by the vacuum (n -> 0) and
#  changes with the number of G (see doc/greens_function_fxc.md); ALDA is run in
#  transition space. BSfxc does not work in Yambo 5.3.
#
# Output: kernel_compare.dat (kernel, A energy, binding) and the o-kc_* spectra.

BANDS=${BANDS:-"11 | 16"}
NG=${NG:-21}
GINP=${GINP:-1}
WBLK=${WBLK:-"5 Ry"}
WDIR=${WDIR:-cohsex}
QP=${QP:-'KfnQPdb= "E < g0w0/ndb.QP"'}
XQP=${XQP:-'XfnQPdb= "E < g0w0/ndb.QP"'}
ELO=${ELO:-0.5}; EHI=${EHI:-3.5}; DE=${DE:-0.005}; ETA=${ETA:-0.05}
ALPHAS=${ALPHAS:-"1.0 1.5 2.0 2.5 3.0"}
YFLAGS=${YFLAGS--nompi}

if type module >/dev/null 2>&1 && [ -n "$SLURM_JOB_ID" ]; then
  module purge
  module use "$HOME/modulefiles"
  module load cuda_hpc_sdk/24.5
  module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
  module load yambo/5.3-cpu
  source "$NVHPC_ROOT/comm_libs/12.4/hpcx/latest/hpcx-init.sh" && hpcx_load
fi
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-${OMP_NUM_THREADS:-1}}
cd "${SLURM_SUBMIT_DIR:-.}"; mkdir -p log
test -e kx_xq/ndb.Chi_fragment_1 || { echo "!!! kx_xq/ndb.Chi_fragment_1 missing: run xq_dispersion.sh first"; exit 1; }
test -e $WDIR/ndb.em1s || { echo "!!! $WDIR/ndb.em1s missing"; exit 1; }
rm -rf w_em1s; mkdir -p w_em1s; for f in $WDIR/ndb.em1s*; do ln -s "$PWD/$f" w_em1s/; done
NE=$(python3 -c "print(int(round(($EHI - $ELO) / $DE)) + 1)")
EXTRA=(); [ "$GINP" = 1 ] && EXTRA=(BSEGinplane)

chi_in () {   # file Chimod [extra lines...]   G-space Dyson equation at q1
  local f=$1 k=$2; shift 2
  cat > $f <<EOF
optics
chi
tddft
dipoles
X_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
Chimod= "$k"
NGsBlkXd= $NG RL
% QpntsRXd
 1 | 1 |
%
% BndsRnXd
 $BANDS |
%
% EnRngeXd
 $ELO | $EHI | eV
%
% DmRngeXd
 $ETA | $ETA | eV
%
ETStpsXd= $NE
% LongDrXd
 1.0 | 0.0 | 0.0 |
%
$XQP
EOF
  for l in "$@"; do printf '%s\n' "$l" >> $f; done
}
bse_in () {   # file BSKmod [extra lines...]   transition space at q1
  local f=$1 k=$2; shift 2
  cat > $f <<EOF
optics
bss
bse
dipoles
BSKmod= "$k"
BSEmod= "coupling"
BSSmod= "d"
BSENGexx= $NG RL
BSENGfxc= $NG RL
K_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
% BSEQptR
 1 | 1 |
%
% BSEBands
 $BANDS |
%
% BEnRange
 $ELO | $EHI | eV
%
% BDmRange
 $ETA | $ETA | eV
%
BEnSteps= $NE
% BLongDir
 1.0 | 0.0 | 0.0 |
%
BSEprop= "abs"
$QP
EOF
  for l in "$@"; do printf '%s\n' "$l" >> $f; done
}
run () {   # job job-string
  rm -rf $1; rm -f [rlo]-$1[._]*
  yambo $YFLAGS -F $1.in -J "$2" > log/$1.out 2>&1 && ls r-$1_* >/dev/null 2>&1 && return 0
  echo "!!! $1 failed: see l-$1* and log/$1.out"; tail -5 log/$1.out; return 1
}

# alpha of the exact kernel at w = 0 and at the exciton energy E_X of q1. E_X is the self-consistent
# energy of q1 (xq_dispersion_omega.dat, else column 7 of xq_dispersion.dat), else the BSE A exciton
# of kc_sex; alpha(E_X) is left out if E_X is outside the exported frequencies (never guessed)
kx_alpha () {   # [E_X fallback] -> "A0 AX WX SRC"
python3 - "$@" <<'PY2'
import sys, numpy as np, netCDF4
d = netCDF4.Dataset('kx_xq/ndb.Chi_fragment_1')
w = d.variables['CHI_FREQ_Q_1'][:]; w = w[:, 0] * 27.211386
f = d.variables['FXC_Q_1'][:]; f00 = f[:, 0, 0, 0]
qpg = netCDF4.Dataset('kx_xq/ndb.Chi').variables['CHI_QPG'][:]
q = abs(complex(qpg[0, 0, 0], qpg[0, 0, 1]))
a = -f00 * q**2
wx, src = None, '-'
for fn, col in (('xq_dispersion_omega.dat', 1), ('xq_dispersion.dat', 6)):
    try:
        for l in open(fn):
            s = l.split()
            if s and not s[0].startswith('#') and s[0] == '1': wx, src = float(s[col]), fn
    except (OSError, IndexError, ValueError):
        pass
    if wx is not None: break
if wx is None and len(sys.argv) > 1 and sys.argv[1] not in ('', 'nan'):
    wx, src = float(sys.argv[1]), 'BSE_A_exciton'
ax = 'nan'
if wx is not None and w[1:].min() <= wx <= w[1:].max():
    ax = f'{np.interp(wx, w[1:], a[1:]):.4f}'
elif wx is not None:
    src += '_outside_exported_range'
print(f'{a[0]:.4f} {ax} {wx if wx is not None else "nan"} {src}')
PY2
}
read A0 AX WX SRC <<< $(kx_alpha)

# transition space
bse_in kc_sex.in SEX "BSENGBlk= $WBLK" "${EXTRA[@]}"; run kc_sex "kc_sex,w_em1s"
if [ "$WX" = nan ]; then
  EBSE=$(grep -h "lowest bright excitation (strength" r-kc_sex_* 2>/dev/null | head -1 | awk '{print $NF}')
  read A0 AX WX SRC <<< $(kx_alpha "$EBSE")
fi
echo "=== exact kernel at q1: alpha(0) = $A0, alpha(E_X = $WX eV, from $SRC) = $AX ==="
[ "$AX" = nan ] && echo "!!! alpha(E_X) not available (E_X from $SRC): the LRC run at alpha(E_X) is skipped"
bse_in kc_hart.in Hartree "${EXTRA[@]}";            run kc_hart kc_hart
bse_in kc_alda.in ALDA "${EXTRA[@]}";               run kc_alda kc_alda
if [ "${MDR:-1}" = 1 ]; then
  bse_in kc_mdrx.in SEX "BSENGBlk= $WBLK" "${EXTRA[@]}" 'BSEChiOut= "mdr"'
  if run kc_mdrx "kc_mdrx,w_em1s"; then
    grep -h "MDR: P of the exchange-free" r-kc_mdrx_* | head -1
    rm -rf kx_mdr; mkdir -p kx_mdr; cp kc_mdrx/ndb.Chi* kx_mdr/
    bse_in kc_mdrst.in CHI "${EXTRA[@]}";           run kc_mdrst "kc_mdrst,kx_mdr"
    bse_in kc_mdr.in CHI "${EXTRA[@]}" BSEChiDyn;   run kc_mdr "kc_mdr,kx_mdr"
  fi
fi
# G space
chi_in kc_ip.in IP;        run kc_ip kc_ip
chi_in kc_rpa.in Hartree;  run kc_rpa kc_rpa
for a in $A0 $([ "$AX" != nan ] && echo $AX) $ALPHAS; do
  chi_in kc_lrc$a.in LRC "LRC_alpha= -$a"; run kc_lrc$a kc_lrc$a
done
for p in EMP RBO CUR JGM; do
  chi_in kc_pf$p.in PF "PF_alpha= \"$p\""; run kc_pf$p kc_pf$p
done

python3 - "$A0" "$AX" <<'PY'
import glob, re, sys
import numpy as np
a0, ax = sys.argv[1], sys.argv[2]

def rep(job, key):
    for f in glob.glob(f'r-{job}_*'):
        for l in open(f):
            if key in l: return float(l.split()[-1])
    return np.nan

def peak(job):
    fs = glob.glob(f'o-{job}.eps_q1_*')
    if not fs: return np.nan, np.nan
    d = np.loadtxt(fs[0]); w, y = d[:, 0], d[:, 1]
    for i in range(1, len(y) - 1):
        if y[i] > 0.05 * y.max() and y[i] >= y[i - 1] and y[i] >= y[i + 1]:
            a, b, c = y[i - 1], y[i], y[i + 1]; den = a - 2 * b + c
            return w[i] + (0.5 * (a - c) / den if den else 0) * (w[1] - w[0]), y[i]
    return np.nan, np.nan

gap = np.nan
for f in glob.glob('r-kc_sex_*'):
    for l in open(f):
        if 'lowest transition energy of the window' in l: gap = float(l.split()[-1])
rows = []
for job, name in (('kc_sex', 'BSE (SEX), reference'), ('kc_hart', 'RPA (Hartree), transition space'),
                  ('kc_alda', 'ALDA, transition space'), ('kc_mdrst', 'MDR kernel fxc(0), static')):
    e = rep(job, 'lowest bright excitation (strength')
    rows.append((name, e, gap - e, peak(job)[1]))
try:
    for l in open('xq_dispersion.dat'):
        s = l.split()
        if s and not s[0].startswith('#') and s[0] == '1':
            rows.append(('exact kernel fxc(w), self-consistent', float(s[6]), gap - float(s[6]), np.nan))
            rows.append(('exact kernel fxc(0), static', float(s[7]), gap - float(s[7]), np.nan))
except OSError:
    pass
lrc = []
for job, name in [('kc_mdr', 'MDR kernel fxc(w)'), ('kc_ip', 'IP'), ('kc_rpa', 'RPA, G space')] + \
        [(re.match(r'o-(.*?)\.eps', f).group(1), None) for f in sorted(glob.glob('o-kc_lrc*.eps_q1_*'))] + \
        [(f'kc_pf{p}', f'PF {p}') for p in ('EMP', 'RBO', 'CUR', 'JGM')]:
    p, h = peak(job)
    if name is None:
        a = job[len('kc_lrc'):]
        tag = ' (exact alpha(0))' if a == a0 else ' (exact alpha(E_X))' if a == ax else ''
        name = f'LRC alpha = {a}{tag}'
        lrc.append((float(a), gap - p))
    rows.append((name, p, gap - p, h))
eb = rows[0][2]
lrc.sort()
fit = np.nan
for (a1, e1), (a2, e2) in zip(lrc, lrc[1:]):
    if np.isfinite(e1) and np.isfinite(e2) and (e1 - eb) * (e2 - eb) <= 0 and e2 != e1:
        fit = a1 + (eb - e1) * (a2 - a1) / (e2 - e1); break
with open('kernel_compare.dat', 'w') as out:
    out.write(f'# q1, window gap {gap:.4f} eV; binding = gap - A exciton; height = eps2 at the peak\n')
    out.write(f'# LRC alpha giving the BSE binding ({eb:.3f} eV): {fit:.3f} (linear between neighbours)\n')
    out.write(f'# {"kernel":42s} {"A [eV]":>8s} {"Eb [eV]":>8s} {"height":>8s}\n')
    for n, e, b, h in rows:
        out.write(f'  {n:42s} {e:8.4f} {b:8.4f} {h:8.2f}\n')
print(open('kernel_compare.dat').read())
PY
