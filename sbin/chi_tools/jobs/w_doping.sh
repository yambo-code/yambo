#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --cpus-per-task=2
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=24:00:00
#SBATCH --job-name=w_doping
#SBATCH --output=log/w_doping.o%A
#SBATCH --error=log/w_doping.e%A
#SBATCH --mail-type=ALL,TIME_LIMIT_80
#SBATCH --mail-user=ad531114@ucf.edu

# Static screened interaction W(q) of a monolayer against electron doping. One folder per doping
# level, each with its own SAVE (QE scf + nscf with tot_charge = -x, smearing occupations,
# assume_isolated = '2D', then p2y), all on the same k grid:
#
#   cd WS2/doping && DIRS="n0 n0.01 n0.02 n0.05 n0.1" sbatch w_doping.sh
#
# For each folder: yambo setup and static screening (em1s, slab cutoff, RIM) at electronic
# temperature TEL, the same bands and block for every doping. Then, at the momenta QLIST (indices of
# the IBZ q-points; q1 = 0 is left out, where the intraband term needs a Drude model):
#   eps^-1_00(q)  = W_00(q) / v_00(q): the head of the inverse dielectric matrix, local fields included,
#                   i.e. the factor by which the screening reduces the e-h attraction at momentum q
#   W_2D(q)       = eps^-1_00(q) * 2 pi e^2 / q (eV A): the head as a strictly 2D interaction
# Writes w_doping.dat (table) and w_doping.png (both against the carrier density, one curve per q).
#
# The doping is the electron count of each SAVE (r_setup "Electrons") minus NEL0 (default: its
# integer part, i.e. the undoped count), per unit-cell area, in 1e13 cm^-2.
#
# k grid: the extra electrons fill small pockets around K (and Q). For their intraband screening to
# be sampled, the grid must resolve the Fermi pockets, and the smallest q of the grid (2 pi / (N a))
# should be comparable with 2 k_F. 12x12 resolves only heavy doping (~1e14 cm^-2); use 24x24 or more
# below that, and say which q you plot (|q| is in the table).

DIRS=${DIRS:?set DIRS to the doping folders, e.g. DIRS="n0 n0.02 n0.05"}
QLIST=${QLIST:-"2 3 4"}
TEL=${TEL:-0.025852}                 # eV (300 K): electronic temperature of the occupations
XBLK=${XBLK:-6}                      # Ry, as the BSE screening
CUT=${CUT:-'CUTGeo= "slab z"'}
RIM=${RIM:-$'RandQpts= 1000000\nRandGvec= 100 RL'}
NEL0=${NEL0:-}
YFLAGS=${YFLAGS-auto}
RUNNER=${RUNNER:-""}

if type module >/dev/null 2>&1 && [ -n "$SLURM_JOB_ID" ]; then
  module purge
  module use "$HOME/modulefiles"
  module load cuda_hpc_sdk/24.5
  module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
  module load yambo/5.3-cpu
  source "$NVHPC_ROOT/comm_libs/12.4/hpcx/latest/hpcx-init.sh" && hpcx_load
fi
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-${OMP_NUM_THREADS:-1}}
[ "$YFLAGS" = auto ] && { YFLAGS=""; yambo -h 2>&1 | grep -q nompi && YFLAGS=-nompi; }
NP=${NP:-${SLURM_NTASKS:-1}}
if [ "$NP" -gt 1 ]; then
  if [ "$NP" = "${SLURM_NTASKS:-}" ]; then [ -n "$RUNNER" ] || RUNNER="srun --cpus-per-task=$OMP_NUM_THREADS"
  else [ -n "$RUNNER" ] || RUNNER="srun -n $NP --cpus-per-task=$OMP_NUM_THREADS"; fi
  YFLAGS=""
fi
TOP="${SLURM_SUBMIT_DIR:-$PWD}"; cd "$TOP"; mkdir -p log

fit () {     # ntasks cap1 cap2 cap3 -> a b c, a*b*c = ntasks, each <= its cap
  local n=$1 c1=$2 c2=$3 c3=$4 a b c
  for ((a = c1 < n ? c1 : n; a >= 1; a--)); do (( n % a )) && continue
    for ((b = c2 < n / a ? c2 : n / a; b >= 1; b--)); do (( (n / a) % b )) && continue
      c=$((n / a / b)); (( c <= c3 )) && { echo $a $b $c; return 0; }
    done
  done
  return 1
}
rs () { awk -F: -v p="$2" '$0 ~ p {print $2+0; exit}' "$1/r_setup" 2>/dev/null; }

# every folder first: SAVE, setup at TEL, the same grid and spinors
for d in $DIRS; do
  [ -f "$TOP/$d/SAVE/ns.db1" ] || { echo "!!! $d/SAVE/ns.db1 not found"; exit 1; }
  if [ ! -f "$TOP/$d/r_setup" ]; then
    ( cd "$TOP/$d" && yambo $YFLAGS -i -V RL -F setup.in > /dev/null 2>&1 && echo "ElecTemp= $TEL eV" >> setup.in &&
      $RUNNER yambo $YFLAGS -F setup.in > log_setup.out 2>&1 )
  fi
  [ -n "$(rs $TOP/$d 'IBZ Q-points')" ] || { echo "!!! yambo setup failed in $d"; tail -5 "$TOP/$d/log_setup.out"; rm -f "$TOP/$d/r_setup"; exit 1; }
done
ref=""
for d in $DIRS; do
  s="$(rs $TOP/$d 'IBZ K-points') $(rs $TOP/$d 'Spinor components')"
  [ -z "$ref" ] && ref=$s
  [ "$s" = "$ref" ] || { echo "!!! $d has (IBZ k, spinors) = ($s), the first folder ($ref): use one grid for all"; exit 1; }
done
SF=$(rs $TOP/${DIRS%% *} 'Spinor components'); WB=${WB:-$((100 * SF))}
echo "=== W(q) against doping: $DIRS; screening $WB bands, block $XBLK Ry, T = $TEL eV, q = $QLIST; $NP tasks ==="

for d in $DIRS; do
  D=$TOP/$d
  if ls $D/wdop/ndb.em1s >/dev/null 2>&1; then echo "  $d: screening done"; continue; fi
  NB=$(rs $D '^ *Bands  '); NK=$(rs $D 'IBZ K-points'); NQ=$(rs $D 'IBZ Q-points')
  NKBZ=$(awk -F: '/BZ +K-points/ && !/IBZ/{print $2+0; exit}' $D/r_setup)
  VF=$(rs $D 'Filled Bands'); VM=$(awk -F: '/Metallic Bands/{n=split($2,a," "); print a[n]+0; exit}' $D/r_setup)
  [ -n "$VM" ] || VM=$VF
  [ "$WB" -le "$NB" ] || { echo "!!! $d: WB=$WB > the $NB bands of its SAVE"; exit 1; }
  par=""
  if [ "$NP" -gt 1 ]; then        # dipoles k, c (above the metallic bands), v (filled); screening k, q only
    p=($(fit $NP $NK $((WB - VM)) $VF)) || { echo "!!! $NP tasks do not fit the dipoles of $d"; exit 1; }
    x=($(fit $NP $NKBZ $NQ 1)) || { echo "!!! $NP tasks do not fit the screening of $d: k <= $NKBZ, q <= $NQ"; exit 1; }
    par=$(printf 'DIP_CPU= "%d %d %d"\nDIP_ROLEs= "k c v"\nX_and_IO_CPU= "%d 1 %d 1 1"\nX_and_IO_ROLEs= "q g k c v"' \
          ${p[0]} ${p[1]} ${p[2]} ${x[1]} ${x[0]})
  fi
  cat > $D/wdop.in <<EOF
em1s
rim_cut
ElecTemp= $TEL eV
X_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
$par
$CUT
$RIM
Chimod= "hartree"
% DipBands
 1 | $WB |
%
% BndsRnXs
 1 | $WB |
%
NGsBlkXs= $XBLK Ry
% LongDrXs
 1.0 | 0.0 | 0.0 |
%
EOF
  echo "  $d: screening ($(date +%H:%M))"
  for try in 1 2 3; do
    rm -rf $D/wdop $D/[rl]-wdop_* $D/LOG/l-wdop_*
    ( cd $D && $RUNNER yambo $YFLAGS -F wdop.in -J wdop > wdop.out 2>&1 )
    ls $D/wdop/ndb.em1s >/dev/null 2>&1 && break
    grep -qs "Reading File .*ndb.cutoff" $D/LOG/l-wdop_* $D/l-wdop_* || break   # Yambo MPI race: retry
  done
  ls $D/wdop/ndb.em1s >/dev/null 2>&1 || { echo "!!! screening failed in $d: see $d/wdop.out, $d/LOG";
    grep -h ERROR $D/LOG/l-wdop_* $D/l-wdop_* 2>/dev/null | sort -u | head -5; exit 1; }
done

python3 - "$TOP" "$DIRS" "$QLIST" "${NEL0}" <<'PY'
import glob, sys
import numpy as np
import netCDF4 as nc
top, dirs, ql, nel0 = sys.argv[1], sys.argv[2].split(), [int(x) for x in sys.argv[3].split()], sys.argv[4]
BOHR = 0.529177210903; E2 = 14.399645     # A; e^2 in eV A
rows, qabs = [], {}
for d in dirs:
    D = f'{top}/{d}'
    rs = {l.split(':')[0].strip(): l.split(':')[1] for l in open(f'{D}/r_setup') if ':' in l}
    nel = float(rs['Electrons'].split()[0])
    ef = float(next((v.split()[0] for k, v in rs.items() if 'Fermi Level' in k), 'nan'))
    s = nc.Dataset(f'{D}/SAVE/ns.db1')
    lat = np.array(s.variables['LATTICE_VECTORS'][:]); alat = np.array(s.variables['LATTICE_PARAMETER'][:])
    area = abs(np.cross(lat[0], lat[1])[2]) * BOHR ** 2 * 1e-16          # cm^2
    e = nc.Dataset(f'{D}/wdop/ndb.em1s')
    qp = np.array(e.variables['HEAD_QPT'][:]).T                          # units 2 pi / alat_i
    n0 = float(nel0) if nel0 else np.floor(nel + 1e-6)
    dn = nel - n0
    r = [d, nel, dn, dn / area / 1e13, ef]
    for iq in ql:
        q = 2 * np.pi * np.sqrt(np.sum((qp[iq - 1] / alat) ** 2)) / BOHR   # A^-1
        qabs[iq] = q
        f = glob.glob(f'{D}/wdop/ndb.em1s_fragment_{iq}')
        x = np.array(nc.Dataset(f[0]).variables[f'X_Q_{iq}'][0, 0, 0])     # head of eps^-1 - 1 (re, im)
        einv = 1 + x[0]
        r += [einv, einv * 2 * np.pi * E2 / q]
    rows.append(r)
rows.sort(key=lambda r: r[2])
with open(f'{top}/w_doping.dat', 'w') as out:
    hdr = '# dir  electrons  dn[e/cell]  n[1e13 cm^-2]  E_F[eV]' + ''.join(
        f'  eps^-1_00(q{iq}, |q|={qabs[iq]:.4f}/A)  W_2D(q{iq})[eV A]' for iq in ql)
    print(hdr); out.write(hdr + '\n')
    for r in rows:
        l = f'{r[0]:>10s} {r[1]:9.4f} {r[2]:9.4f} {r[3]:9.3f} {r[4]:9.4f}' + ''.join(f' {v:12.5f}' for v in r[5:])
        print(l); out.write(l + '\n')
try:
    import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
    n = [r[3] for r in rows]
    fig, ax = plt.subplots(1, 2, figsize=(10, 3.8))
    for j, iq in enumerate(ql):
        ax[0].plot(n, [r[5 + 2 * j] for r in rows], 'o-', label=f'q{iq}, |q| = {qabs[iq]:.3f} Å$^{{-1}}$')
        ax[1].plot(n, [r[6 + 2 * j] for r in rows], 'o-', label=f'q{iq}')
    ax[0].set_ylabel('ε$^{-1}_{00}$(q) = W(q) / v(q)'); ax[1].set_ylabel('W$_{2D}$(q) = ε$^{-1}_{00}$ 2πe²/q  (eV Å)')
    for a in ax:
        a.set_xlabel('electron density (10$^{13}$ cm$^{-2}$)'); a.legend(fontsize=8)
    fig.tight_layout(); fig.savefig(f'{top}/w_doping.png', dpi=150); print('wrote', f'{top}/w_doping.png')
except Exception as e:
    print('no plot:', e)
PY
