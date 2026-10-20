#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --partition=normal
#SBATCH --job-name=p2y
#SBATCH --output=p2y.o%A
#SBATCH --error=p2y.e%A
#SBATCH --mail-type=ALL,TIME_LIMIT_80
#SBATCH --mail-user=ad531114@ucf.edu

# QE -> Yambo: p2y in the QE <prefix>.save folder, then the SAVE moves to DEST and the yambo setup
# runs there (r_setup). On a compute node: p2y of a SOC nscf with many bands needs a few GB, more
# than a login node allows. Submit from inside the .save folder:
#
#   cd tmp/ws2.save && DEST=~/wse2ws2/WS2/prod/k24 sbatch ~/yambo-cpu/sbin/chi_tools/jobs/p2y.sh
#
# p2y comes from the cluster's Yambo 5.2.3 module (the 5.3 build has no p2y); Yambo 5.3 reads its
# SAVE. The setup runs with the yambo of the production runs.

DEST=${DEST:?set DEST to the folder that gets the SAVE (e.g. DEST=../../GW)}
cd "${SLURM_SUBMIT_DIR:-.}"
[ -f data-file-schema.xml ] || { echo "!!! run from the QE <prefix>.save folder (no data-file-schema.xml here)"; exit 1; }
[ -e "$DEST/SAVE" ] && { echo "!!! $DEST/SAVE exists: move it away first"; exit 1; }

module purge
module load yambo/yambo-5.2.3-openmpi-4.1.5-gcc-12.2.0
echo "p2y: $(which p2y)"
rm -rf SAVE
p2y > p2y.log 2>&1 || { tail -20 p2y.log; exit 1; }
[ -f SAVE/ns.db1 ] || { echo "!!! p2y wrote no SAVE/ns.db1"; tail -20 p2y.log; exit 1; }

mkdir -p "$DEST" && mv SAVE "$DEST"/ || exit 1
module purge
module use "$HOME/modulefiles"
module load cuda_hpc_sdk/24.5
module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
module load yambo/5.3-cpu
cd "$DEST" && rm -f r_setup && yambo -nompi > /dev/null 2>&1 || yambo > /dev/null 2>&1
grep -E "^ *Bands  |IBZ K-points|Spinor components|Grid dimensions|Filled Bands|Direct Gap  |Indirect Gap  " r_setup ||
  echo "!!! no r_setup in $DEST: run yambo there by hand"
