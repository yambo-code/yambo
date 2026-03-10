#!/bin/bash -x
#SBATCH --account=slai
#SBATCH --nodes=4
#SBATCH --ntasks=16
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=32
#SBATCH --output=job-%j.out
#SBATCH --error=job-%j.err
#SBATCH --time=0:15:00
#SBATCH --partition=dc-gpu-devel

export SRUN_CPUS_PER_TASK=${SLURM_CPUS_PER_TASK}
export OMP_NUM_THREADS=${SRUN_CPUS_PER_TASK}
export OPENBLAS_NUM_THREADS=${SRUN_CPUS_PER_TASK}

echo "Num threads = ${OPENBLAS_NUM_THREADS}"

#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 01_3D_BSE_screening.in -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -o b -k sex -F 02_3D_BSE_kernel.in -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 02_3D_BSE_kernel.in -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 03_3D_BSE_pardiago_solver.in -y d -V par qp -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 02_3D_BSE_pardiago_solver.in -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -D
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F Inputs/01_init -J 01_init
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -i -V RL
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -X s -F 01_BSE_screening.in
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 01_BSE_screening.in -J 3D_BSE

#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -D
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -i -V RL
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -X s -F 01_BSE_screening.in
#srun /p/home/jusers/richefort2/jureca/Libraries/yambo/bin/yambo -F 01_BSE_screening.in -J 3D_BSE
#srun /p/home/jusers/richefort2/jureca/Projects/yambo/bin/yambo -F 02_BSE_pardiago_solver.in -y c -V par qp -J 3D_BSE
srun /p/home/jusers/richefort2/jureca/Projects/yambo/bin/yambo -F 02_BSE_pardiago_solver.in -J 3D_BSE
