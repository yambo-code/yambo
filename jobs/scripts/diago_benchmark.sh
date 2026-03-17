sed -i 's/BSSmod=\(.*\)"/BSSmod="d"/' 02_BSE_pardiago_solver.in

array_nodes=(1 4)

for node in ${array_nodes[@]}
do
	ncpu=$((4 * ${node}))

	sed -i "s/nodes=\([0-9]*\)/nodes=${node}/" slk_run.sh
	sed -i "s/ntasks=\([0-9]*\)/ntasks=${ncpu}/" slk_run.sh
	sed -i "s/ntasks-per-node=\([0-9]*\)/ntasks-per-node=4/" slk_run.sh

	sed -i "s/BS_nCPU_LinAlg_DIAGO=\([0-9]*\)/BS_nCPU_LinAlg_DIAGO=${ncpu}/" 02_BSE_pardiago_solver.in  	

	job=$(sbatch slk_run.sh | cut -d' ' -f 4)

	state=$(squeue -j ${job} -o %t | tail -n 1 | sed 's/ //g')
	while [[ "$state" != "CG" ]]
	do
		state=$(squeue -j ${job} -o %t | tail -n 1)
	done

	cp r-3D* time/diago-${ncpu}-CPU_time.txt

	if [ "${node}" ==  "1" ]; then 
		bash output_to_csv.sh "diago" "load_matrix_data"
	else
		bash output_to_csv.sh "diago"
	fi

	bash clean.sh
done

bash get_solver_time.sh "diago" ${neig} 

