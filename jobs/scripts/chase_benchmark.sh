sed -i 's/BSSmod=\(.*\)"/BSSmod="c"/' 02_BSE_pardiago_solver.in
array_nodes=( 1 4)
array_neigs=(10 20 50 100)
for neig in ${array_neigs[@]}
do
	sed -i "s/BSSNEig=\([0-9]*\)/BSSNEig=${neig}/" 02_BSE_pardiago_solver.in  
	for node in ${array_nodes[@]}
	do
		ncpu=$((4*${node}))
		echo "NCPU is = $ncpu, neig is = $neig"
		sed -i "s/nodes=\([0-9]*\)/nodes=${node}/" chase_run.sh
		sed -i "s/ntasks=\([0-9]*\)/ntasks=${ncpu}/" chase_run.sh
		sed -i "s/ntasks-per-node=\([0-9]*\)/ntasks-per-node=4/" chase_run.sh
		sed -i "s/BS_nCPU_LinAlg_DIAGO=\([0-9]*\)/BS_nCPU_LinAlg_DIAGO=${ncpu}/" 02_BSE_pardiago_solver.in  	

		job=$(sbatch chase_run.sh | cut -d' ' -f 4)
		state=$(squeue -j ${job} -o %t | tail -n 1 | sed 's/ //g')
		while [[ "$state" != "CG" ]]
		do
			state=$(squeue -j ${job} -o %t | tail -n 1)
		done
		bash output_to_csv.sh "chase"
		cp r-3D* time/chase-${ncpu}-CPU_time.txt
		bash clean.sh
	done
	bash get_solver_time.sh "chase" ${neig}
done

