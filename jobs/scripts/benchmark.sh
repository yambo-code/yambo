
array_nodes=(1 4)

solver=
if [ "${1}" == "chase" ]; then
	letter='c'
	solver="chase"
	array_neigs=(10 20 50 100)
elif [ "${1}" == "diagx" ]; then
	letter='x'
	solver="diagx"
	array_neigs=(10 20 50 100)
else
	letter="d"
	solver="diago"
	array_neigs=(0)
fi

sed -i "s/BSSmod=\(.*\)/BSSmod=\"${letter}\"/" 02_BSE_pardiago_solver.in

for neig in ${array_neigs[@]}
do
	sed -i "s/BSSNEig=\([0-9]*\)/BSSNEig=${neig}/" 02_BSE_pardiago_solver.in

	for node in ${array_nodes[@]}
	do
		ncpu=$((4 * ${node}))

		if [ "${solver}" == "chase" ]; then
			sed -i "s/nodes=\([0-9]*\)/nodes=${node}/" chase_run.sh
			sed -i "s/ntasks=\([0-9]*\)/ntasks=${ncpu}/" chase_run.sh
			sed -i "s/ntasks-per-node=\([0-9]*\)/ntasks-per-node=4/" chase_run.sh
		else
			sed -i "s/nodes=\([0-9]*\)/nodes=${node}/" slk_run.sh
			sed -i "s/ntasks=\([0-9]*\)/ntasks=${ncpu}/" slk_run.sh
			sed -i "s/ntasks-per-node=\([0-9]*\)/ntasks-per-node=4/" slk_run.sh
		fi

		echo "------------ Solving new eigenproblem with ${solver} --------------"
		echo "--> Number of CPU         = ${ncpu}"
		echo "--> Number of node        = ${node}"
		echo "--> Number of task/node   = 4"
		echo "--> Number of CPU/task    = 32"
		
		matrix_size=$(tail -n 1 matrix_data.txt | sed 's/ //g' 2>/dev/null)
		echo "--> Matrix size           = ${matrix_size}" 

		if [ "${solver}" == "chase" ]; then
			echo "--> Number of eigenvalues = ${neig}"
		elif [ "${solver}" == "diagx" ]; then
			echo "--> Number of eigenvalues = ${neig}"
		else
			echo "--> Number of eigenvalues = All"
		fi

		sed -i "s/BS_nCPU_LinAlg_DIAGO=\([0-9]*\)/BS_nCPU_LinAlg_DIAGO=${ncpu}/" 02_BSE_pardiago_solver.in

		if [ "${solver}" == "chase" ]; then
			job=$(sbatch chase_run.sh | cut -d' ' -f 4)
		else
			job=$(sbatch slk_run.sh | cut -d' ' -f 4)
		fi

		state=$(squeue -j ${job} -o %t | tail -n 1 | sed 's/ //g')
		while [[ "$state" != "CG" ]]
		do
			state=$(squeue -j ${job} -o %t | tail -n 1)
		done

		linalg_time=$(cat r-3D* | grep "LINEAR ALGEBRA" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1 | sed 's/ //g')

		cp r-3D* time/${solver}-${ncpu}-CPU_time.txt

		if [ "${solver}" == "diago" ]; then
			if [ "${node}" ==  "1" ]; then 
				bash output_to_csv.sh "diago" "load_matrix_data"
			else
				bash output_to_csv.sh "diago"
			fi
		else

			bash output_to_csv.sh ${solver}
		fi

		echo "==> Elapsed Time          = ${linalg_time}s"
		echo "---------------------------------------------------------------"

		bash clean.sh
	done

	bash get_solver_time.sh ${solver} ${neig} 
done
