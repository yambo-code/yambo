if [ "${1}" == "diago" ]; then
	filename=csv_files/${1}_time_benchmark.csv
else
	filename=csv_files/${1}-${2}neig_time_benchmark.csv
fi

touch ${filename}

echo "n_CPU ${1}_BSS_time LINEAR_ALGEBRA_driver_time ${1}_eigensolver_time init_time" > ${filename}

for file in time/*.txt
do
	BSS_time=
	init_time=
	linalg_time=
	eigensolver_time=
	n_cpu=$(ls $file | cut -d- -f 2)

	if [ "${1}" == "diagx" ]; then
		BSS_time=$(cat $file | grep "Diagx Solver" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
		eigensolver_time=$(cat $file | grep "_feweigenvalues" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
	elif [ "${1}" == "chase" ]; then
		BSS_time=$(cat $file | grep "ChASE Solver" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
		eigensolver_time=$(cat $file | grep "_chase" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
		init_time=$(cat $file | grep "ChASE_INIT" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
	else
		BSS_time=$(cat $file | grep "Diago Solver" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
		eigensolver_time=$(cat $file | grep "_diagonalization" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
	fi
	linalg_time=$(cat $file | grep "LINEAR ALGEBRA" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)

	echo ${n_cpu} ${BSS_time} ${linalg_time} ${eigensolver_time} ${init_time} >> ${filename}
done
rm time/*
