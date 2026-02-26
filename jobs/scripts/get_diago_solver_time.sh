filename=csv_files/${1}neig_time_benchmark.csv
touch ${filename}
echo "n_CPU time" > ${filename}
for file in time/*.txt
do
	n_cpu=$(ls $file | cut -d- -f 1 | cut -d/ -f 2)
	time=$(cat $file | grep "Diago Solver" | cut -d: -f 2 | tail -n -1 | cut -ds -f 1)
	echo ${n_cpu} ${time} >> ${filename}
done
