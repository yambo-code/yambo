beg=$(cat o-3D_BSE.eps_q1_* | grep -n "Re(eps)" | cut -d: -f 1)
end=$(cat o-3D_BSE.eps_q1_* | grep "BEnSteps" | cut -d= -f 2 | cut -d# -f 1)
end=$((beg+end+1))

neigs=$(cat o-3D_BSE.eps_q1_* | grep "BSSNEig" | cut -d= -f 2 | cut -d# -f 1 | sed 's/ //g')
ncpus=$(cat o-3D_BSE.eps_q1_* | grep 'BS_nCPU_LinAlg_DIAGO=' | cut -d= -f 2 | cut -d# -f 1 | sed 's/ //g')

if [ "${1}" == "diago" ]; then
	file="csv_files/$1-${ncpus}.csv"
else
	file="csv_files/$1-${ncpus}-neig${neigs}.csv"
fi

cat o-3D_BSE.eps_q1_* | sed -n "${beg},${end}p" > ${file}

if [ "${2}" == "load_matrix_data" ]; then
	size=$(cat o-3D_BSE.eps_q1_* | grep "Dimension" | cut -d: -f2 | sed 's/ //g')
        echo "Matrix size"	> matrix_data.txt
	echo "${size}" >> matrix_data.txt
fi

sed -i '0,/#/s/#//' ${file}
sed -i '/#/d' ${file}
