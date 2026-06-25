#!/bin/sh
#PBS -l ncpus=32,mpiprocs=32,host=hpc_c02
#PBS -M yosugi
#PBS -m abe
#PBS -N Nmodel_Int
#PBS -e jobreport
#PBS -o jobreport

export OMP_NUM_THREADS=32      # 環境変数の設定
echo Working Directory is $PBS_O_WORKDIR
cd $PBS_O_WORKDIR
echo `pwd`
bash nitrogen.sh 2 > log.txt

