#!/bin/sh
#PBS -l ncpus=32,mpiprocs=32,host=hpc_c03
#PBS -M yosugi
#PBS -m abe
#PBS -N VISIToRN_MCMC
#PBS -e jobreport
#PBS -o jobreport

export OMP_NUM_THREADS=32      # 環境変数の設定
echo Working Directory is $PBS_O_WORKDIR
cd $PBS_O_WORKDIR
echo `pwd`
Rscript Run_VISIToRN_MCMC.R 2 > log.txt

