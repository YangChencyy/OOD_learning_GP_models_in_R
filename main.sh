#!/bin/bash

#SBATCH --account=sunwbgt98
#SBATCH --job-name=main
#SBATCH --nodes=1
#SBATCH --mem=8GB
#SBATCH --time=24:00:00
#SBATCH --mail-user=rivachen@umich.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=standard

if [ "x${PBS_NODEFILE}" != "x" ]; then
    cat $PBS_NODEFILE
fi

cd /home/rivachen/OOD_learning_GP_models_in_R/

module load R
R CMD BATCH --no--save --no--restore function.R