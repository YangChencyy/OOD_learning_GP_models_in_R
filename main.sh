#!/bin/bash

#SBATCH --account=sunwbgt98
#SBATCH --job-name=main
#SBATCH --nodes=1
#SBATCH --mem=8GB
#SBATCH --time=24:00:00
#SBATCH --mail-user=rivachen@umich.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=standard


module load R
R CMD BATCH --no--save --no--restore main.R
