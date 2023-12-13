#!/bin/bash

#SBATCH --account=sunwbgt98
#SBATCH --job-name=cifar_10
#SBATCH --nodes=1
#SBATCH --mem=8GB
#SBATCH --time=24:00:00
#SBATCH --mail-user=rivachen@umich.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=standard
#SBATCH --output=/home/rivachen/OOD_learning_GP_models_in_R/results_mnist_5000_32.log

module load R
Rscript --save main.R "Cifar_10" 1000 1000 32    

