#!/bin/bash

#SBATCH --account=sunwbgt98
#SBATCH --job-name=mnist_R
#SBATCH --nodes=1
#SBATCH --mem=8GB
#SBATCH --time=24:00:00
#SBATCH --mail-user=rivachen@umich.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=standard
#SBATCH --output=/home/rivachen/OOD_learning_GP_models_in_R/results_mnist_5000_32_3.log

module load R
Rscript --save main.R "MNIST" 5000 1000 32

