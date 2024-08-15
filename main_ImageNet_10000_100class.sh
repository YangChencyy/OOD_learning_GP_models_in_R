#!/bin/bash

#SBATCH --account=sunwbgt0
#SBATCH --job-name=ImageNet_R_100class_80000_1000
#SBATCH --nodes=1
#SBATCH --mem=8GB
#SBATCH --time=24:00:00
#SBATCH --mail-user=rivachen@umich.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=standard
#SBATCH --output=/home/rivachen/OOD_learning_GP_models_in_R/results_imagenet_100calss_80000.log

module load R
Rscript --save main_laGP_100class.R "ImageNet" 80000 1000 128

