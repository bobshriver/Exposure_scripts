#!/bin/bash
#SBATCH --job-name=ExFUT_SWAallOD                      # the name of your job
#SBATCH --output=/home/jbb239/ExFUT_SWAallOD.out	# this is the file your output and errors go to
#SBATCH --time=120:00:00						# 48 hours, this is the MAX time your job will run
#SBATCH --workdir=/home/jbb239				# your work directory
#SBATCH --cpus-per-task=24 
#SBATCH —-mail-type=ALL

# load a module, for example
#module load workshop

# run your application, precede the application command with srun
srun date
srun Rscript ExtFUT_SWAall_OctDec.R
srun date
 