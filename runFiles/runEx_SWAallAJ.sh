#!/bin/bash
#SBATCH --job-name=ExFSWAallAJ                      # the name of your job
#SBATCH --output=/home/jbb239/ExFSWAallAJ.out	# this is the file your output and errors go to
#SBATCH --time=48:00:00						# 48 hours, this is the MAX time your job will run
#SBATCH --workdir=/home/jbb239				# your work directory
#SBATCH --cpus-per-task=24 
#SBATCH —-mail-type=ALL

# load a module, for example
#module load workshop

# run your application, precede the application command with srun
srun date
srun Rscript Ex_SWABall_AprJun.R
srun date
 