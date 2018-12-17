#!/bin/bash
#SBATCH --job-name=ExSWABall_OctDec
#SBATCH --output=/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data/SWABall_OctDec.out 
#SBATCH -n 1
#SBATCH -c 10
#SBATCH -p normal
#SBATCH --account=swbsc
#SBATCH --time=40:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=rshriver@usgs.gov
#SBATCH --workdir=/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_scripts/For_Sense

# load a module, for example
#module load workshop

# run your application, precede the application command with srun
# run your application, precede the application command with srun
srun date
srun Rscript Ex_SWABall_OctDec.R
srun date
 
