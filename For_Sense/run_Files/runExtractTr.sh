#!/bin/bash
#SBATCH --job-name=                   # the name of your job
	# this is the file your output and errors go to
#!/bin/bash
#SBATCH --job-name=ExtractTr10  
#SBATCH --output=/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data/ExtractTr10.out 
#SBATCH -n 1
#SBATCH -c 20
#SBATCH -p normal
#SBATCH --account=swbsc
#SBATCH --time=40:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=rshriver@usgs.gov
#SBATCH --workdir=/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_scripts

# load a module, for example
#module load workshop

# run your application, precede the application command with srun
# run your application, precede the application command with srun
srun date
srun Rscript ExtractTr10.R
srun date
 