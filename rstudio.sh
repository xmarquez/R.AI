#!/bin/bash
#SBATCH --job-name=rstudio
#SBATCH --output=rstudio_%j.log
#SBATCH --error=rstudio_%j.err
#SBATCH --time=04:00:00
#SBATCH --cpus-per-task=20
#SBATCH --mem=16G
#SBATCH --partition=parallel

# Load necessary modules
module purge
module load GCC/10.2.0 OpenMPI/4.0.5 Singularity/3.10.2

# Start RStudio Server in the Singularity container with $PWD as the working directory
PASSWORD='yourpassword' singularity exec \
--home $PWD \
--bind /nfs/scratch/marquexa/rstudio-tmp:/tmp \
--bind /nfs/scratch/marquexa/rstudio-lib:/var/lib/rstudio-server \
--bind /nfs/scratch/marquexa/rstudio-run:/var/run/rstudio-server \
--bind ~/rsession.conf:/etc/rstudio/rsession.conf \
/nfs/scratch/marquexa/Lyrics/lyrics_container.sif \
rserver --auth-none=0 --auth-pam-helper-path=pam-helper --www-address=127.0.0.1 --server-user=$USER

# Keep the job running so you can access RStudio Server via SSH tunnel
while true; do sleep 60; done
