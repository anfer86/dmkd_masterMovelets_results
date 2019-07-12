To run an experiment using Slurm:

sbatch masterMovelets gowalla/run1 16 descriptions/gowalla.json -1 10

Please, see the file masterMovelets for more details about the arguments.

MasterMovelets generates the movelets by class, so you need to merge all the movelets in a unique dataset

sbatch marge_datasets <dir>

where <dir> is the directory where the movelets were extracted. For instance <dir> ../results/gowalla/run1/MasterMovelets/gowalla_ED/mnf_-1__q_LSP__ms_1__Ms_10/

this script will generate two files: train.csv and test.csv.

Then you need to build models using

sbatch buildModel <dir>

where <dir> is the same directory used for merge_datasets
