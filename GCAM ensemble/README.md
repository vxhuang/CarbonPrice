# Run GCAM Queries

## Configuration
1. [Set up the conda environment](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#create-env-from-file) from the `environment.yml` file. I called this `gcam`, but you can call it whatever, just make sure to edit `run_query_xh.pbs` accordingly.
2. Check if any of the environment variables in `gcam-load.sh` need to be edited. They should be based on `$HOME`, but there might be an absolute path somewhere that needs to be changed.
3. You may need to edit some paths within the relevant python files, but I don't think anything related to the location of query files should be absolute (just the location of the GCAM databases, which wouldn't be edited anyway).

## Run

Just submit `run_query_xh.pbs` once you edit the run parameters and so on.