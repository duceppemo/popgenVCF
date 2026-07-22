# Running popgenVCF with Apptainer

popgenVCF includes a native `Apptainer.def` for Linux clusters where Docker is unavailable or prohibited. A released GHCR image can also be converted to SIF without rebuilding the software stack from source.

## Preferred reproducible route: pull a digest

Set the released OCI identity:

```bash
export POPGENVCF_IMAGE="ghcr.io/duceppemo/popgenvcf@sha256:<digest>"
```

Then create a local SIF:

```bash
apptainer pull popgenvcf.sif \
  "docker://${POPGENVCF_IMAGE}"
sha256sum popgenvcf.sif > popgenvcf.sif.sha256
```

Record both the OCI digest and SIF checksum. The conversion creates a new artifact, so the OCI digest alone does not identify the SIF bytes.

## Build from the repository definition

From the exact source revision:

```bash
sudo apptainer build popgenvcf.sif Apptainer.def
```

On systems configured for unprivileged builds:

```bash
apptainer build --fakeroot popgenvcf.sif Apptainer.def
```

Record the source commit and all retrieved dependency identities. Prefer a published digest for long-term archives because future upstream package resolution can affect a source rebuild.

## Generate a configuration

```bash
apptainer run --cleanenv \
  --bind "$PWD:/data" \
  popgenvcf.sif \
  --write-config /data/analysis.yml
```

## Run an analysis

```bash
apptainer run --cleanenv \
  --bind "$PWD:/data" \
  popgenvcf.sif \
  --config /data/analysis.yml
```

Input and output paths in the configuration must refer to paths visible inside the container, normally under `/data`. Bind reference, executable, or scratch directories explicitly when the configuration uses them.

## Slurm example

```bash
#!/usr/bin/env bash
#SBATCH --job-name=popgenvcf
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=08:00:00
#SBATCH --output=logs/popgenvcf-%j.out

set -euo pipefail
module load apptainer
mkdir -p logs

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export TMPDIR="${SLURM_TMPDIR:-$PWD/tmp}"
mkdir -p "$TMPDIR"

apptainer run --cleanenv \
  --bind "$PWD:/data" \
  --bind "$TMPDIR:/tmp" \
  popgenvcf.sif \
  --config /data/analysis.yml
```

Set `compute.threads` no higher than `SLURM_CPUS_PER_TASK`. Retain the scheduler log and accounting information with the result archive.

## File visibility and permissions

Some clusters disable implicit home or current-directory binds. Use explicit `--bind` arguments and container-visible paths. Keep final results on persistent storage and use job-local scratch only for caches and temporary files.

Avoid broad permission changes to genomic data. Apptainer normally runs as the invoking user, so persistent output should retain the user's ownership.

## Run R directly

```bash
apptainer exec --cleanenv popgenvcf.sif \
  Rscript -e 'cat(as.character(packageVersion("popgenVCF")), "\n")'
```

## Verify the image

The native definition includes an Apptainer `%test` section:

```bash
apptainer test popgenvcf.sif
```

CI builds the SIF image, executes package and CLI smoke tests, and reruns deterministic scientific validation. These checks validate the software artifact, not a user's sample definitions, input quality, or biological interpretation.

## Network-restricted systems

Pull and verify the image on an approved networked host, transfer it according to institutional policy, and run the local SIF offline. Materialize remote datasets and optional external software before the job starts, preserving checksums, licenses, versions, and approval state.
