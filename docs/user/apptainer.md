# Running popgenVCF with Apptainer

popgenVCF includes a native `Apptainer.def` for Linux clusters where Docker is unavailable or prohibited.

## Build the image

From the repository root:

```bash
sudo apptainer build popgenvcf.sif Apptainer.def
```

On systems configured for unprivileged builds, `--fakeroot` may be used instead of `sudo`:

```bash
apptainer build --fakeroot popgenvcf.sif Apptainer.def
```

## Run an analysis

Bind the analysis directory to `/data` and use the regular command-line interface:

```bash
apptainer run \
  --bind "$PWD:/data" \
  popgenvcf.sif \
  --config /data/analysis.yml
```

Input and output paths in the configuration must refer to paths visible inside the container, normally under `/data`.

## Generate a configuration

```bash
apptainer run \
  --bind "$PWD:/data" \
  popgenvcf.sif \
  --write-config /data/analysis.yml
```

## Run R directly

```bash
apptainer exec popgenvcf.sif \
  Rscript -e 'cat(as.character(packageVersion("popgenVCF")), "\n")'
```

## Verify the image

The definition includes an Apptainer `%test` section:

```bash
apptainer test popgenvcf.sif
```

CI builds the SIF image from the repository definition, executes the package and CLI smoke tests, and reruns both scientific validation suites.

## Use the published OCI image instead

Apptainer can also consume a released GHCR image without rebuilding the native definition:

```bash
apptainer pull popgenvcf.sif \
  docker://ghcr.io/duceppemo/popgenvcf:0.9.0
```

For archival work, replace the version tag with the immutable digest attached to the corresponding GitHub Release.
