# Scientific validation policy

A method is not considered stable until it has deterministic fixture coverage, a trusted independent reference implementation, an explicit tolerance, and a recorded tool version. The bundled synthetic VCF is designed to exercise missingness, monomorphic loci, exact LD duplicates, chromosome boundaries, differentiation, and spatial metadata.

External real datasets are downloaded or supplied locally and are never silently redistributed. Dataset manifests must include a license and SHA-256 checksum.
