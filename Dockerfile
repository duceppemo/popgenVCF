FROM condaforge/miniforge3:25.3.0-3

ARG VCS_REF="unknown"
ARG VERSION="development"

LABEL org.opencontainers.image.title="popgenVCF" \
      org.opencontainers.image.description="Reproducible population-genomics toolkit for diploid biallelic SNP VCF data" \
      org.opencontainers.image.source="https://github.com/duceppemo/popgenVCF" \
      org.opencontainers.image.documentation="https://github.com/duceppemo/popgenVCF#readme" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.version="${VERSION}"

COPY inst/conda/environment.yml /tmp/popgenvcf-environment.yml

RUN mamba env create --file /tmp/popgenvcf-environment.yml && \
    mamba clean --all --yes && \
    rm -f /tmp/popgenvcf-environment.yml

ENV PATH="/opt/conda/envs/popgenvcf/bin:${PATH}" \
    CONDA_DEFAULT_ENV="popgenvcf" \
    R_ENVIRON_USER="/dev/null" \
    R_PROFILE_USER="/dev/null" \
    LC_ALL="C.UTF-8" \
    LANG="C.UTF-8"

WORKDIR /opt/popgenVCF
COPY . /opt/popgenVCF

RUN Rscript inst/scripts/install-bioconductor.R && \
    R CMD INSTALL . && \
    Rscript -e 'stopifnot(as.character(packageVersion("popgenVCF")) == read.dcf("DESCRIPTION")[1, "Version"])' && \
    Rscript -e 'x <- popgenVCF::run_scientific_validation(integration = TRUE, threads = 4); print(x$checks); stopifnot(x$passed)' && \
    Rscript -e 'x <- popgenVCF::run_population_structure_validation(integration = TRUE); print(x$checks); stopifnot(x$passed)'

COPY docker/entrypoint.sh /usr/local/bin/popgenvcf
RUN chmod 0755 /usr/local/bin/popgenvcf && \
    mkdir -p /data && \
    chmod 0777 /data

WORKDIR /data
ENTRYPOINT ["/usr/local/bin/popgenvcf"]
CMD ["--help"]
