FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libtiff5-dev libjpeg-dev pandoc build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY . /src/popgenVCF
WORKDIR /src
RUN Rscript -e 'install.packages("BiocManager", repos="https://cloud.r-project.org")' \
 && Rscript -e 'BiocManager::install(c("SNPRelate","gdsfmt"), ask=FALSE, update=FALSE)' \
 && Rscript -e 'install.packages(c("remotes"), repos="https://cloud.r-project.org")' \
 && Rscript -e 'remotes::install_local("/src/popgenVCF", dependencies=TRUE, upgrade="never")'

ENTRYPOINT ["Rscript", "/usr/local/lib/R/site-library/popgenVCF/scripts/popgenVCF.R"]
CMD ["--help"]
