process POPGENVCF {
    tag "${vcf.simpleName}"
    publishDir params.outdir, mode: 'copy'
    cpus 8
    memory '32 GB'
    time '48h'
    container params.container ?: null

    input:
    path vcf
    path metadata
    path config

    output:
    path 'results/**', emit: results

    script:
    """
    mkdir -p results
    Rscript \$(Rscript -e 'cat(system.file("scripts", "popgenVCF.R", package="popgenVCF"))') \\
      --config ${config} \\
      --vcf ${vcf} \\
      --metadata ${metadata} \\
      --outdir results \\
      --threads ${task.cpus}
    """
}
