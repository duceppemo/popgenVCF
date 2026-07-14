nextflow.enable.dsl=2

params.vcf = null
params.metadata = null
params.config = null
params.outdir = 'popgen_results'
params.container = null

include { POPGENVCF } from './modules/popgenvcf'

workflow {
    if (!params.vcf || !params.metadata || !params.config) {
        error 'Required parameters: --vcf, --metadata, --config'
    }

    vcf_ch = Channel.fromPath(params.vcf, checkIfExists: true)
    metadata_ch = Channel.fromPath(params.metadata, checkIfExists: true)
    config_ch = Channel.fromPath(params.config, checkIfExists: true)

    POPGENVCF(vcf_ch, metadata_ch, config_ch)
}
