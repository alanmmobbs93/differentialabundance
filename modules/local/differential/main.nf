process LIMMA_DIFFERENTIAL {
    tag "$meta"
    label 'process_single'

    conda "${moduleDir}/environment.yml"
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'oras://community.wave.seqera.io/library/bioconductor-edger_bioconductor-ihw_bioconductor-limma_r-dplyr_r-readr:7fc48564d286c1c6' :
        'community.wave.seqera.io/library/bioconductor-edger_bioconductor-ihw_bioconductor-limma_r-dplyr_r-readr:edea0f9fbaeba3a0' }"
    debug true

    input:
    tuple val(meta), val(contrast_variable), val(reference), val(target)
    tuple val(meta2), path(samplesheet), path(intensities)
    val type

    output:
    tuple val(meta), path("*.limma.results.tsv")          , emit: results
    tuple val(meta), path("*.limma.mean_difference.png")  , emit: md_plot
    tuple val(meta), path("*.MArrayLM.limma.rds")         , emit: rdata
    tuple val(meta), path("*.normalised_counts.tsv")      , emit: normalised_counts, optional: true
    tuple val(meta), path("*.limma.model.txt")            , emit: model
    tuple val(meta), path("*.R_sessionInfo.log")          , emit: session_info
    path "versions.yml"                                   , emit: versions

    when:
    task.ext.when == null || task.ext.when

    script:
    template 'limma_de_rnaseq.R'
}
