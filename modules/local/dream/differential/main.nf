// TODO:
//      should this module take all variables already processed (meta),
//      or should it be able to take the yaml file and parse it on its own?

process DREAM_DIFFERENTIAL {
    tag "${meta.id}"
    label 'process_single'

    conda "${moduleDir}/environment.yml" // TODO: COMPLETE THIS TOO!
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'https://community-cr-prod.seqera.io/docker/registry/v2/blobs/sha256/d6/d6fa8a7908dd357484d63bb574a378f1739440a2c0752ce91b1e0e9d1ac1638c/data' :
        'community.wave.seqera.io/library/bioconductor-edger_bioconductor-variancepartition_r-optparse:ba778938d72f30c5' }"

    input:
    val meta // array with id, [...]
    tuple val(meta2), path(samplesheet), path(intensities)

    output: // TODO: COMPLETE OUTPUTS!
    tuple val(meta), path("*.dream.results.tsv")          , emit: results
    tuple val(meta), path("*.dream.mean_difference.png")  , emit: md_plot
    tuple val(meta), path("*.MArrayMM.dream.rds")         , emit: rdata
    tuple val(meta), path("*.dream.model.txt")            , emit: model
    tuple val(meta), path('*.dream.contrasts_plot.png')   , emit: contrasts_png
    tuple val(meta), path("*.R_sessionInfo.log")          , emit: session_info
    tuple val(meta), path("*.normalised_counts.tsv")      , emit: normalised_counts, optional: true
    path "versions.yml"                                   , emit: versions

    when:
    task.ext.when == null || task.ext.when

    script:
    def prefix                 = task.ext.prefix ?: meta.id
    def blocking_factors       = meta.blocking_factors ? "--blocking_variables '${meta.blocking_factors.join(';')}'" : ''
    def exclude_samples_col    = meta.exclude_samples_col ? "--exclude_samples_col '${meta.exclude_samples_col.join(';')}'" : ''
    def exclude_samples_values = meta.exclude_samples_values ? "--exclude_samples_values '${meta.exclude_samples_values.join(';')}'" : ''
    println("Meta is: ${meta}")
    """
    ## TODO: CHECK WHERE DO `exclude_samples_col` `exclude_samples_values` and `number` OPTIONS COME FROM!!
    dream_de.R  \\
        --output_prefix ${prefix} \\
        --count_file ${intensities} \\
        --sample_file ${samplesheet} \\
        --contrast_variable ${meta.contrast_variable} \\
        --reference_level ${meta.reference} \\
        --target_level ${meta.target} \\
        ${blocking_factors} \\
        ${exclude_samples_col} \\
        ${exclude_samples_values} \\
        --threads ${task.cpus} \\
        --number 100 ## ??

    cat <<-END_VERSIONS > versions.yml
    "${task.process}":
        r-base: \$(echo \$(R --version 2>&1) | sed 's/^.*R version //; s/ .*\$//')
        r-optparse: \$(Rscript -e "library(optparse); cat(as.character(packageVersion('optparse')))")
        r-edgeR: \$(edgeR -e "library(edgeR); cat(as.character(packageVersion('edgeR')))")
        r-variancePartition: \$(Rscript -e "library(variancePartition); cat(as.character(packageVersion('variancePartition')))")
        r-BiocParallel: \$(Rscript -e "library(BiocParallel); cat(as.character(packageVersion('BiocParallel')))")
    END_VERSIONS
    """


    stub: // TODO: COMPLETE STUB!
    prefix = task.ext.prefix   ?: "${meta.id}"
    """
    #!/usr/bin/env Rscript
    library(limma)
    a <- file("${prefix}.dream.results.tsv", "w")
    close(a)
    a <- file("${prefix}.dream.mean_difference.png", "w")
    close(a)
    a <- file("${prefix}.MArrayLM.dream.rds", "w")
    close(a)
    a <- file("${prefix}.normalised_counts.tsv", "w")
    close(a)
    a <- file("${prefix}.dream.model.txt", "w")
    close(a)
    a <- file("${prefix}.R_sessionInfo.log", "w")
    close(a)

    cat <<-END_VERSIONS > versions.yml
    "${task.process}":
        r-base: \$(echo \$(R --version 2>&1) | sed 's/^.*R version //; s/ .*\$//')
        r-optparse: \$(Rscript -e "library(optparse); cat(as.character(packageVersion('optparse')))")
        r-edgeR: \$(edgeR -e "library(edgeR); cat(as.character(packageVersion('edgeR')))")
        r-variancePartition: \$(Rscript -e "library(variancePartition); cat(as.character(packageVersion('variancePartition')))")
        r-BiocParallel: \$(Rscript -e "library(BiocParallel); cat(as.character(packageVersion('BiocParallel')))")
    END_VERSIONS
    """
}
