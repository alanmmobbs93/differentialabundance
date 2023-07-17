process READ_FROM_SOFT {
    tag "$meta.id"
    label 'process_single'

    conda "bioconda::bioconductor-geoquery=2.66.0"
    container "${ workflow.containerEngine == 'singularity' && !task.ext.singularity_pull_docker_container ?
        'https://depot.galaxyproject.org/singularity/bioconductor-geoquery:2.66.0--r42hdfd78af_0' :
        'quay.io/biocontainers/bioconductor-geoquery:2.66.0--r42hdfd78af_0' }"

    input:
    tuple val(meta), path(samplesheet)

    output:
    tuple val(meta), path("*.rds")                  , emit: rds
    tuple val(meta), path("*matrix.tsv")            , emit: expression
    tuple val(meta), path("*matrix.annotated.tsv")  , emit: expression_annotated
    tuple val(meta), path("*.annotation.tsv")       , emit: annotation
    path "versions.yml"                             , emit: versions

    when:
    task.ext.when == null || task.ext.when

    script:
    template 'read_soft_matrix.R'
}
