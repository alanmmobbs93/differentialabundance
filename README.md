<h1>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/images/nf-core-differentialabundance_logo_dark.png">
    <img alt="nf-core/differentialabundance" src="docs/images/nf-core-differentialabundance_logo_light.png">
  </picture>
</h1>

[![GitHub Actions CI Status](https://github.com/nf-core/differentialabundance/actions/workflows/ci.yml/badge.svg)](https://github.com/nf-core/differentialabundance/actions/workflows/ci.yml)
[![GitHub Actions Linting Status](https://github.com/nf-core/differentialabundance/actions/workflows/linting.yml/badge.svg)](https://github.com/nf-core/differentialabundance/actions/workflows/linting.yml)[![AWS CI](https://img.shields.io/badge/CI%20tests-full%20size-FF9900?labelColor=000000&logo=Amazon%20AWS)](https://nf-co.re/differentialabundance/results)[![Cite with Zenodo](http://img.shields.io/badge/DOI-10.5281/zenodo.7568000-1073c8?labelColor=000000)](https://doi.org/10.5281/zenodo.7568000)
[![nf-test](https://img.shields.io/badge/unit_tests-nf--test-337ab7.svg)](https://www.nf-test.com)

[![Nextflow](https://img.shields.io/badge/nextflow%20DSL2-%E2%89%A524.04.2-23aa62.svg)](https://www.nextflow.io/)
[![run with conda](http://img.shields.io/badge/run%20with-conda-3EB049?labelColor=000000&logo=anaconda)](https://docs.conda.io/en/latest/)
[![run with docker](https://img.shields.io/badge/run%20with-docker-0db7ed?labelColor=000000&logo=docker)](https://www.docker.com/)
[![run with singularity](https://img.shields.io/badge/run%20with-singularity-1d355c.svg?labelColor=000000)](https://sylabs.io/docs/)
[![Launch on Seqera Platform](https://img.shields.io/badge/Launch%20%F0%9F%9A%80-Seqera%20Platform-%234256e7)](https://cloud.seqera.io/launch?pipeline=https://github.com/nf-core/differentialabundance)

[![Get help on Slack](http://img.shields.io/badge/slack-nf--core%20%23differentialabundance-4A154B?labelColor=000000&logo=slack)](https://nfcore.slack.com/channels/differentialabundance)[![Follow on Twitter](http://img.shields.io/badge/twitter-%40nf__core-1DA1F2?labelColor=000000&logo=twitter)](https://twitter.com/nf_core)[![Follow on Mastodon](https://img.shields.io/badge/mastodon-nf__core-6364ff?labelColor=FFFFFF&logo=mastodon)](https://mstdn.science/@nf_core)[![Watch on YouTube](http://img.shields.io/badge/youtube-nf--core-FF0000?labelColor=000000&logo=youtube)](https://www.youtube.com/c/nf-core)

## Introduction

**nf-core/differentialabundance** is a bioinformatics pipeline that can be used to analyse data represented as matrices, comparing groups of observations to generate differential statistics and downstream analyses. The pipeline supports RNA-seq data such as that generated by the nf-core [rnaseq workflow](https://github.com/nf-core/rnaseq), and Affymetrix arrays via .CEL files. Other types of matrix may also work with appropriate changes to parameters, and PRs to support additional specific modalities are welcomed.

The pipeline is built using [Nextflow](https://www.nextflow.io), a workflow tool to run tasks across multiple compute infrastructures in a very portable manner. It uses Docker/Singularity containers making installation trivial and results highly reproducible. The [Nextflow DSL2](https://www.nextflow.io/docs/latest/dsl2.html) implementation of this pipeline uses one container per process which makes it much easier to maintain and update software dependencies. Where possible, these processes have been submitted to and installed from [nf-core/modules](https://github.com/nf-core/modules) in order to make them available to all nf-core pipelines, and to everyone within the Nextflow community!

On release, automated continuous integration tests run the pipeline on a full-sized dataset on the AWS cloud infrastructure. This ensures that the pipeline runs on AWS, has sensible resource allocation defaults set to run on real-world datasets, and permits the persistent storage of results to benchmark between pipeline releases and other analysis sources. The results obtained from the full-sized test can be viewed on the [nf-core website](https://nf-co.re/differentialabundance/results).

## Pipeline summary

![nf-core/differentialabundance metro map](docs/images/workflow.png)

1. Optionally generate a list of genomic feature annotations using the input GTF file (if a table is not explicitly supplied).
2. Cross-check matrices, sample annotations, feature set and contrasts to ensure consistency.
3. Run differential analysis over all contrasts specified.
4. Optionally run a differential gene set analysis.
5. Generate exploratory and differential analysis plots for interpretation.
6. Optionally build and (if specified) deploy a Shiny app for fully interactive mining of results.
7. Build an HTML report based on R markdown, with interactive plots (where possible) and tables.

## Usage

> [!NOTE]
> If you are new to Nextflow and nf-core, please refer to [this page](https://nf-co.re/docs/usage/installation) on how to set-up Nextflow. Make sure to [test your setup](https://nf-co.re/docs/usage/introduction#how-to-run-a-pipeline) with `-profile test` before running the workflow on actual data.

RNA-seq with deseq2:

```bash
 nextflow run nf-core/differentialabundance \
     --input samplesheet.csv \
     --contrasts contrasts.csv \
     --matrix assay_matrix.tsv \
     --gtf mouse.gtf \
     --outdir <OUTDIR>  \
     -profile rnaseq,<docker/singularity/podman/shifter/charliecloud/conda/institute>
```

:::note
If you are using the outputs of the nf-core rnaseq workflow as input here **either**:

- supply the raw count matrices (file names like **gene_counts.tsv**) alongide the transcript length matrix via `--transcript_length_matrix` (rnaseq versions >=3.12.0, preferred)
- **or** supply the **gene_counts_length_scaled.tsv** or **gene_counts_scaled.tsv** matrices.

RNA-seq limma+voom:

```bash
 nextflow run nf-core/differentialabundance \
     --input samplesheet.csv \
     --contrasts contrasts.csv \
     --matrix assay_matrix.tsv \
     --gtf mouse.gtf \
     --outdir <OUTDIR>  \
     -profile rnaseq_limma,<docker/singularity/podman/shifter/charliecloud/conda/institute>
```

:::note
If you are using the outputs of the nf-core rnaseq workflow as input here **either**:

Provide either the **gene_counts_length_scaled.tsv** or **gene_counts_scaled.tsv** matrices. This follows the [recommendation from the tximport documentation](https://bioconductor.org/packages/devel/bioc/vignettes/tximport/inst/doc/tximport.html#limma-voom):

> "Because limma-voom does not use the offset matrix stored in `y$offset`, we recommend using scaled counts generated from abundances, either 'scaledTPM' or 'lengthScaledTPM'."

These matrices, **gene_counts_length_scaled.tsv** or **gene_counts_scaled.tsv**, are generated in the RNA-seq workflow and meet this recommendation by providing appropriately scaled counts for analysis.

See the [usage documentation](https://nf-co.re/differentialabundance/usage) for more information.
:::

Affymetrix microarray:

```bash
 nextflow run nf-core/differentialabundance \
     --input samplesheet.csv \
     --contrasts contrasts.csv \
     --affy_cel_files_archive cel_files.tar \
     --outdir <OUTDIR>  \
     -profile affy,<docker/singularity/podman/shifter/charliecloud/conda/institute>
```

> [!WARNING]
> Please provide pipeline parameters via the CLI or Nextflow `-params-file` option. Custom config files including those provided by the `-c` Nextflow option can be used to provide any configuration _**except for parameters**_; see [docs](https://nf-co.re/docs/usage/getting_started/configuration#custom-configuration-files).

For more details and further functionality, please refer to the [usage documentation](https://nf-co.re/differentialabundance/usage) and the [parameter documentation](https://nf-co.re/differentialabundance/parameters).

### Reporting

The pipeline reports its outcomes in two forms.

#### R markdown and HTML

The primary workflow output is an HTML-format report produced from an [R markdown template](assets/differentialabundance_report.Rmd) (you can also supply your own). This leverages helper functions from [shinyngs](https://github.com/pinin4fjords/shinyngs) to produce rich plots and tables, but does not provide significant interactivity.

![screenshot of the markdown report](docs/images/markdown_report.png "Markdown report")

Additionally, a zip file is produced by the pipeline, containing an R markdown file and all necessary file inputs for reporting. The markdown file is the same as the input template, but with the parameters set appropriately, so that you can run the reporting yourself in RStudio, and add any customisations you need.

#### Shiny-based data mining app

A second optional output is produced by leveraging [shinyngs](https://github.com/pinin4fjords/shinyngs) to build an interactive Shiny application. This allows more interaction with the data, setting of thresholds etc.

![screenshot of the ShinyNGS contrast table](docs/images/shinyngs_contrast_table.png "ShinyNGS contrast table")

![screenshot of the ShinyNGS gene plot](docs/images/shinyngs_gene_plot.png "ShinyNGS gene plot")

By default the application is provided as an R script and associated serialised data structure, which you can use to quickly start the application locally. With proper configuration the app can also be deployed to [shinyapps.io](https://www.shinyapps.io/) - though this requires you to have an account on that service (free tier available).

## Pipeline output

To see the results of an example test run with a full size dataset refer to the [results](https://nf-co.re/differentialabundance/results) tab on the nf-core website pipeline page.
For more details about the output files and reports, please refer to the
[output documentation](https://nf-co.re/differentialabundance/output).

## Credits

nf-core/differentialabundance was originally written by Jonathan Manning ([@pinin4fjords](https://github.com/pinin4fjords)) and Oskar Wacker ([@WackerO](https://github.com/WackerO)). Jonathan Manning (now at Seqera) initially worked on this workflow as an employee of Healx, an AI-powered, patient-inspired tech company, accelerating the discovery and development of treatments for rare diseases. Oskar Wacker works for [QBiC](https://www.qbic.uni-tuebingen.de/) at Tübingen University. We are grateful for the support of open science in this project.

We thank the many members of the nf-core community who assisted with this pipeline, often by reviewing module pull requests including but not limited to:

- [@ggabernet](https://github.com/ggabernet),
- [@SPPearce](https://github.com/SPPearce),
- [@nvnieuwk](https://github.com/nvnieuwk),
- [@jfy133](https://github.com/jfy133),
- [@mahesh-panchal](https://github.com/mahesh-panchal),
- [@mashehu](https://github.com/mashehu),
- [@apeltzer](https://github.com/apeltzer)

## Contributions and Support

If you would like to contribute to this pipeline, please see the [contributing guidelines](.github/CONTRIBUTING.md).

For further information or help, don't hesitate to get in touch on the [Slack `#differentialabundance` channel](https://nfcore.slack.com/channels/differentialabundance) (you can join with [this invite](https://nf-co.re/join/slack)).

## Citations

If you use nf-core/differentialabundance for your analysis, please cite it using the following doi: [10.5281/zenodo.7568000](https://doi.org/10.5281/zenodo.7568000).

An extensive list of references for the tools used by the pipeline can be found in the [`CITATIONS.md`](CITATIONS.md) file.

This pipeline uses code and infrastructure developed and maintained by the [nf-core](https://nf-co.re) community, reused here under the [MIT license](https://github.com/nf-core/tools/blob/master/LICENSE).

You can cite the `nf-core` publication as follows:

> **The nf-core framework for community-curated bioinformatics pipelines.**
>
> Philip Ewels, Alexander Peltzer, Sven Fillinger, Harshil Patel, Johannes Alneberg, Andreas Wilm, Maxime Ulysse Garcia, Paolo Di Tommaso & Sven Nahnsen.
>
> _Nat Biotechnol._ 2020 Feb 13. doi: [10.1038/s41587-020-0439-x](https://dx.doi.org/10.1038/s41587-020-0439-x).
