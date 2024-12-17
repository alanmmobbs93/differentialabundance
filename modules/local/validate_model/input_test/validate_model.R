#!/usr/bin/env Rscript

## TODO: add make contrast arguments

# Load required libraries
suppressWarnings(suppressMessages({
    library(tidyverse)
    library(optparse)
    library(yaml)
    #library(readr)
    #library(dplyr)
}))

# Define command-line arguments
option_list <- list(
    make_option(c("-y", "--yml"), type = "character", default = NULL,
                help = "Path to the models.yml file", metavar = "character"),
    make_option(c("-s", "--samplesheet"), type = "character", default = NULL,
                help = "Path to the samplesheet CSV file", metavar = "character"),
    make_option(c("-i", "--sample"), type = "character", default = "sample",
                help = "Column that contains sample identificators", metavar = "character")
)

# Parse command-line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Validate input arguments
## Required arguments
if (is.null(opt$models) || is.null(opt$samplesheet) ) {
    stop("'--yml', '--samplesheet' and '--sample' arguments are required.", call. = FALSE)
}

## Collect parameters
path_yml <- "/workspace/differentialabundance/modules/local/validate_model/input_test/contrasts.yml" #opt$models
path_samplesheet <- "/workspace/differentialabundance/results/SRP254919.samplesheet.csv" #opt$samplesheet
sample_column <- "sample" #opt$sample

# Load models.yml file
tryCatch({
    models <- read_yaml(path_yml)
    cat("Loaded YML file successfully.\n")
}, error = function(e) {
    stop("Error loading YML file: ", e$message)
})

# Load samplesheet CSV file
tryCatch({
    samplesheet <- readr::read_csv(path_samplesheet, show_col_types = FALSE)
    if ( !sample_column %in% colnames(samplesheet) ) {
        stop(paste0("Column ", sample_column, " not present in colnames(samplesheet)") )
    }
    cat("Loaded samplesheet successfully.\n")
}, error = function(e) {
    stop("Error loading samplesheet CSV: ", e$message)
})

## Collect all variables and factors from the yml file
var <- list(); contrasts_list <- list(); for (FORMULA in models$models) {

    ## Populate list
    for (CONTRAST in FORMULA$contrasts) {
        ## Get column name: first comparison's element
        name <- CONTRAST$comparison[1]

        ## Get factors
        variables <- CONTRAST$comparison[2:length(CONTRAST$comparison)]

        ## Populate contrasts_list for model validation
        contrasts_list[[ CONTRAST$id ]] <- list(
            "formula" = FORMULA$formula,
            "variable" = name,
            "contrast" = variables,
            "blocking_factors" = CONTRAST$blocking_factors
        )

        ## If the column is already present in the list, add more components to it
        if (name %in% names(var) ) {
            variables <- unique( c(var[[ name ]], CONTRAST$comparison[2:3] ))
            var[[ name ]] <- variables
            cat("Duplicated variable: ", name, ". Adding more factors to it.\n", sep = "")
        ## Default to new variable
        } else {
            cat("Detected variable:", name, "\n")
            var[[ length(var) + 1 ]] <- variables
            names(var)[length(var) ] <- name
        }

        ## Get blocking factors
        blocking <- c()
        if ( !is.null(CONTRAST$blocking_factors) ) {
            cat("Blocking factors detected for variable", name, "\n")
            blocking <- CONTRAST$blocking_factors
            cat(paste(blocking, collapse = ""), "\n")
        }

        ## Add blockig variables
        if ( "blocking_factors" %in% names(var) ) {
            var[[ "blocking_factors" ]] <- unique( c( var[[ "blocking_factors" ]], blocking ))
        } else {
            var[[ "blocking_factors" ]] <- unique( blocking )
        }
    }
}

## Print explicit message
for (INDEX in 1:length(var)) {
    cat("Detected '", names(var)[INDEX], "' variable with ", paste(var[[INDEX]], collapse = " "), " levels.\n", sep = "")
}


## Function to validate the samplesheet against the yml file
validate_model <- function(sample_column, variables, samplesheet) { # variables: list; samplesheet: df

    #variables   <- var
    #samplesheet <- samplesheet
    undesired_chars <- "[^a-zA-Z0-9_]"

    #######################################################
    ##
    ## Function that takes a list of variables (and expected values) from the models.yml file that must be present in the sample sheet.
    ##
    ## The function evaluates that:
    ## * Every variable is present in the sample sheet.
    ## * Every variable has valid name
    ## * There are no NAs
    ## * All expected values are present
    ## * All samples contains a valid level.
    ##
    ## Finally, the function returns
    ## * An error vector containing all errors found.
    ## * A warnings vector
    ##
    ## If the error vector contains values, the script will end with a non-zero status.
    ##
    #######################################################

    ## Initialize errors vector
    errors <- c()

    ## Initialize warning errors
    warnings <- c()

    # Initialize vector to report continuos variables
    continuous <- c()

    ## Check samplesheet names for invalid characters
    df_colnames <- names(samplesheet)
    true_columns <- stringr::str_detect(df_colnames, pattern = regex(undesired_chars))

    if ( sum(true_columns) > 0 ) {
        errors <- c(errors,
            paste0("The following columns contain undesired characters: ", paste(df_colnames[true_columns], collapse = " "))
        )
    }

    ## Check that blocking variables exists and do not contain NAs, if they were specified
    blocking_factors <- c()
    if ( !is.null(variables$blocking_factors) ) {
        blocking_factors <- variables$blocking_factors

        for (VARIABLE in blocking_factors) {
            ## Check that the column exists
            if ( VARIABLE %in% colnames(samplesheet) ) {
                ## Check if there are NAs
                na_rows <- is.na( samplesheet[[ VARIABLE ]])
                if ( sum(na_rows) > 0 ) {
                    errors <- c(errors, paste0("Blocking factor ", VARIABLE, " contains NA/s in the following rows: ", paste(which(na_rows), collapse = " ")))
                }
            } else {
                errors <- c(
                    errors,
                    paste0("Blocking factor ", VARIABLE, " not present in sample sheet. Please check the compatibility between ", basename(path_yml), " and ", basename(path_samplesheet), " files."))
            }

            ## Alert about continuous variables
            if ( is.numeric(samplesheet[[ VARIABLE ]]) ) {
                continuous <- c(continuous, VARIABLE)
            } else {
                paste(VARIABLE, "not continuous")
            }
        }
    }

    ## Check variables and levels specified
    for (VARIABLE in names(variables)[ names(variables) != "blocking_factors" ] ) { ## Exclude blocking factors, they cannot be treated equally here

        ## Check that the column exists
        if ( VARIABLE %in% colnames(samplesheet) ) {

            ## Alert about continuous variables
            if ( is.numeric(samplesheet[[ VARIABLE ]]) ) {
                continuous <- unique(c(continuous, VARIABLE))
            } else {
                paste(VARIABLE, "is not continuous")
            }

            ## Check if there are NAs
            na_rows <- is.na( samplesheet[[ VARIABLE ]] )
            if ( sum(na_rows) > 0 ) {
                errors <- c(errors, paste0("Column ", VARIABLE, " contains NA/s in the following rows: ", paste(which(na_rows), collapse = " ")))
            }

            ## Check that the column data does not contain undesidered characters
            if ( sum( stringr::str_detect(samplesheet[[ VARIABLE ]], pattern = undesired_chars )) > 0 ) {
                errors <- c(errors,
                    paste0("Column ", VARIABLE, " contains undesired characters\n")
                )
            }

            ## Extract the expected levels according to the YML file
            expected_factors_levels <- c( rep(NA, length(variables[[ VARIABLE ]])) )

            ## Assign names
            names(expected_factors_levels) <- variables[[ VARIABLE ]]

            ## Extract the levels from the sample sheet
            obtained_factors_levels <- unique(samplesheet[[ VARIABLE ]])

            ## Check that all expected levels for this variable are effectively present
            for (LEVEL in names(expected_factors_levels) ) {
                expected_factors_levels[ LEVEL ] <- LEVEL %in% obtained_factors_levels
            }

            if( !all(expected_factors_levels) ) {
                errors <- c(
                    errors,
                    paste0(
                        "Missing factor levels for variable '", VARIABLE, "'. ",
                        "Present levels: ", paste( names( expected_factors_levels[ expected_factors_levels ]), collapse = " " ), ". ",
                        "Missing levels: ", paste( names( expected_factors_levels[!expected_factors_levels ]), collapse = " " )
                        )
                    )
            }

            ## Check whether there are levels in the table that are not present in the models definitions
            if (!all( obtained_factors_levels[!is.na(obtained_factors_levels)] %in% names(expected_factors_levels)) ) {
                warnings <- c(warnings,
                paste0( "The following labels are present in the samplesheet but are not part of the models definition and will be excluded: ", paste( obtained_factors_levels[!obtained_factors_levels %in% names(expected_factors_levels) ], collapse = " " ) ))
            }

        ## Report that a column is not even present in the table
        } else {
            errors <- c(
                errors,
                paste0("Column ", VARIABLE, " not present in sample sheet. Please check the compatibility between ", basename(path_yml), " and ", basename(path_samplesheet), " files."))
        }
    }

    if ( !is.null(continuous)) {
        warnings <- c(warnings, paste0("The following continuous variables were detected or coerced into numeric: ", paste(continuous, collapse = "" )))
    }

    if (length(errors) > 0 ) {
        stop(cat("Some errors where found while validating the samplesheet and models definitions:\n", paste(errors, collapse = "\n"), "\n", sep = ""))
    }

    ## Generate validated samplesheet
    tryCatch({
        selected_columns <- c( sample_column, names(variables)[ names(variables) != "blocking_factors" ], blocking_factors)
        pheno_table      <- samplesheet %>% dplyr::select(all_of(selected_columns))
    }, error = function(e) {
        stop("Error generating validated samplesheet: ", e$message)
    })

    return(list( 'pheno_table' = pheno_table, 'errors' = errors, 'warnings' = warnings) )

}

## Collect all outputs from function: errors must be empty, warnings can have messages
phenotable_errors_warnings <- validate_model(sample_column, var, samplesheet)

## Cat warnings messages
cat(phenotable_errors_warnings[[ 3 ]], "\n")

## Get validated sample sheet
pheno_table <- phenotable_errors_warnings[[ 1 ]]

## CHECK THAT THE MODELS ARE FULL RANKED
check_model_contrasts <- function(contrasts_list, colData) {
    for (model_name in names(contrasts_list)) {

        # Extract model components
        model        <- contrasts_list[[model_name]]
        base_formula <- as.formula(model$formula)  # Ensure formula object
        variable     <- model$variable
        contrast     <- model$contrast
        blocking     <- model$blocking_factor  # Extract blocking factor

        # Check if blocking factor is already in the formula
        if (!is.null(blocking)) {
            # Get terms from the formula
            formula_terms <- all.vars(base_formula)

            # If blocking factor is not already in the formula, add it
            if (!(blocking %in% formula_terms)) {
                #cat("\nAdding blocking factor:", blocking, "to the model.\n")
                updated_formula <- as.formula(paste(deparse(base_formula), "+", blocking))
            } else {
                #cat("\nBlocking factor:", blocking, "is already in the formula. Skipping addition.\n")
                updated_formula <- base_formula
            }
        } else {
            updated_formula <- base_formula
        }

        cat("\nChecking:", model_name, "\nFormula:", deparse(updated_formula), "\n")

        # Build the design matrix
        design_matrix <- model.matrix(updated_formula, data = colData)
        cat("Design matrix:\n")
        print(design_matrix)

        # Check the rank of the design matrix
        rank <- qr(design_matrix)$rank
        expected_rank <- ncol(design_matrix)

        if (rank == expected_rank) {
            cat("The design matrix is full rank.\n")
        } else {
            cat("WARNING: The design matrix is NOT full rank!\n")
        }

        # Check if contrast variable exists in colData
        if (variable %in% colnames(colData)) {
            factor_levels <- levels(factor(colData[[variable]], levels = contrast))
            if (all(contrast %in% factor_levels)) {
                cat("Contrast levels are valid:", paste(contrast, collapse = " vs "), "\n")
            } else {
                cat("ERROR: Contrast levels", paste(contrast, collapse = " vs "),
                    "do not exist in", variable, "\n")
            }
        } else {
            cat("ERROR: Contrast variable", variable, "is not in colData.\n")
        }
    }
}

check_model_contrasts(contrasts_list, pheno_table)
