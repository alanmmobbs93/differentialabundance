#!/usr/bin/env Rscript

## TODO: add make contrast arguments

# Load required libraries
suppressWarnings(suppressMessages({
    library(optparse)
    library(yaml)
    library(readr)
    library(dplyr)
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
    cat("Loaded models.yml successfully.\n")
}, error = function(e) {
    stop("Error loading models.yml: ", e$message)
})

# Load samplesheet CSV file
tryCatch({
    samplesheet <- readr::read_csv(path_samplesheet, show_col_types = FALSE)
    cat("Loaded samplesheet successfully.\n")
}, error = function(e) {
    stop("Error loading samplesheet CSV: ", e$message)
})

## Collect all variables and factors from the yml file
var <- list(); for (FORMULA in models$models) {
    ## Populate list
    for (CONTRAST in FORMULA$contrasts) {
        ## Get column name: first comparison's element
        name <- CONTRAST$comparison[1]

        ## Get factors
        variables <- CONTRAST$comparison[2:3]

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
validate_model <- function(variables, samplesheet) { # variables: list; samplesheet: df

    variables   <- var
    samplesheet <- samplesheet

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
    true_columns <- stringr::str_detect(df_colnames, pattern = undesired_chars)

    if ( sum(true_columnms) > 0 ) {
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
                na_rows <- which(is.na( samplesheet[[ VARIABLE ]]) )
                if ( na_rows > 0 ) {
                    errors <- c(errors, paste0("Blocking factor ", VARIABLE, " contains NA/s in the following rows: ", paste(na_rows, collapse = " ")))
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
            na_rows <- which(is.na( samplesheet[[ VARIABLE ]]) )
            if ( na_rows > 0 ) {
                errors <- c(errors, paste0("Column ", VARIABLE, " contains NA/s in the following rows: ", paste(na_rows, collapse = " ")))
            }

            ## Check that the column data does not contain undesidered characters
            if (stringr::str_detect(samplesheet[[ VARIABLE ]], pattern = undesired_chars )) {
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

        ## Check that the column name is valid
        ## Check that the column content is valid -> done
        ## Check that the column does not have NAs -> Done
        ## Check that all factors exists -> Done
    }

    if ( !is.null(continuous)) {
        warnings <- c(warnings, paste0("The following continuous variables were detected or coerced into numeric: ", paste(continuous, collapse = "" )))
    }
    return(list( 'errors' = errors, 'warnings' = warnings) )

}

errors_warnings <- validate_model(var, samplesheet)


# Validate that these variables are present as columns in the samplesheet
cat("\n--- Validating Variables in Samplesheet ---\n")
missing_columns <- setdiff(variables, colnames(samplesheet))

if (length(missing_columns) > 0) {
    stop("The following required columns are missing in the samplesheet: ",
        paste(missing_columns, collapse = ", "))
} else {
    cat("All required variables are present in the samplesheet.\n")
}

# Validate components in samplesheet columns
cat("\n--- Validating Column Components ---\n")
for (variable in variables) {
    expected_values <- unique(unlist(lapply(models$models, function(model) {
        unlist(lapply(model$contrasts, function(contrast) {
        if (variable %in% contrast$comparison) return(contrast$comparison)
        }))
    })))
    expected_values <- expected_values[!is.na(expected_values)]  # Remove NAs

    actual_values <- unique(samplesheet[[variable]])
    if (!all(actual_values %in% expected_values)) {
        cat("Column:", variable, "\n")
        cat("Expected values:", paste(expected_values, collapse = ", "), "\n")
        cat("Actual values:", paste(actual_values, collapse = ", "), "\n")
        stop("Mismatch in expected and actual values for column: ", variable)
    } else {
        cat("Column '", variable, "' has valid components.\n", sep = "")
    }
}

# Validate column names for recommended characters
cat("\n--- Checking Column Names for Recommended Characters ---\n")
invalid_columns <- grep("[^a-zA-Z0-9_]", colnames(samplesheet), value = TRUE)

if (length(invalid_columns) > 0) {
    stop("The following column names contain non-recommended characters: ",
        paste(invalid_columns, collapse = ", "))
} else {
    cat("All column names are valid.\n")
}

# --- Step 5: Validate column contents for recommended characters and NAs ---
cat("\n--- Checking Column Contents for Recommended Characters and NAs ---\n")
for (variable in variables) {
    invalid_values <- grep("[^a-zA-Z0-9_]", samplesheet[[variable]], value = TRUE)
    if (any(is.na(samplesheet[[variable]]))) {
        stop("Column '", variable, "' contains missing values (NA).")
    }
    if (length(invalid_values) > 0) {
        stop("Column '", variable, "' contains invalid values: ",
            paste(invalid_values, collapse = ", "))
    } else {
        cat("Column '", variable, "' contents are valid.\n")
    }
}

cat("\nValidation completed successfully.\n")
