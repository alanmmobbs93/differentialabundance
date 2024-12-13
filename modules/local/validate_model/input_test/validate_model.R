#!/usr/bin/env Rscript

## TOD: add make contrast arguments

# Load required libraries
suppressWarnings(suppressMessages({
    library(optparse)
    library(yaml)
    library(readr)
    library(dplyr)
}))

# Define command-line arguments
option_list <- list(
    make_option(c("-m", "--models"), type = "character", default = NULL,
                help = "Path to the models.yml file", metavar = "character"),
    make_option(c("-s", "--samplesheet"), type = "character", default = NULL,
                help = "Path to the samplesheet CSV file", metavar = "character")
)

# Parse command-line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Validate input arguments
if (is.null(opt$models) || is.null(opt$samplesheet)) {
    stop("Both --models and --samplesheet arguments are required.", call. = FALSE)
}

# Load models.yml file
tryCatch({
    models <- yaml.load_file(opt$models)
    cat("Loaded models.yml successfully.\n")
}, error = function(e) {
    stop("Error loading models.yml: ", e$message)
})

# Load samplesheet CSV file
tryCatch({
    samplesheet <- read_csv(opt$samplesheet, show_col_types = FALSE)
    cat("Loaded samplesheet CSV successfully.\n")
}, error = function(e) {
    stop("Error loading samplesheet CSV: ", e$message)
})

# --- Step 1: Collect all variables from contrasts in the YAML ---
cat("\n--- Collecting Variables from Models ---\n")
variables <- unique(unlist(lapply(models$models, function(model) {
    unlist(lapply(model$contrasts, function(contrast) contrast$comparison))
})))

cat("Variables in contrasts:", paste(variables, collapse = ", "), "\n")

# --- Step 2: Validate these variables are present as columns in the samplesheet ---
cat("\n--- Validating Variables in Samplesheet ---\n")
missing_columns <- setdiff(variables, colnames(samplesheet))

if (length(missing_columns) > 0) {
    stop("The following required columns are missing in the samplesheet: ",
        paste(missing_columns, collapse = ", "))
} else {
    cat("All required variables are present in the samplesheet.\n")
}

# --- Step 3: Validate components in samplesheet columns ---
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

# --- Step 4: Validate column names for recommended characters ---
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
