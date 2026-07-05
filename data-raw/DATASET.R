## Source this script to recreate the package's /data files
library(tidyverse)
library(data.table)
source("data-raw/maps.R")

.modelCode <- list(
  GAMS = readLines("gams/energyRt.gms"),
  JuMP = readLines("julia/energyRt.jl"),
  JuMPOutput = readLines("julia/energyRtOutput.jl"),
  PYOMOConcrete = readLines("pyomo/energyRtConcrete.py"),
  PYOMOConcreteOutput = readLines("pyomo/energyRtConcreteOutput.py"),
  PYOMOAbstract = readLines("pyomo/energyRtAbstract.py"),
  PYOMOAbstractOutput = readLines("pyomo/energyRtAbstractOutput.py"),
  GLPK = readLines("glpk/energyRt.mod"),
  GAMS_output = readLines("gams/output.gms"),
  checkGAMS = readLines("gams/check.gms"),
  checkJULIA = readLines("julia/check.jl"),
  checkPYOMO = readLines("pyomo/check.py"),
  checkGLPK = readLines("glpk/check.mod")
)

# data, visible to user ####
# usethis::use_data(utopia_continent, utopia_island,
#                   utopia_honeycomb, utopia_squares,
#                   internal = FALSE, overwrite = TRUE)
usethis::use_data(model_structure, #.modelCode,
                  internal = FALSE, overwrite = TRUE)

# Internal data ####
.defVal <- yaml::read_yaml("data-raw/config_default_values.yml")
.defInt <- yaml::read_yaml(file = "data-raw/config_default_interpolation.yml")
.modInp <- yaml::read_yaml("data-raw/modInp.yml")

readr::write_lines(c(
  "# This file is auto-created from *.yml files inputs,",
  "# see data-raw/DATASETS.R for details.",
  "",
  ".defVal <- ",
  dput(.defVal) |> deparse() |> stringr::str_replace_all(", ", ",\n"),
  "",
  ".defInt <- ",
  dput(.defInt) |> deparse() |> stringr::str_replace_all(", ", ",\n")
  # "",
  # ".modInp <- ",
  # dput(.modInp) |> deparse(),
  ),
  file = "R/defaults.R"
)
styler::style_file("R/defaults.R")

# all names of sets used in parameter@dimSet
.dimSets <- c(
  "horizon", # test
  "tech", "techp", "dem", "sup", "weather", "acomm", "comm", "commp",
  "group", "region", "regionp", "src", "dst",
  "year", "yearp", "slice", "slicep", "stg", "expp", "imp", "trade"
)

.set_dimSets <- .set_set # drop after renaming in gams2x

# DefVal <- .defVal

# Roxygen docs ####
# write/edit .yaml files in data-raw/
yaml_to_df <- function(yaml_content) {

  # Recursive helper function to process slots and extract relevant information
  process_slot <- function(class_name, slot_name, slot_data) {
    result <- list()

    # Extract common attributes: description and type
    description <- slot_data$description
    slot_type <- slot_data$type

    # Handle data.frame type with nested columns
    if (slot_type == "data.frame" && !is.null(slot_data$columns)) {
      # Process columns for data.frame
      for (col_name in names(slot_data$columns)) {
        col_data <- slot_data$columns[[col_name]]
        result[[length(result) + 1]] <- tibble(
          class = class_name,
          slotname = slot_name,
          description = description,
          type = slot_type,
          col.name = col_name,
          col.type = col_data$type,
          col.description = col_data$description
        )
      }
    } else {
      # Non data.frame slot, populate basic fields
      result[[length(result) + 1]] <- tibble(
        class = class_name,
        slotname = slot_name,
        description = description,
        type = slot_type,
        col.name = NA,
        col.type = NA,
        col.description = NA
      )
    }

    return(bind_rows(result))
  }

  # Parse the YAML content using yaml::read_yaml
  parsed_yaml <- yaml::read_yaml(yaml_content)

  # Initialize an empty list to store results
  all_results <- list()

  for (class_name in names(parsed_yaml$class)) {
    class_data <- parsed_yaml$class[[class_name]]

    # Process each slot within the technology
    for (slot_name in names(class_data)) {
      slot_data <- class_data[[slot_name]]

      # Call process_slot for each item
      all_results[[length(all_results) + 1]] <- process_slot(class_name, slot_name, slot_data)
    }
  }

  final_df <- bind_rows(all_results)

  return(final_df)
}

# classes
.classes <- yaml_to_df("data-raw/classes.yml") |> as.data.frame()

# mapping specification (recipe-driven creation of m*/meq* mapping parameters)
# Regenerate the skeleton with: source("data-raw/make_mapping_spec.R"); make_mapping_spec()
.mapping_spec <- yaml::read_yaml("data-raw/mapping_spec.yml")

# .set_set,
usethis::use_data(
  .dimSets,
  .modInp,
  # .defInt,
  # .defVal,
  # DefVal,
  .classes,
  .set_dimSets,
  .set_description,
  .parameter_set,
  .parameter_description,
  .variable_set,
  .variable_description,
  .variable_mapping,
  .equation_mapping,
  .equation_set,
  .equation_description,
  .equation_variable,
  .mapping_spec,
  .modelCode,
  internal = T, overwrite = TRUE,
  compress = "xz"
)
