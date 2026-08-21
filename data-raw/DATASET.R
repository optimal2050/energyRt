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
# NOTE: saved further below, after `.variable_set` is corrected -- the `dim`
# column of its variable rows is derived from it.

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

# all names of sets used in parameter@dimSet and variable@dimSets
.dimSets <- c(
  "horizon", # test
  "tech", "techp", "dem", "sup", "weather", "acomm", "comm", "commp",
  "group", "region", "regionp", "src", "dst",
  "year", "yearp", "timeslice", "timeslicep", "stg", "expp", "imp", "trade",
  "costs" # user-cost object names; the `vUserCosts` dimension
)

# `.variable_set` (data-raw/maps.R) derives dims from `.variable_mapping` by
# stripping everything outside the parentheses. A SCALAR variable has no
# parentheses, so both gsubs are no-ops and the "dimension" comes back as the
# variable's own name -- `.variable_set$vObjective == "vObjective"`. Corrected
# here rather than in maps.R, which gams2x overwrites.
.variable_set <- lapply(.variable_mapping, function(x) {
  if (!grepl("(", x, fixed = TRUE)) return(character(0))
  strsplit(gsub("[ ]*[)].*$", "", gsub("^[^(]*[(][ ]*", "", x)),
           "[ ]*[,][ ]*")[[1]]
})
stopifnot(all(unlist(.variable_set) %in% .dimSets))

# `model_structure` (built in maps.R) inherits the same phantom in its variable
# `dim` column. It also has a worse problem: it takes `name` from
# `.variable_description` but `dim`/`map` positionally from `.variable_set` /
# `.variable_mapping`, and those two carry the same 69 names in a DIFFERENT
# ORDER -- so every variable's dims and gating map were attached to the wrong
# variable. Rebuilt here by name, and saved after the correction.
local({
  ii <- model_structure$type == "variable"
  nms <- model_structure$name[ii]
  model_structure$dim[ii] <<- vapply(
    .variable_set[nms], function(x) paste(x, collapse = ", "), character(1))
  model_structure$map[ii] <<- vapply(nms, function(n) {
    x <- .variable_mapping[[n]]
    if (!grepl("$", x, fixed = TRUE)) return("")
    gsub("[,]", ", ", gsub("[ ]", "", sub(".*[$]", "", x)))
  }, character(1))
})
usethis::use_data(model_structure, internal = FALSE, overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# Variable facts that live ONLY in the solver templates
#
# Two things about a variable exist nowhere in `.variable_mapping`: the column
# names it is actually written out with, and whether it is declared positive or
# free. Both are parsed from the GLPK template (the reference writer) rather
# than hand-maintained, so they cannot drift from the model.
#
# The emitted header is the reason this matters: `.variable_set$vTradeIr` says
# `region, region` while the writer emits `src, dst`, and `vTechRetiredNewCap`
# is `year, year` against an emitted `year, yearp`. Those two namings have never
# been reconciled; the cross-check below turns any further divergence into a
# build error.
# ---------------------------------------------------------------------------- #

.variable_colnames <- local({
  ln <- .modelCode$GLPK
  # printf "tech,region,year,value\n" > "output/vTechCap.csv";
  i <- grep('^printf "[^"]*value\\\\n" > "output/v[A-Za-z0-9]+[.]csv";$', ln)
  hd <- sub('^printf "([^"]*)\\\\n".*$', "\\1", ln[i])
  nm <- sub('^.*output/(v[A-Za-z0-9]+)[.]csv.*$', "\\1", ln[i])
  out <- lapply(strsplit(hd, ",", fixed = TRUE), setdiff, "value")
  names(out) <- nm
  # printf "value\n%s\n",vObjective > "output/vObjective.csv";  -- the scalar
  j <- grep('^printf "value\\\\n%s\\\\n",[A-Za-z0-9]+ > "output/v[A-Za-z0-9]+[.]csv";$', ln)
  for (k in j) out[[sub('^.*output/(v[A-Za-z0-9]+)[.]csv.*$', "\\1", ln[k])]] <-
    character(0)
  out
})

.variable_positive <- local({
  ln <- grep("^var v[A-Za-z0-9]+", .modelCode$GLPK, value = TRUE)
  nm <- sub("^var (v[A-Za-z0-9]+).*$", "\\1", ln)
  stats::setNames(grepl(">=[ ]*0[ ]*;$", ln), nm)
})

# Every declared variable must be accounted for in both, and the emitted header
# must line up with the model dims -- same arity, and each column either the dim
# itself or a known alias of it.
local({
  .alias <- list(year = "yearp", region = c("src", "dst", "regionp"),
                 comm = c("commp", "acomm"), timeslice = "timeslicep", tech = "techp")
  miss <- setdiff(names(.variable_mapping), names(.variable_colnames))
  if (length(miss)) {
    stop("No output header found in the GLPK template for: ",
         paste(miss, collapse = ", "))
  }
  miss <- setdiff(names(.variable_mapping), names(.variable_positive))
  if (length(miss)) {
    stop("No `var` declaration found in the GLPK template for: ",
         paste(miss, collapse = ", "))
  }
  for (v in names(.variable_mapping)) {
    d <- .variable_set[[v]]
    cn <- .variable_colnames[[v]]
    if (length(d) != length(cn)) {
      stop("Variable ", v, ": the GLPK output header has ", length(cn),
           " column(s) (", paste(cn, collapse = ", "), ") but the model ",
           "declares ", length(d), " dimension(s) (",
           paste(d, collapse = ", "), ").")
    }
    if (length(d) == 0) next # scalar variable: nothing to line up
    ok <- cn == d | mapply(function(a, b) b %in% .alias[[a]], d, cn,
                           SIMPLIFY = TRUE, USE.NAMES = FALSE)
    if (!all(ok)) {
      stop("Variable ", v, ": output column(s) ",
           paste(cn[!ok], collapse = ", "), " are neither the model dimension ",
           paste(d[!ok], collapse = ", "), " nor a known alias of it.")
    }
  }
})

# ---------------------------------------------------------------------------- #
# `.variables` -- the composed variable specification
#
# The counterpart of `.modInp` for parameters: a plain named list that
# `modOut`'s initialize turns into `variable` objects. Composed from the
# generated tables plus the `variables.yml` overlay, and the overlay is required
# to be TOTAL so it cannot quietly fall behind the model.
# ---------------------------------------------------------------------------- #

.variables <- local({
  ov <- yaml::read_yaml("data-raw/variables.yml")

  computed <- names(ov)[vapply(ov, function(x)
    identical(x$origin, "computed"), logical(1))]
  extra <- setdiff(names(ov), c(names(.variable_mapping), computed))
  if (length(extra)) {
    stop("data-raw/variables.yml has entries that are not declared in the ",
         "model and are not marked `origin: computed`: ",
         paste(extra, collapse = ", "))
  }
  missed <- setdiff(names(.variable_mapping), names(ov))
  if (length(missed)) {
    stop("data-raw/variables.yml is missing an entry for: ",
         paste(missed, collapse = ", "))
  }

  roles <- c("source", "sink", "flow", "activity", "balance", "stock",
             "capacity", "cost")
  units <- c("commodity", "activity", "capacity", "currency")

  out <- lapply(names(ov), function(v) {
    o <- ov[[v]]
    if (!o$role %in% roles) {
      stop("Variable ", v, ": unknown role '", o$role, "'. One of: ",
           paste(roles, collapse = ", "))
    }
    if (!o$unit %in% units) {
      stop("Variable ", v, ": unknown unit '", o$unit, "'. One of: ",
           paste(units, collapse = ", "))
    }
    computed_here <- identical(o$origin, "computed")
    dims <- if (computed_here) as.character(o$dimSets) else .variable_set[[v]]
    list(
      name = v,
      # `desc`: the generated description, except for the computed variables,
      # which no generated table knows about.
      desc = if (computed_here) o$desc else
        as.character(.variable_description[[v]]),
      # RAW model dims, duplicates preserved. Byte-identical to `.variable_set`
      # by construction -- `class-costs.R` / `class-constraint.R` consume it
      # positionally against the `.variable_mapping` string.
      dimSets = dims,
      # The column names the variable is actually written out with; differs from
      # `dimSets` only where a dim repeats (src/dst, year/yearp).
      colNames = if (computed_here) dims else .variable_colnames[[v]],
      # The `$`-condition gating map, "" when the variable is unconditional.
      gate = if (computed_here) "" else {
        x <- .variable_mapping[[v]]
        if (!grepl("$", x, fixed = TRUE)) "" else
          sub("[ ]*[(].*$", "", trimws(sub(".*[$]", "", x)))
      },
      role = o$role,
      unit = o$unit,
      positive = if (computed_here) FALSE else
        unname(.variable_positive[[v]]),
      origin = if (computed_here) "computed" else "solver"
    )
  })
  names(out) <- names(ov)
  out
})

# The computed variables may use dimensions the solver never sees.
stopifnot(all(unlist(lapply(.variables, `[[`, "dimSets")) %in% .dimSets))

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
  .variables,
  .equation_mapping,
  .equation_set,
  .equation_description,
  .equation_variable,
  .mapping_spec,
  .modelCode,
  internal = T, overwrite = TRUE,
  compress = "xz"
)
