# =========================================================================== #
# make_mapping_spec.R
#
# Generates a SKELETON `data-raw/mapping_spec.yml` describing every mapping
# parameter (`m*` / `meq*`) used by the model.
#
# The skeleton is produced automatically from the authoritative model
# structure in `data-raw/maps.R` (variable/equation -> mapping conditions) and
# `data-raw/modInp.yml` (declared parameter dimensions). The auto-filled fields
# (`name`, `dims`, `gates_var`, `gates_eq`) must NOT be edited by hand - re-run
# this script to refresh them. The annotation fields
# (`recipe`, `source`, `predicate`, `join_keys`, `filter_by`, `depends_on`)
# are filled in MANUALLY after generation and are preserved on re-runs.
#
# Usage (from package root):
#   source("data-raw/make_mapping_spec.R")
#   make_mapping_spec()
# =========================================================================== #

library(yaml)

# --- helpers --------------------------------------------------------------- #

# Parse a "name( d1 , d2 , ... )" token into list(name=, dims=).
# Returns NULL if no parenthesised argument list is present (scalars).
.parse_mapping_token <- function(token) {
  token <- trimws(token)
  if (!nzchar(token)) {
    return(NULL)
  }
  m <- regmatches(token, regexec("^([A-Za-z0-9_]+)\\s*\\(([^)]*)\\)\\s*$", token))[[1]]
  if (length(m) != 3) {
    # No dims (e.g. a bare scalar variable like vObjective)
    nm <- regmatches(token, regexec("^([A-Za-z0-9_]+)\\s*$", token))[[1]]
    if (length(nm) == 2) {
      return(list(name = nm[2], dims = character(0)))
    }
    return(NULL)
  }
  dims <- strsplit(m[3], ",")[[1]] |> trimws()
  dims <- dims[nzchar(dims)]
  list(name = m[2], dims = dims)
}

# From a `.variable_mapping` value "v(..) $ m(..)" return list(var=, map=).
# When there is no `$ m(..)` condition (e.g. "vObjective"), map is NULL.
.parse_variable_mapping <- function(var_name, value) {
  parts <- strsplit(value, "\\$", perl = TRUE)[[1]]
  var_tok <- .parse_mapping_token(parts[1])
  var <- if (is.null(var_tok)) var_name else var_tok$name
  map <- if (length(parts) >= 2) .parse_mapping_token(parts[2]) else NULL
  list(var = var, map = map)
}

# --- core ------------------------------------------------------------------ #

build_mapping_spec_skeleton <- function(
    maps_file = "data-raw/maps.R",
    modinp_file = "data-raw/modInp.yml") {
  # Source the auto-generated model structure (defines the `.variable_mapping`
  # etc. objects into a dedicated environment).
  env <- new.env()
  sys.source(maps_file, envir = env)

  variable_mapping <- get(".variable_mapping", envir = env)
  equation_mapping <- get(".equation_mapping", envir = env)

  modinp <- yaml::read_yaml(modinp_file)

  # Collect mapping entries keyed by mapping name.
  specs <- list()

  add_dims <- function(name, dims) {
    if (is.null(specs[[name]])) {
      specs[[name]] <<- list(
        name = name,
        dims = dims,
        gates_var = character(0),
        gates_eq = character(0)
      )
    } else if (length(specs[[name]]$dims) == 0 && length(dims) > 0) {
      specs[[name]]$dims <<- dims
    }
  }

  # 1) From variable -> mapping conditions.
  for (vn in names(variable_mapping)) {
    pm <- .parse_variable_mapping(vn, variable_mapping[[vn]])
    if (is.null(pm$map)) next
    add_dims(pm$map$name, pm$map$dims)
    specs[[pm$map$name]]$gates_var <-
      union(specs[[pm$map$name]]$gates_var, pm$var)
  }

  # 2) From equation -> mapping conditions.
  for (en in names(equation_mapping)) {
    tok <- .parse_mapping_token(equation_mapping[[en]])
    if (is.null(tok) || !nzchar(tok$name)) next
    add_dims(tok$name, tok$dims)
    specs[[tok$name]]$gates_eq <-
      union(specs[[tok$name]]$gates_eq, en)
  }

  # 3) Backfill dims from modInp.yml for any mapping declared there, and flag
  #    declared-but-unreferenced mappings.
  declared_maps <- names(modinp)[grepl("^m", names(modinp))]
  for (mn in declared_maps) {
    entry <- modinp[[mn]]
    if (!is.null(entry$type) && entry$type != "map") next
    dims <- entry$dimSets %||% character(0)
    if (is.null(specs[[mn]])) {
      add_dims(mn, dims)
      specs[[mn]]$unreferenced <- TRUE
    } else if (length(specs[[mn]]$dims) == 0) {
      specs[[mn]]$dims <- dims
    }
  }

  specs
}

# Merge previously hand-annotated values back onto a freshly generated
# skeleton so manual edits are not lost when regenerating.
.merge_annotations <- function(skeleton, existing) {
  annot_fields <- c("recipe", "source", "predicate",
                    "join_keys", "filter_by", "depends_on", "notes",
                    "deprecated")
  for (nm in names(skeleton)) {
    if (!is.null(existing[[nm]])) {
      for (f in annot_fields) {
        if (!is.null(existing[[nm]][[f]])) {
          skeleton[[nm]][[f]] <- existing[[nm]][[f]]
        }
      }
    }
  }
  skeleton
}

# Add blank annotation fields (TODO markers) for any mapping that lacks them.
.add_blank_annotations <- function(specs) {
  for (nm in names(specs)) {
    s <- specs[[nm]]
    if (is.null(s$recipe)) s$recipe <- "TODO" # 1..6
    if (is.null(s$source)) s$source <- "TODO" # object_slot|param|calendar|constraint|derived
    if (is.null(s$predicate)) s$predicate <- NA # e.g. "!is.na(value)"
    if (is.null(s$join_keys)) s$join_keys <- list()
    if (is.null(s$filter_by)) s$filter_by <- list()
    if (is.null(s$depends_on)) s$depends_on <- list()
    specs[[nm]] <- s
  }
  specs
}

`%||%` <- function(a, b) if (is.null(a)) b else a

make_mapping_spec <- function(
    maps_file = "data-raw/maps.R",
    modinp_file = "data-raw/modInp.yml",
    annotations_file = "data-raw/mapping_annotations.R",
    out_file = "data-raw/mapping_spec.yml") {
  skeleton <- build_mapping_spec_skeleton(maps_file, modinp_file)

  skeleton <- .add_blank_annotations(skeleton)

  # Apply the reproducible manual annotations (recipe / source / predicate /
  # filter_by / depends_on) from `mapping_annotations.R`.
  if (file.exists(annotations_file)) {
    ann_env <- new.env()
    sys.source(annotations_file, envir = ann_env)
    skeleton <- ann_env$annotate_specs(skeleton)

    unclassified <- names(Filter(
      function(s) identical(s$recipe, "UNCLASSIFIED"), skeleton
    ))
    if (length(unclassified) > 0) {
      warning("Mappings with no recipe assigned in mapping_annotations.R:\n  ",
              paste(unclassified, collapse = ", "))
    }
  }

  # Stable ordering by mapping name for readable diffs.
  skeleton <- skeleton[order(names(skeleton))]

  header <- paste(
    "# AUTO-GENERATED SKELETON by data-raw/make_mapping_spec.R",
    "# Auto fields (name, dims, gates_var, gates_eq) are refreshed on re-run.",
    "# Annotation fields (recipe, source, predicate, join_keys, filter_by,",
    "# depends_on) are filled MANUALLY and preserved across re-runs.",
    sep = "\n"
  )
  yml <- yaml::as.yaml(skeleton)
  writeLines(paste0(header, "\n", yml), out_file)

  # Report unreferenced mappings (declared in modInp.yml but not gating
  # any variable or equation).
  unref <- names(Filter(function(s) isTRUE(s$unreferenced), skeleton))
  if (length(unref) > 0) {
    message("Mappings declared in modInp.yml but not referenced in maps.R:\n  ",
            paste(unref, collapse = ", "))
  }
  message("Wrote ", length(skeleton), " mapping specs to ", out_file)
  invisible(skeleton)
}
