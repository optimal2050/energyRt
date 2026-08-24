# =============================================================================#
# paths_rebase.R — path rebasing on load + the layout upgrade tool
# (storage layout 3, stage S4b).
#
# Saved objects record where their parquet stores were AT SAVE TIME, relative
# to the then-working directory. Instead of trusting those strings,
# load_scenario() rebuilds every stored path deterministically from the
# folder actually being loaded — so a moved or renamed scenario folder, or a
# different getwd(), just works.
# =============================================================================#

# Rebuild every stored path of a loaded scenario from `root` (the directory
# the scenario was loaded from). Layout-aware: the solution store lives under
# the active run (layout 3) or at the top level (layout 2 / pre-run saves).
.scenario_rebase_paths <- function(scen, root) {
  root <- gsub("[\\/]+", "/", root)
  old_root <- gsub("[\\/]+", "/", scen@path %||% "")
  scen@path <- root

  # modInp + each parameter (an own-problem variant's store lives under its
  # variant dir, the base problem's at the scenario level)
  scen@modInp <- .modinp_rebase(scen@modInp, .problem_modinp_root(scen, root))

  # modOut + each variable
  mo_root <- if (nzchar(scen@misc$run %||% "")) {
    fp(.run_dir(scen, .run_variant(scen), scen@misc$run), "modOut")
  } else {
    fp(root, "modOut")
  }
  if (length(get_ondisk_slots(scen@modOut))) {
    scen@modOut@misc$path <- mo_root
  }
  for (nm in names(scen@modOut@variables)) {
    v <- scen@modOut@variables[[nm]]
    if (isS4(v) && length(get_ondisk_slots(v))) {
      v@misc$path <- fp(mo_root, "variables", nm)
      scen@modOut@variables[[nm]] <- v
    }
  }

  # embedded model (a model-store reference is resolved by load_scenario and
  # rebased by load_model)
  if (is.null(scen@model@misc$model_ref)) {
    scen@model <- .model_rebase(scen@model, fp(root, "model"))
  }

  # legacy external/working dirs recorded inside the old scenario folder
  # (layout-2 `script/<solver>` or an old misc$tmp.dir): re-prefix when they
  # pointed inside the folder that moved; leave truly external dirs alone
  for (key in c("solver.dir", "tmp.dir")) {
    d <- scen@misc[[key]]
    if (is.character(d) && length(d) == 1L && nzchar(d) &&
        nzchar(old_root) && !identical(old_root, root)) {
      d <- gsub("[\\/]+", "/", d)
      if (startsWith(d, paste0(old_root, "/"))) {
        scen@misc[[key]] <- fp(root, substring(d, nchar(old_root) + 2L))
      }
    }
  }
  scen
}

#' Upgrade a scenario folder to the current on-disk layout
#'
#' @description
#' Migrates a scenario directory in place to layout 3 with the current run
#' structure. Idempotent — running it on an up-to-date folder changes
#' nothing. It handles, in one pass:
#'
#' * layout 2: each `script/<solver>/` directory (recognized by its `solver`
#'   csv) becomes `runs/<solver>/solver/` with a backfilled `run.yml`
#'   (`status: "legacy"`, solver identity from the csv, timestamps from file
#'   modification times); the top-level `modOut/` store moves under the
#'   active run. Transient timestamp directories are left where they are.
#' * interim layout 3 (early builds): the `runs/default/<solve>/` wrapper is
#'   flattened to `runs/<solve>/`, and each run's `script/` working dir is
#'   renamed to `solver/`.
#'
#' The scenario object is re-saved with rebased paths, `scenario.yml` (with
#' its `default:` run) and the `layout` marker are written, and the project
#' registry is updated.
#'
#' @param path character, the scenario directory.
#' @param verbose logical.
#' @return the upgraded scenario object, invisibly.
#' @export
scenario_upgrade_layout <- function(path, verbose = TRUE) {
  path <- gsub("[\\/]+", "/", path)
  if (!dir.exists(path) || !file.exists(fp(path, "scen.RData"))) {
    stop("'", path, "' is not a saved scenario directory (no scen.RData)")
  }
  say <- function(...) if (verbose) message(...)

  scen <- load_scenario(path, env = NULL, verbose = FALSE)

  .mv <- function(from, to) {
    # rename with a copy fallback (cross-volume moves)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    if (suppressWarnings(file.rename(from, to))) return(invisible(TRUE))
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    ff <- list.files(from, recursive = TRUE, all.files = TRUE,
                     full.names = FALSE, include.dirs = TRUE)
    for (f in ff) {
      src <- fp(from, f)
      dst <- fp(to, f)
      if (dir.exists(src)) {
        dir.create(dst, recursive = TRUE, showWarnings = FALSE)
      } else {
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
      }
    }
    unlink(from, recursive = TRUE, force = TRUE)
    invisible(TRUE)
  }

  # -- interim layout 3: flatten runs/default/<solve>/ -> runs/<solve>/ ------
  interim_root <- fp(path, "runs", "default")
  if (dir.exists(interim_root) &&
      !file.exists(fp(interim_root, "run.yml")) &&
      !file.exists(fp(interim_root, "variant.yml"))) {
    for (sd in list.dirs(interim_root, recursive = FALSE)) {
      target <- fp(path, "runs", basename(sd))
      if (dir.exists(target)) {
        say("Keeping '", basename(sd), "' under runs/default/ (a run of ",
            "that name already exists at the base level)")
        next
      }
      say("Flattening runs/default/", basename(sd), " -> runs/",
          basename(sd))
      .mv(sd, target)
      # fix the record's variant field
      ry <- fp(target, "run.yml")
      if (file.exists(ry)) {
        rec <- tryCatch(yaml::read_yaml(ry), error = function(e) NULL)
        if (!is.null(rec)) {
          rec$variant <- ""
          yaml::write_yaml(rec, ry)
        }
      }
    }
    if (length(list.files(interim_root)) == 0) unlink(interim_root,
                                                      recursive = TRUE)
    if (identical(scen@misc$variant, "default")) scen@misc$variant <- ""
  }

  # -- rename each run's script/ working dir to solver/ ----------------------
  rd <- .run_dirs(scen)
  for (i in seq_len(nrow(rd))) {
    old_sd <- fp(rd$dir[i], "script")
    new_sd <- fp(rd$dir[i], "solver")
    if (dir.exists(old_sd) && !dir.exists(new_sd)) {
      say("Renaming ", .run_id(rd$variant[i], rd$solve[i]),
          "/script -> solver")
      .mv(old_sd, new_sd)
    }
  }

  # -- layout 2: script/<solver>/ -> runs/<solver>/solver/ + run.yml ---------
  legacy_root <- fp(path, "script")
  migrated_active <- NULL
  if (dir.exists(legacy_root)) {
    old_active <- gsub("[\\/]+", "/",
                       scen@misc$solver.dir %||% scen@misc$tmp.dir %||% "")
    for (sd in list.dirs(legacy_root, recursive = FALSE)) {
      sf <- fp(sd, "solver")
      if (!file.exists(sf)) next # transient/unknown dir: leave in place
      lbl <- basename(sd)
      run_dir <- .run_dir(scen, "", lbl)
      if (file.exists(fp(run_dir, "run.yml"))) next # already migrated
      say("Migrating script/", lbl, " -> runs/", lbl, "/solver")
      .mv(sd, fp(run_dir, "solver"))
      sv <- tryCatch(utils::read.csv(sf <- fp(run_dir, "solver", "solver"),
                                     stringsAsFactors = FALSE),
                     error = function(e) NULL)
      getv <- function(k) {
        if (is.null(sv)) return("")
        v <- sv$value[sv$name == k]
        if (length(v)) v[1] else ""
      }
      out_log <- fp(run_dir, "solver", "output", "log.csv")
      mt <- file.info(fp(run_dir, "solver"))$mtime
      rec <- list(
        label = lbl, variant = "", scenario = scen@name,
        solver_name = getv("name"), lang = getv("lang"),
        solver = getv("solver"), cmdline = getv("cmdline"),
        started = format(mt, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        status = if (file.exists(out_log)) "legacy" else "legacy-unsolved",
        modinp = "shared",
        energyRt_version = as.character(utils::packageVersion("energyRt"))
      )
      yaml::write_yaml(rec, fp(run_dir, "run.yml"))
      if (identical(gsub("[\\/]+", "/", fp(path, "script", lbl)),
                    old_active) || is.null(migrated_active)) {
        migrated_active <- lbl
      }
    }
    if (length(list.files(legacy_root)) == 0) unlink(legacy_root,
                                                     recursive = TRUE)
  }

  # -- top-level modOut/ moves under the active run --------------------------
  top_modout <- fp(path, "modOut")
  if (is.null(scen@misc$run %||% NULL) || !nzchar(scen@misc$run %||% "")) {
    if (!is.null(migrated_active)) {
      scen@misc$variant <- ""
      scen@misc$run <- migrated_active
      scen@misc$tmp.dir <- NULL
      scen@misc$solver.dir <- NULL
    }
  }
  if (dir.exists(top_modout) && nzchar(scen@misc$run %||% "")) {
    target <- fp(.run_dir(scen, .run_variant(scen), scen@misc$run), "modOut")
    if (!dir.exists(target)) {
      say("Moving modOut/ under runs/",
          .run_id(.run_variant(scen), scen@misc$run))
      .mv(top_modout, target)
    }
  }

  # -- finalize: rebase paths, then re-save the shell through save_scenario()
  # (which re-applies the sourceCode dedup and model-ref logic that loading
  # undid, and writes layout marker + manifest + registry rows)
  scen <- .scenario_rebase_paths(scen, path)
  scen <- suppressMessages(save_scenario(scen, path = path, verbose = FALSE))
  say("Scenario '", scen@name, "' upgraded to layout ", .SCENARIO_LAYOUT)
  invisible(scen)
}
