#### methods for drawing schematic representation of processes

#' Draw a schematic representation of a process
#'
#' A generic method for drawing a schematic representation of
#' processes-type classes.
#'
#' @param object The object to draw: `technology`, `storage`, `trade`, `demand`, `supply`,
#' `export`, or `import`.
#' @param ... Additional arguments passed to the specific method.
#'
#' @family draw
#' @rdname draw
#' @return displays a schematic representation of the process, returns `NULL`.
#'
#' @export
setGeneric("draw", function(object, ...) standardGeneric("draw"))

## Constants ####
keys <- c(
  "region", "year", "timeslice", "comm", "acomm",
  # "value",
  "lab_par", "lab_txt",
  "tech", "group", "weather", "unit", "io", "parameter"
)

# fixing "no visible binding for global variable" warnings
utils::globalVariables(
  c(
    "value", "parameter", "comm", "acomm", "unit", "weather",
    "lab_par", "lab_txt", "lab_waf", "lab_wcinp", "lab_wafs",
    "lab_wafc", "lab_waf", "lab_wafs", "lab_par", "lab_txt",
    "ioname", "iotype", "group", "waf.fx", "waf.lo", "waf.up",
    "wafs.fx", "wafs.lo", "wafs.up", "wafc.fx", "wafc.lo", "wafc.up",
    "ainp", "aout", "wacinp.fx", "wacinp.lo", "wacinp.up",
    "wacout.fx", "wacout.lo", "wacout.up", "wacact.fx", "wacact.lo",
    "wcinp.fx", "wcinp.lo", "wcinp.up", "wcout.fx", "wcout.lo", "wcout.up",
    "src", "dst", "region", "year", "timeslice",
    "cap2act", "cap2stg", "cap2use",
    "io", "na.omit", "share.lo", "share.up", "share.fx",
    "val_lbl", "where", "ginp2use", "desc", "x", "y"
  )
)

# Arrow label: centred HORIZONTALLY on the arrow, sitting just ABOVE the line,
# over a semi-transparent white pad. The pad separates the label from ghost
# boxes and from neighbouring arrows on a technology with many flows.
#
# `y_line` is the ARROW's own y; this function works out where the text has to
# sit so the PAD's bottom edge clears the line by `gap`. Callers used to pass
# the text centre and guess the offset as `font_in_npc(10) * 0.34` -- but
# `font_in_npc()` returns a device-independent constant while `grobHeight()` is
# device-dependent, so the pad cleared a 7x4.2in figure by ~0.1mm and overlapped
# the line outright on a taller one. Measuring the pad here instead makes the
# clearance real on any device.
# `side` clamps long labels away from the process box: a label is centred
# on the arrow while it fits, but once wider than the arrow span it is
# shifted OUTWARD (left for input arrows, right for output arrows) so its
# inner edge never crosses the box edge at `x1`/`x0`. Overflow at the
# canvas edge is the accepted degradation.
#' @noRd
.draw_arrow_label <- function(label, x0, x1, y_line, fontsize = 10,
                              pad_alpha = 0.75, col = "black",
                              gap = grid::unit(0.8, "mm"), lwd = 2,
                              side = c("centre", "input", "output")) {
  side <- match.arg(side)
  if (length(label) == 0L || is.na(label) || !nzchar(label)) return(invisible(NULL))
  cx <- (x0 + x1) / 2
  tg0 <- grid::textGrob(label, gp = grid::gpar(fontsize = fontsize, col = col))
  pad_h <- grid::grobHeight(tg0) + grid::unit(1.0, "mm")
  # clamp: keep the label's inner edge clear of the box side of the arrow
  w_npc <- grid::convertWidth(grid::grobWidth(tg0) + grid::unit(1.6, "mm"),
                              "npc", valueOnly = TRUE)
  margin <- 0.005
  if (side == "input") {
    # box edge is at x1 (arrow points right into the box)
    inner <- cx + w_npc / 2
    if (inner > x1 - margin) cx <- x1 - margin - w_npc / 2
  } else if (side == "output") {
    # box edge is at x0 (arrow leaves the box to the right)
    inner <- cx - w_npc / 2
    if (inner < x0 + margin) cx <- x0 + margin + w_npc / 2
  }
  # half the arrow's own stroke, so `gap` is measured from the line's EDGE
  half_lwd <- grid::unit(lwd / 96 / 2, "inches")

  y <- grid::unit(y_line, "npc") + half_lwd + gap + 0.5 * pad_h
  grid::grid.rect(
    x = grid::unit(cx, "npc"), y = y,
    width  = grid::grobWidth(tg0) + grid::unit(1.6, "mm"),
    height = pad_h,
    gp = grid::gpar(fill = grDevices::rgb(1, 1, 1, pad_alpha), col = NA)
  )
  grid::grid.text(label, x = grid::unit(cx, "npc"), y = y,
                  just = c("centre", "centre"),
                  gp = grid::gpar(fontsize = fontsize, col = col))
  invisible(NULL)
}

## draw.technology ####
# One technology, one figure. This is the original `draw.technology()` body,
# unchanged except that it now forwards `...` (and therefore `newpage`) to
# `draw_process()`. `draw.technology()` below wraps it to handle vintages.
.draw_technology_one <- function(object, ..., .ghost = FALSE) {
  # browser()
  com_inp <- object@input |>
    mutate(io = "cinp", .before = 1) |>
    rowwise() |>
    mutate(
      lab_txt = make_label(
        comm,
        in_brackets = unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      )
    ) |>
    # add technology parameters
    left_join(object@ceff, by = "comm") |>
    pivot_longer(
      cols = matches("2"), # non-grouped-comm-params have "2" in their names
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!grepl("cact|out", parameter)) |>
    group_by(io, comm) |>
    filter(!is.na(value) | (all(is.na(value)) & row_number() == 1)) |>
    ungroup()

  com_out <- object@output |>
    mutate(io = "cout", .before = 1) |>
    rowwise() |>
    mutate(
      lab_txt = make_label(
        comm,
        in_brackets = unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      )
    ) |>
    # add technology parameters
    left_join(object@ceff, by = "comm") |>
    pivot_longer(
      cols = matches("2"), # non-grouped-comm-params have "2" in their names
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(grepl("cact|out", parameter)) |>
    group_by(io, comm) |>
    filter((!is.na(value)) |
      (all(is.na(value)) & row_number() == 1)) |>
    ungroup()

  com_par <- bind_rows(com_inp, com_out)

  # # add technology parameters
  # com_par <- com_tbl |>
  #   full_join(object@ceff, by = "comm") |>
  #   pivot_longer(
  #     cols = matches("2"), # non-grouped-comm-params have "2" in their names
  #     names_to = "parameter",
  #     values_to = "value"
  #   ) |>
  #   group_by(io, comm) |>
  #   filter(!is.na(value) | (is.na(value) & row_number() == 1))

  # parameter-labels for grouped commodities
  # gcom_par <- com_par |> filter(!is.na(group))
  gcom_inp <- com_inp |> filter(!is.na(group))
  gcom_out <- com_out |> filter(!is.na(group))
  gcom_par <- bind_rows(gcom_inp, gcom_out)


  if (nrow(gcom_par) > 0) {
    gcom_par <- gcom_par |>
      group_by(
        across(
          any_of(c("comm", "acomm", "group", "unit", "io", "parameter"))
        )
      ) |>
      summarise(
        val_lbl = make_label(
          paste0(parameter, ":"),
          # in_brackets = prettyNum(value, digits = 2),
          in_brackets = format_number(value),
          two_lines = if_else(all(grepl("use2cact", parameter)), TRUE, FALSE),
          bracket_type = NULL
        ),
        smin = if_else(all(is.na(share.lo)) & all(is.na(share.fx)),
                       0,
                       min(share.lo, share.fx, Inf, na.rm = TRUE)
        ),
        smax = if_else(all(is.na(share.up)) & all(is.na(share.fx)),
                       1,
                       max(share.up, share.fx, -Inf, na.rm = TRUE)
        ),
        share_lbl = paste0(
          # paste0(round(100 * min(share.lo, share.fx, na.rm = TRUE), 2), "%,",
          #        round(100 * max(share.up, share.fx, na.rm = TRUE), 2), "%")
          # paste0(
          #   min(share.lo, share.fx, na.rm = TRUE), ",",
          #   max(share.up, share.fx, na.rm = TRUE)
          # )
          paste0(
            format_number(smin),
            ",",
            format_number(smax)
          )
        ),
        lab_par = make_label(
          val_lbl,
          in_brackets = if_else(all(grepl("use2cact", parameter)),
            NA,
            share_lbl
          ),
          two_lines = TRUE,
          bracket_type = "square"
        ),
        # lab_use2cact = make_label(
        #   val_lbl,
        #   in_brackets = val_lbl,
        #   two_lines = FALSE
        # ),
        .groups = "drop"
      ) |>
      rowwise() |>
      mutate(
        lab_par = if_else(
          grepl("use2cact", parameter),
          val_lbl,
          lab_par
        ),
        lab_txt = make_label(
          comm,
          in_brackets = unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        )
      ) |>
      select(-val_lbl, -share_lbl) |>
      select(-any_of(c("region", "year", "timeslice")))
    # as.data.table()
    gcom_par$lab_par
    gcom_par
  }

  # parameter-labels for non-grouped commodities
  ccom_par <- com_par |>
    filter(is.na(group)) |>
    group_by(across(
      any_of(keys)
      # any_of(c("io", "comm", "region", "year", "timeslice", "parameter"))
    )) |>
    summarise(
      lab_par = make_label(
        paste0(parameter, ":"),
        in_brackets = value,
        two_lines = FALSE
      ),
      .groups = "drop"
    ) |>
    select(-any_of(c("region", "year", "timeslice")))
  ccom_par

  # auxiliary inputs ####
  aux_tbl <- object@aux |>
    full_join(object@aeff, by = "acomm")

  ainp <- aux_tbl |>
    select(
      # any_of(c("acomm", "comm", "region", "year", "timeslice", "unit")),
      any_of(c(keys)),
      matches("ainp")
    ) |>
    rowwise() |>
    mutate(
      ainp = sum(abs(c_across(matches("ainp"))), na.rm = TRUE)
    ) |>
    filter(ainp != 0) |>
    # drop numeric columns columns with all NAs
    select(-where(~ all(is.na(.)) & is.numeric(.)), -ainp) |>
    mutate(io = "ainp", .before = 1)
  ainp

  # aux outputs ####
  aout <- aux_tbl |>
    select(
      # any_of(c("acomm", "comm", "region", "year", "timeslice", "unit")),
      any_of(c(keys)),
      matches("aout")
    ) |>
    rowwise() |>
    mutate(
      aout = sum(abs(c_across(matches("aout"))), na.rm = TRUE)
    ) |>
    filter(aout != 0) |>
    # drop numeric columns columns with all NAs
    select(-where(~ all(is.na(.)) & is.numeric(.)), -aout) |>
    mutate(io = "aout", .before = 1)
  aout

  # aux combined
  # browser()
  aux <- bind_rows(ainp, aout)
  if (nrow(aux) > 0) {
    aux <- aux |>
      pivot_longer(
        # cols = -any_of(c("io", "acomm", "comm", "region", "year", "timeslice", "unit")),
        cols = -any_of(c(keys)),
        names_to = "parameter",
        values_to = "value"
      ) |>
      filter(!is.na(value)) |>
      group_by(io, acomm, comm, unit, parameter) |>
      summarise(
        lab_par = make_label(
          paste0(parameter, ":"),
          in_brackets = value,
          two_lines = FALSE,
          bracket_type = "square"
        ),
        .groups = "keep"
      ) |>
      mutate(
        lab_txt = make_label(
          acomm,
          in_brackets = unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        ),
        lab_par = if_else(is.na(comm),
          lab_par,
          make_label(lab_par, in_brackets = comm, two_lines = TRUE)
        )
      ) |>
      ungroup()
  }

  aux

  # weather factors
  wea <- object@weather |>
    rowwise() |>
    mutate(
      lab_wafc = if_else(
        !is.na(wafc.fx),
        # create a label with the fixed value, ignore the lo and up values
        make_label("wafc.fx:", in_brackets = wafc.fx, two_lines = FALSE),
        # "A",
        # create a label with the lo and up values
        # NA
        if_else(!is.na(wafc.lo) & !is.na(wafc.up),
          paste0("wafc.lo: ", wafc.lo, "\n", "wafc.up: ", wafc.up),
          if_else(!is.na(wafc.lo),
            # create a label with the lo value
            paste0("wafc.lo: ", wafc.lo),
            # create a label with the up value
            if_else(!is.na(wafc.up),
              paste0("wafc.up: ", wafc.up),
              # if all values are NA, return NA
              NA_character_
            )
          )
        )
      )
    ) |>
    select(-wafc.lo, -wafc.up, -wafc.fx) |>
    mutate(
      lab_waf = if_else(
        !is.na(waf.fx),
        make_label("waf.fx:", in_brackets = waf.fx, two_lines = FALSE),
        if_else(!is.na(waf.lo) & !is.na(waf.up),
          paste0("waf.lo: ", waf.lo, "\n", "waf.up: ", waf.up),
          if_else(!is.na(waf.lo),
            paste0("waf.lo: ", waf.lo),
            if_else(!is.na(waf.up),
              paste0("waf.up: ", waf.up),
              NA_character_
            )
          )
        )
      )
    ) |>
    select(-waf.lo, -waf.up, -waf.fx) |>
    mutate(
      lab_wafs = if_else(
        !is.na(wafs.fx),
        make_label("wafs.fx:", in_brackets = wafs.fx, two_lines = FALSE),
        if_else(!is.na(wafs.lo) & !is.na(wafs.up),
          paste0("wafs.lo: ", wafs.lo, "\n", "wafs.up: ", wafs.up),
          if_else(!is.na(wafs.lo),
            paste0("wafs.lo: ", wafs.lo),
            if_else(!is.na(wafs.up),
              paste0("wafs.up: ", wafs.up),
              NA_character_
            )
          )
        )
      )
    ) |>
    select(-wafs.lo, -wafs.up, -wafs.fx) |>
    rowwise() |>
    mutate(
      lab_txt = if_else(
        is.na(comm),
        weather,
        make_label(weather, in_brackets = comm, two_lines = FALSE)
      ),
      lab_par = if_else(
        all(is.na(c(lab_wafc, lab_waf, lab_wafs))),
        NA_character_,
        paste(na.omit(c(lab_wafc, lab_waf, lab_wafs)), collapse = "\n")
      )
    ) |>
    select(-lab_wafc, -lab_waf, -lab_wafs) |>
    mutate(
      io = "winp"
    )
  wea

  geff <- object@geff |>
    pivot_longer(
      cols = ginp2use,
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(across(any_of(keys))) |>
    summarise(
      lab_par = make_label(
        paste0(parameter, ":"),
        # NULL,
        in_brackets = value,
        two_lines = T
      ),
      .groups = "drop"
    ) |>
    mutate(
      io = "ginp",
      lab_txt = NA_character_
    )
  geff


  # inputs to draw_process ####
  grouped_com_inputs <- gcom_par |>
    bind_rows(geff) |>
    filter(grepl("inp", io)) |>
    select(io, comm, group, parameter, lab_par) |>
    rename(ioname = comm, iotype = io) |>
    unique()


  single_com_inputs <- ccom_par |>
    filter(grepl("inp", io)) |>
    select(io, comm, parameter, lab_par) |>
    rename(ioname = comm, iotype = io) |>
    unique()

  aux_inputs <- aux |>
    filter(grepl("inp", io)) |>
    select(any_of(c(
      "io", "acomm", "parameter", "lab_par"
    ))) |>
    rename(ioname = acomm, iotype = io) |>
    unique()

  weather_factors <- wea |>
    select(io, weather, lab_par) |>
    rename(ioname = weather, iotype = io) |>
    unique()

  # outputs to draw_process ####
  grouped_com_outputs <- gcom_par |>
    filter(grepl("out", io)) |>
    select(any_of(c(
      "comm", "value", "lab_txt", "lab_par", "group", "unit",
      "io", "parameter", "lab_par"
    ))) |>
    rename(ioname = comm, iotype = io) |>
    unique()

  single_com_outputs <- ccom_par |>
    select(any_of(c(
      "comm", "value", "lab_txt", "lab_par", "group", "unit",
      "io", "parameter", "lab_par"
    ))) |>
    filter(grepl("out", io)) |>
    rename(ioname = comm, iotype = io) |>
    unique()

  aux_outputs <- aux |>
    filter(grepl("out", io)) |>
    select(any_of(c(
      "io", "acomm", "parameter", "lab_par"
    ))) |>
    rename(ioname = acomm, iotype = io) |>
    unique()


  # arrow_labels (all inputs and outputs) ####
  arrow_labels_tb <- data.table()
  if (nrow(gcom_par) > 0) {
    arrow_labels_tb <- rbindlist(
      list(
        arrow_labels_tb,
        gcom_par |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = comm)
      ),
      use.names = TRUE, fill = TRUE
    )
  }
  if (nrow(ccom_par) > 0) {
    arrow_labels_tb <- rbindlist(
      list(
        arrow_labels_tb,
        ccom_par |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = comm)
      ),
      use.names = TRUE, fill = TRUE
    )
  }
  if (nrow(aux) > 0) {
    arrow_labels_tb <- rbindlist(
      list(
        arrow_labels_tb,
        aux |> select(any_of(c("acomm", "lab_txt"))) |> unique() |> rename(ioname = acomm)
      ),
      use.names = TRUE, fill = TRUE
    )
  }
  if (nrow(wea) > 0) {
    arrow_labels_tb <- rbindlist(
      list(
        arrow_labels_tb,
        wea |> select(any_of(c("weather", "lab_txt"))) |> unique() |> rename(ioname = weather)
      ),
      use.names = TRUE, fill = TRUE
    )
  }

  # arrow_labels_tb <- rbindlist(list(
  #   gcom_par |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = comm),
  #   ccom_par |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = comm),
  #   aux |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = acomm),
  #   wea |> select(any_of(c("comm", "lab_txt"))) |> unique() |> rename(ioname = weather)
  # ),
  # use.names = TRUE, fill = TRUE
  # )

  # cap2act ####
  cap2act_label <- paste0("cap2act: ", object@cap2act)

  stopifnot(length(unique(arrow_labels_tb$ioname)) == nrow(arrow_labels_tb))
  arrow_labels <- arrow_labels_tb$lab_txt
  names(arrow_labels) <- arrow_labels_tb$ioname
  stopifnot(length(arrow_labels) == nrow(arrow_labels_tb))

  # A ghost is a shape, not a caption. Fading alone leaves every title and
  # commodity label legible, so three vintages stack three overlapping titles on
  # top of each other and the figure becomes unreadable. Keep the box and the
  # arrows -- they carry the "how many, and where does the selected one sit"
  # signal -- and drop the text.
  if (isTRUE(.ghost)) {
    arrow_labels <- NULL
    cap2act_label <- NULL
  }

  # Ghost arrows extend well beyond the box on both sides, so with several
  # vintages they cross each other and the selected figure's own arrows -- the
  # one thing the reader is meant to follow. Ghosts keep their box (which is
  # what carries "how many, and where in the sequence") and drop the flows.
  gh <- isTRUE(.ghost)
  try(
    draw_process(
      process_name = if (gh) "" else object@name,
      process_desc = if (gh) "" else object@desc,
      grouped_com_inputs = grouped_com_inputs,
      single_com_inputs = single_com_inputs,
      aux_inputs = aux_inputs,
      weather_factors = weather_factors,
      grouped_com_outputs = grouped_com_outputs,
      single_com_outputs = single_com_outputs,
      # com_outputs = com_outputs,
      aux_outputs = aux_outputs,
      arrow_labels = arrow_labels,
      center_label = cap2act_label,
      show_iuao_labels = !gh,
      show_inputs = !gh, show_outputs = !gh,
      show_use_bar = !gh, show_act_bar = !gh,
      ...
    )
  )
} # end of .draw_technology_one

# -- variants: vintages and clusters ----------------------------------------- #
# A vintaged technology is N technologies after expansion, so the figure shows
# ONE of them in full and the rest as faded, slightly smaller "ghosts" behind:
# earlier vintages drift left, later ones right. A glance then tells you how
# many vintages exist and whether the one in front is the first, in the middle,
# or the last.
#
# Ghosts need no change to the drawing code. `R/draw.R` never sets `alpha`, so
# pushing a viewport with `gpar(alpha = ...)` fades every grob inside it,
# including the hardcoded colours.
#
# CLUSTERS ARE NOT DRAWN THE SAME WAY, and deliberately so. Vintages are the
# same plant at different build years -- mutually exclusive selves, which is
# exactly what a fan of past/future ghosts says. Clusters are parallel
# sub-processes that coexist in the same year and compete with each other.
# Fanning them too would say the wrong thing, and would also swamp the figure:
# the real IDEEA wind technology is 11 clusters x 4 vintages = 44 cells, and its
# 11 cluster boxes are visually IDENTICAL (they differ only in which land
# commodity and which weather series they name). So the cluster axis is drawn as
# an index -- a tick rail, or a deck edge -- not as more boxes. See
# `.draw_cluster_axis()`.

# Every expanded cell of one process, with each axis' levels in declared order.
# Returns NULL when there is nothing to show (no variants, or a single cell).
#' @noRd
.tech_variants <- function(object) {
  lev <- function(dim) {
    tryCatch(as.character(.variant_levels(object, dim)), error = function(e) character())
  }
  vins <- lev("vintage")
  clus <- lev("cluster")
  if (length(vins) < 2L && length(clus) < 2L) return(NULL)
  ex <- tryCatch(.expand_one_process(object), error = function(e) NULL)
  if (is.null(ex) || is.null(ex$provenance) || length(ex$objects) < 2L) {
    return(NULL)
  }
  prov <- ex$provenance
  prov$vintage <- as.character(prov$vintage)
  prov$cluster <- as.character(prov$cluster)
  # Order cells by (vintage, cluster) as DECLARED -- `.variant_levels()` already
  # returns the intended order (sorted years; the declared `order` column for
  # clusters), so ranking by `match()` rather than re-sorting keeps a
  # best-to-worst cluster ranking from being shuffled back to alphabetical.
  ord <- order(match(prov$vintage, vins), match(prov$cluster, clus))
  list(objects = ex$objects[ord], prov = prov[ord, , drop = FALSE],
       vintages = if (length(vins)) vins else NULL,
       clusters = if (length(clus)) clus else NULL)
}

# Which level of ONE axis is selected: NULL = `default`, else a level name or a
# 1-based index into `levels`.
#' @noRd
.resolve_axis_pick <- function(sel, levels, dim = "vintage",
                               default = length(levels)) {
  if (is.null(sel)) return(default)
  if (is.numeric(sel)) {
    i <- as.integer(sel)[1]
    if (is.na(i) || i < 1L || i > length(levels)) {
      stop("`", dim, "` index ", i, " is out of range; the technology has ",
           length(levels), " ", dim, "s.", call. = FALSE)
    }
    return(i)
  }
  i <- match(as.character(sel)[1], levels)
  if (is.na(i)) {
    stop("Unknown ", dim, " '", as.character(sel)[1], "'. Available: ",
         paste(levels, collapse = ", "), ".", call. = FALSE)
  }
  i
}

# Kept for the single-axis callers (and their tests): a vintage with no
# selection means the LAST one, i.e. the newest.
#' @noRd
.resolve_vintage_pick <- function(vintage, levels) {
  .resolve_axis_pick(vintage, levels, "vintage", default = length(levels))
}

# Resolve a (vintage, cluster) CELL. The axes are resolved independently and
# then intersected, which is the whole point: selecting a vintage used to mean
# "the first row of the cross product carrying that vintage", leaving every
# other cluster of that vintage unreachable by name.
#
# Defaults differ by axis and both are deliberate: the newest vintage (the one
# a modeller is usually building now) and the FIRST cluster in declared order
# (declarations rank clusters best-resource-first).
#' @noRd
.resolve_cell <- function(v, vintage = NULL, cluster = NULL) {
  vi <- if (is.null(v$vintages)) NA_character_ else
    v$vintages[.resolve_axis_pick(vintage, v$vintages, "vintage",
                                  default = length(v$vintages))]
  ci <- if (is.null(v$clusters)) NA_character_ else
    v$clusters[.resolve_axis_pick(cluster, v$clusters, "cluster", default = 1L)]
  hit <- (is.na(vi) | v$prov$vintage %in% vi) &
    (is.na(ci) | v$prov$cluster %in% ci)
  i <- which(hit)
  if (length(i) == 0L) {
    # Not reachable through the public arguments -- the grid is a full cross
    # product -- but a wrong answer here would be silent, so refuse instead.
    stop("No variant of '", v$prov$base[1], "' has vintage '", vi,
         "' and cluster '", ci, "'.", call. = FALSE)
  }
  list(index = i[1], vintage = vi, cluster = ci)
}

#' Geometry of the faded "ghost" vintages behind a drawn technology
#'
#' Collects the ghost-stack settings of [draw()] into one object, so the
#' drawing methods keep a short signature. Pass either this constructor or a
#' plain named list: `draw(x, ghost = list(drift = 0.2))` is merged onto the
#' defaults. Unknown names are an error in both forms -- misspelling a flat
#' argument used to be swallowed silently by `...` and forwarded downstream.
#'
#' @param alpha opacity of the nearest ghost; further ones fade towards
#'   `alpha_min`.
#' @param alpha_min opacity of the furthest ghost.
#' @param scale,drift,rise geometry of the ghost stack, all describing the
#'   FURTHEST ghost rather than a per-step increment: its size relative to the
#'   selected figure, and how far it is offset horizontally and vertically (in
#'   npc). Intermediate ghosts interpolate. Defining the total extent this way
#'   keeps a technology with thirty annual vintages inside the viewport -- a
#'   per-step offset would put it off-canvas. `rise` defaults to 0: with ghost
#'   arrows suppressed there is nothing for a vertical offset to clear, so the
#'   stack reads better as a flat fan.
#' @param stamp label each ghost with its vintage. The first, last and selected
#'   vintages are always stamped; intermediate ones only when the spacing
#'   leaves room, so a dense stack does not turn into overlapping text.
#'
#' @return a list of class `ghost_options`.
#' @family draw
#' @export
#' @examples
#' ghost_options(drift = 0.25, stamp = FALSE)
ghost_options <- function(alpha = 0.45, alpha_min = 0.12, scale = 0.72,
                          drift = 0.17, rise = 0, stamp = TRUE) {
  structure(
    list(alpha = alpha, alpha_min = alpha_min, scale = scale,
         drift = drift, rise = rise, stamp = stamp),
    class = "ghost_options"
  )
}

# Accept a `ghost_options()` object or a plain list of overrides.
#' @noRd
.as_ghost_options <- function(x) {
  if (inherits(x, "ghost_options")) return(x)
  if (is.null(x)) return(ghost_options())
  if (!is.list(x)) {
    stop("`ghost` must be a `ghost_options()` object or a named list.",
         call. = FALSE)
  }
  d <- ghost_options()
  if (length(x) == 0L) return(d)
  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    stop("`ghost` must be NAMED: e.g. ghost = list(drift = 0.2).", call. = FALSE)
  }
  bad <- setdiff(names(x), names(d))
  if (length(bad)) {
    stop("Unknown ghost option(s): ", paste(bad, collapse = ", "),
         ". Valid: ", paste(names(d), collapse = ", "), ".", call. = FALSE)
  }
  structure(utils::modifyList(unclass(d), x), class = "ghost_options")
}

#' @rdname draw
#' @param vintage which vintage to draw in full for a vintaged technology: a
#'   level name, an index, `NULL` (the default, the newest one), or `"all"` to
#'   lay every vintage out side by side at full detail. Other vintages are drawn
#'   as faded ghosts behind the selected one -- earlier to the left, later to
#'   the right.
#' @param cluster which cluster to draw, in the same forms as `vintage`. The
#'   default `NULL` takes the FIRST declared cluster, declarations ranking
#'   clusters best-resource-first. Clusters are not fanned out like vintages:
#'   they coexist rather than succeed one another, and a technology can carry
#'   dozens, so the axis is drawn as an index (see `cluster_style`).
#' @param cluster_style how to show the cluster axis: `"rail"` (the default) a
#'   strip of ticks with the selection filled, `"deck"` slivers at the box edge
#'   suggesting a stack of cards, or `"none"`.
#' @param ghost geometry of the ghost stack: a [ghost_options()] object, or a
#'   named list of overrides.
#' @param max_facets refuse to lay out more than this many panels for
#'   `vintage = "all"` / `cluster = "all"`. A full 11 x 4 grid is unreadable, and
#'   silently drawing it is worse than saying so.
draw.technology <- function(object, ..., vintage = NULL, cluster = NULL,
                            ghost = ghost_options(),
                            cluster_style = c("rail", "deck", "none"),
                            box_width = 0.4, max_facets = 24L) {
  gh <- .as_ghost_options(ghost)
  cluster_style <- match.arg(cluster_style)

  v <- .tech_variants(object)
  if (is.null(v)) return(.draw_technology_one(object, ...))

  is_all <- function(x) is.character(x) && identical(as.character(x)[1], "all")
  if (is_all(vintage) || is_all(cluster)) {
    return(.draw_technology_facet(v, ..., facet_vintage = is_all(vintage),
                                  facet_cluster = is_all(cluster),
                                  vintage = if (is_all(vintage)) NULL else vintage,
                                  cluster = if (is_all(cluster)) NULL else cluster,
                                  max_facets = max_facets))
  }

  cell <- .resolve_cell(v, vintage, cluster)

  # The ghost fan is the VINTAGE MARGINAL through the selected cell: the cells
  # sharing its cluster. Fanning the whole cross product instead is what made a
  # 4 x 11 technology draw 44 boxes and stamp "VIN2020" eleven times over.
  keep <- if (is.na(cell$cluster)) seq_len(nrow(v$prov)) else
    which(v$prov$cluster == cell$cluster)
  objs <- v$objects[keep]
  levels <- v$prov$vintage[keep]
  pick <- match(cell$index, keep)
  n <- length(objs)

  # Fraction of the way from the selected figure to the furthest ghost. Every
  # geometric property is interpolated on this, so the stack always spans
  # exactly `drift` / `rise` however many vintages there are.
  far <- max(abs(seq_len(n) - pick))
  frac <- function(k) if (far == 0) 0 else k / far

  geom <- function(i) {
    k <- abs(i - pick)
    f <- frac(k)
    s <- sign(i - pick)
    list(
      k = k,
      # earlier vintages sit down-left, later ones up-right
      x = 0.5 + s * gh$drift * f,
      y = 0.5 + s * gh$rise * f,
      size = 1 - (1 - gh$scale) * f,
      alpha = gh$alpha - (gh$alpha - gh$alpha_min) * f
    )
  }

  grid::grid.newpage()
  # Back to front: the furthest ghosts first, the selected variant last, so the
  # painter's algorithm leaves the selection on top and fully opaque.
  for (i in order(-abs(seq_len(n) - pick))) {
    g <- geom(i)
    if (g$k == 0L) next
    grid::pushViewport(grid::viewport(
      x = grid::unit(g$x, "npc"), y = grid::unit(g$y, "npc"),
      width = grid::unit(g$size, "npc"), height = grid::unit(g$size, "npc"),
      gp = grid::gpar(alpha = g$alpha)
    ))
    .draw_technology_one(objs[[i]], ..., box_width = box_width,
                         newpage = FALSE, .ghost = TRUE)
    grid::popViewport()
  }
  # The deck goes between the ghosts and the selection: it belongs to the
  # selected box, so it must sit above the fan but below the box it edges.
  if (identical(cluster_style, "deck") && !is.na(cell$cluster)) {
    .draw_cluster_deck(v$clusters, match(cell$cluster, v$clusters),
                       box_width = box_width)
  }
  .draw_technology_one(objs[[pick]], ..., box_width = box_width,
                       newpage = FALSE)

  stamp_y <- NULL
  if (isTRUE(gh$stamp) && n > 1L) {
    stamp_y <- .draw_vintage_stamps(levels, pick, vapply(seq_len(n), function(i) {
      g <- geom(i); c(g$x, g$y, g$size, sign(i - pick))
    }, numeric(4)), box_width = box_width)$y
  }
  if (!is.na(cell$cluster)) {
    .draw_cluster_axis(v$clusters, match(cell$cluster, v$clusters),
                       style = cluster_style, below = stamp_y,
                       box_width = box_width)
  }
  invisible(TRUE)
}

# Vintage labels along the stack. The first, last and selected are always
# stamped; intermediate ones only when the gap between successive stamps is
# wide enough for the text, so thirty annual vintages show three labels rather
# than thirty overlapping ones.
# A vintage's label, in the same form the solver set member takes: the variant
# prefix followed by the level, so a figure and a set listing read alike. The
# level already carrying the prefix (someone naming a vintage "VIN2020") is left
# alone rather than doubled.
#' @noRd
.vintage_stamp_label <- function(level, prefix = NULL) {
  p <- sub("^_", "", (prefix %||% .variant_prefix_default())[["vintage"]])
  l <- as.character(level)
  ifelse(startsWith(toupper(l), toupper(p)), toupper(l), paste0(p, l))
}

#' @noRd
.draw_vintage_stamps <- function(levels, pick, xyzs, min_gap = 0.075,
                                 box_width = 0.4, prefix = NULL) {
  n <- length(levels)
  x <- xyzs[1, ]; y <- xyzs[2, ]; size <- xyzs[3, ]; side <- xyzs[4, ]

  # The SELECTED vintage's label sits within its box's x-interval (anchored at
  # the left edge, reading rightwards like a title). Ghost labels hang OUTWARDS
  # past the edge they peek from -- past to the left, future to the right.
  #
  # Ghosts deliberately do not line up with any particular box: with many
  # vintages their edges are a fraction of a millimetre apart, so precise
  # pointing is impossible and not the point. The labels only need to say "the
  # sequence runs from here to here, and the selection sits in it".
  half <- 0.5 * box_width * size
  bottom <- y - 0.5 * (box_width * 1.5) * size
  at <- ifelse(side < 0, x - half, x + half)
  at[pick] <- x[pick] - half[pick]
  hjust <- ifelse(side > 0, "left", "right")   # ghosts read outwards
  hjust[pick] <- "left"                         # the selection reads inwards
  lab_y <- bottom - 0.028

  # Thin on the box CENTRES, not on the anchor corners. Anchors are
  # left-of-box for past vintages and right-of-box for future ones, so a corner
  # metric makes the two neighbours either side of the selection look far apart
  # when their boxes almost coincide -- which let VIN2036 sit next to VIN2035
  # saying nothing.
  must <- unique(c(1L, pick, n))
  keep <- must
  if (n > 2L) {
    placed <- x[must]
    for (i in setdiff(seq_len(n), must)) {
      if (min(abs(x[i] - placed)) >= min_gap) {
        keep <- c(keep, i)
        placed <- c(placed, x[i])
      }
    }
  }
  txt <- .vintage_stamp_label(levels, prefix)
  for (i in sort(unique(keep))) {
    sel <- identical(i, pick)
    grid::grid.text(
      txt[i],
      x = grid::unit(at[i], "npc"), y = grid::unit(lab_y[i], "npc"),
      just = c(hjust[i], "top"),
      gp = grid::gpar(fontsize = if (sel) 9 else 8,
                      fontface = if (sel) "bold" else "plain",
                      col = if (sel) "grey15" else "grey45")
    )
  }
  # `y` is the lowest text baseline actually used, so the cluster axis can be
  # anchored below the stamps instead of guessing a clearance.
  invisible(list(keep = sort(unique(keep)), y = min(lab_y[keep])))
}

# -- the cluster axis -------------------------------------------------------- #
# A cluster's label, in the same form the solver set member takes -- the mirror
# of `.vintage_stamp_label()`, doubling guard included, so a level someone wrote
# as "CL03" is not stamped "CLCL03".
#' @noRd
.cluster_stamp_label <- function(level, prefix = NULL) {
  p <- sub("^_", "", (prefix %||% .variant_prefix_default())[["cluster"]])
  l <- as.character(level)
  ifelse(startsWith(toupper(l), toupper(p)), toupper(l), paste0(p, l))
}

# The default: one tick per cluster in a centred band, the selection filled and
# taller, with a caption naming it and giving its position. The ticks carry no
# text of their own, so 50 clusters simply pack tighter instead of turning into
# overlapping labels -- which is the whole reason clusters are not fanned out
# like vintages.
#' @noRd
.draw_cluster_rail <- function(levels, pick, y, width = 0.34, prefix = NULL) {
  n <- length(levels)
  if (n < 2L) return(invisible(NULL))
  # Ticks are spaced across the band, but capped so a 2-cluster rail does not
  # become two marks half a page apart.
  span <- min(width, 0.03 * n)
  x <- if (n == 1L) 0.5 else seq(0.5 - span / 2, 0.5 + span / 2, length.out = n)
  tw <- min(grid::unit(2.2, "mm"), grid::unit(span / max(n - 1L, 1L) * 0.6, "npc"))
  for (i in seq_len(n)) {
    sel <- i == pick
    grid::grid.rect(
      x = grid::unit(x[i], "npc"), y = grid::unit(y, "npc"),
      width = tw,
      height = grid::unit(if (sel) 3.4 else 2.0, "mm"),
      gp = grid::gpar(fill = if (sel) "grey15" else "grey78", col = NA)
    )
  }
  grid::grid.text(
    paste0(.cluster_stamp_label(levels[pick], prefix), "  (", pick, " of ", n, ")"),
    x = grid::unit(0.5, "npc"), y = grid::unit(y - 0.035, "npc"),
    just = c("centre", "top"),
    gp = grid::gpar(fontsize = 8, col = "grey45")
  )
  invisible(NULL)
}

# The alternative: slivers peeking from the box's LOWER-RIGHT CORNER, like the
# edge of a deck of cards seen at an angle.
#
# Two constraints shape this. Whole offset rectangles are out: the vintage fan
# already occupies the space to the right, and a full ghost box there would sit
# on top of it. Full-HEIGHT slivers are out too: commodity arrows leave the
# box's right edge at whatever height their flow sits, and a full-height sliver
# is guaranteed to be crossed by one. Restricting them to the lower part of the
# edge clears the arrows in the common case, at the cost of a smaller hint.
#
# The residual overlap with a right-hand ghost is inherent to putting anything
# to the right of the box, and is the reason `"rail"` is the default.
#' @noRd
.draw_cluster_deck <- function(levels, pick, box_width = 0.4, max_slivers = 4L,
                               frac = 0.34) {
  n <- length(levels)
  if (n < 2L) return(invisible(NULL))
  k <- min(n - 1L, max_slivers)
  h <- box_width * 1.5
  bottom <- 0.5 - h / 2
  for (i in seq_len(k)) {
    off <- 0.008 * i
    grid::grid.rect(
      x = grid::unit(0.5 + box_width / 2 + off, "npc"),
      y = grid::unit(bottom - off, "npc"),
      width = grid::unit(0.006, "npc"), height = grid::unit(h * frac, "npc"),
      just = c("left", "bottom"),
      gp = grid::gpar(fill = "grey80", col = "grey60", lwd = 0.5)
    )
  }
  invisible(NULL)
}

# Draw the cluster axis in the requested style, below the vintage stamps.
#' @noRd
.draw_cluster_axis <- function(levels, pick, style = "rail", below = NULL,
                               box_width = 0.4, prefix = NULL) {
  if (identical(style, "none") || length(levels) < 2L) return(invisible(NULL))
  if (identical(style, "deck")) {
    # the slivers themselves are drawn earlier (under the box); only the
    # caption belongs down here
    y <- .cluster_axis_y(below)
    grid::grid.text(
      paste0(.cluster_stamp_label(levels[pick], prefix),
             "  (", pick, " of ", length(levels), ")"),
      x = grid::unit(0.5, "npc"), y = grid::unit(y, "npc"),
      just = c("centre", "top"),
      gp = grid::gpar(fontsize = 8, col = "grey45")
    )
    return(invisible(NULL))
  }
  .draw_cluster_rail(levels, pick, y = .cluster_axis_y(below), prefix = prefix)
}

# Where the cluster axis sits: clear of the vintage stamps when there are any,
# else clear of the box. Floored so a wide `box_width` cannot push it off-page.
#' @noRd
.cluster_axis_y <- function(below = NULL, gap = 0.045, floor = 0.04) {
  y <- if (is.null(below) || !is.finite(below)) 0.14 else below - gap
  max(y, floor)
}

# `vintage = "all"` / `cluster = "all"`: every selected variant at full detail,
# vintages across the columns and clusters down the rows. An axis that was not
# given "all" is pinned to its selected level, so `cluster = "all"` alone gives
# one row per cluster of a single vintage rather than the whole cross product.
#' @noRd
.draw_technology_facet <- function(v, ..., facet_vintage = FALSE,
                                   facet_cluster = FALSE,
                                   vintage = NULL, cluster = NULL,
                                   max_facets = 24L) {
  cols <- if (facet_vintage || is.null(v$vintages)) v$vintages else
    v$vintages[.resolve_axis_pick(vintage, v$vintages, "vintage",
                                  default = length(v$vintages))]
  rows <- if (facet_cluster || is.null(v$clusters)) v$clusters else
    v$clusters[.resolve_axis_pick(cluster, v$clusters, "cluster", default = 1L)]
  nc <- max(length(cols), 1L)
  nr <- max(length(rows), 1L)

  if (nr * nc > max_facets) {
    stop(nr * nc, " panels (", nr, " cluster(s) x ", nc, " vintage(s)) exceeds ",
         "`max_facets` = ", max_facets, ". Fix one axis to a single level, or ",
         "raise `max_facets` if you really want them all.", call. = FALSE)
  }

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(nrow = nr, ncol = nc)))
  on.exit(grid::popViewport(1), add = TRUE)
  for (r in seq_len(nr)) {
    for (cc in seq_len(nc)) {
      hit <- (is.null(cols) | v$prov$vintage %in% cols[cc]) &
        (is.null(rows) | v$prov$cluster %in% rows[r])
      i <- which(hit)
      if (length(i) == 0L) next
      grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = cc))
      .draw_technology_one(v$objects[[i[1]]], ..., newpage = FALSE)
      grid::popViewport()
    }
  }
  invisible(TRUE)
}
#' Draw a schematic representation of a technology
#'
#' @export
#' @family draw technology
#' @rdname draw
#'
#' @include generics.R
#'
#' @examples
#' TECH01 <- newTechnology(
#'   "TECH01",
#'   desc = "Technology Description",
#'   input = data.frame(
#'     comm = c("COM1", "COM2", "COM5", "COM7", "COM8", "COM9"),
#'     group = c("1", "1", NA, "2", "2", "2"),
#'     unit = c("unit1", "unit2", "unit5", "unit7", "unit8", "unit9")
#'   ),
#'   output = data.frame(
#'     comm = c("COM3", "COM4", "COM6"),
#'     group = c("3", NA, "3"),
#'     unit = c("unit3", "unit4", "unit6")
#'   ),
#'   group = data.frame(
#'     group = c("1", "2", "3"),
#'     desc = c("Group1", "Group2", "Group3"),
#'     unit = "unit"
#'   ),
#'   aux = data.frame(
#'     acomm = c("AUX1", "AUX2", "AUX3", "AUX4"),
#'     unit = c("unit1", "unit2", "unit3", "unit4")
#'   ),
#'   region = c("R1", "R2", "R3"),
#'   geff = data.frame(
#'     group = c("1", "2"),
#'     ginp2use = c(0.12, 0.789)
#'   ),
#'   ceff = data.frame(
#'     comm = c("COM1", "COM2", "COM5", "COM7", "COM8", "COM9", "COM3", "COM4", "COM6"),
#'     cinp2ginp = c(.1, .2, NA, .7, .8, .9, rep(NA, 3)),
#'     cinp2use = c(NA, NA, .5, NA, NA, NA, rep(NA, 3)),
#'     use2cact = c(rep(NA, 6), .36, .4, .36),
#'     cact2cout = c(rep(NA, 6), .3, NA, .6),
#'     share.lo = c(.01, .02, NA, .07, .08, .0, .03, NA, .06),
#'     share.up = c(.91, .92, NA, .97, .98, 1, .83, NA, .96)
#'   ),
#'   aeff = data.frame(
#'     acomm = c("AUX1", "AUX2", "AUX3", "AUX4"),
#'     comm = c(NA, "COM1", NA, "COM3"),
#'     act2ainp = c(1, NA, NA, NA),
#'     cinp2aout = c(NA, 2, NA, NA),
#'     cap2aout = c(NA, NA, 3, NA),
#'     cout2aout = c(NA, NA, NA, 4)
#'   ),
#'   weather = data.frame(
#'     weather = "WEATHER_CF1",
#'     waf.up = .99
#'   )
#' )
#' draw(TECH01)
setMethod("draw", "technology", function(object, ...) {
  draw.technology(object, ...)
})

## draw.storage ####
draw.storage <- function(object, ...) {
  keys <- c(
    "region", "year", "timeslice", "comm", "acomm",
    # "value",
    "lab_par", "lab_txt",
    "tech", "group", "weather", "unit", "io", "parameter"
  )
  # browser()
  comm <- data.frame(
    comm = object@commodity
    # unit = object@unit
  ) |>
    cross_join(object@seff) |>
    pivot_longer(
      cols = matches("eff"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    group_by(comm, parameter) |>
    summarize(
      lab_par = make_label(
        paste0(parameter, ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    mutate(
      group = NA_character_,
      # io = "cinp",
      # lab_txt = make_label(
      #   comm,
      #   in_brackets = object@unit,
      #   two_lines = FALSE
      # )
      lab_txt = comm
      # ioname = comm
    )
  comm
  com_txt <- comm |>
    select(comm, lab_txt) |>
    unique()

  com_inp <- comm |>
    filter(grepl("inp", parameter)) |>
    mutate(iotype = "cinp") |>
    rename(ioname = comm)

  com_out <- comm |>
    filter(grepl("out", parameter)) |>
    mutate(iotype = "cout") |>
    rename(ioname = comm)

  # `cap2stg` is deliberately NOT carried here: `@cap2stg` is a data.frame, so
  # `mutate(cap2stg = object@cap2stg)` only works while it has exactly one row,
  # and nothing downstream reads the column -- only `stg_par$lab_par` is used.
  stg_par <- comm |>
    filter(grepl("stg", parameter)) |>
    mutate(iotype = "stg") |>
    rename(ioname = comm)

  # aux
  aux <- object@aux |>
    full_join(object@aeff, by = "acomm") |>
    pivot_longer(
      cols = matches("2"), # non-grouped-comm-params have "2" in their names
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter, acomm, unit) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    mutate(
      iotype = if_else(grepl("ainp$", parameter), "ainp", "aout"),
      ioname = acomm,
      lab_txt = make_label(
        acomm,
        in_brackets = unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      )
    )
  aux
  aux_inputs <- aux |> filter(iotype == "ainp")
  aux_outputs <- aux |> filter(iotype == "aout")


  if (nrow(object@weather) > 0) {
    wea <- object@weather |>
      rowwise() |>
      mutate(
        lab_waf = if_else(
          !is.na(waf.fx),
          make_label("waf.fx:", in_brackets = waf.fx, two_lines = FALSE),
          if_else(!is.na(waf.lo) & !is.na(waf.up),
            paste0("waf.lo: ", waf.lo, "\n", "waf.up: ", waf.up),
            if_else(!is.na(waf.lo),
              paste0("waf.lo: ", waf.lo),
              if_else(!is.na(waf.up),
                paste0("waf.up: ", waf.up),
                NA_character_
              )
            )
          )
        )
      ) |>
      select(-waf.lo, -waf.up, -waf.fx) |>
      mutate(
        lab_wcinp = if_else(
          !is.na(wcinp.fx),
          make_label("wcinp.fx:", in_brackets = wcinp.fx, two_lines = FALSE),
          if_else(!is.na(wcinp.lo) & !is.na(wcinp.up),
            paste0("wcinp.lo: ", wcinp.lo, "\n", "wcinp.up: ", wcinp.up),
            if_else(!is.na(wcinp.lo),
              paste0("wcinp.lo: ", wcinp.lo),
              if_else(!is.na(wcinp.up),
                paste0("wcinp.up: ", wcinp.up),
                NA_character_
              )
            )
          )
        )
      ) |>
      select(-wcinp.lo, -wcinp.up, -wcinp.fx) |>
      rowwise() |>
      mutate(
        lab_txt = if_else(
          is.na(NA),
          weather,
          make_label(weather, in_brackets = object@commodity, two_lines = FALSE)
        ),
        lab_par = if_else(
          all(is.na(c(lab_waf, lab_wcinp))),
          NA_character_,
          paste(na.omit(c(lab_waf, lab_wcinp)), collapse = "\n")
        )
      ) |>
      select(-lab_waf, -lab_wcinp) |>
      rename(
        ioname = weather
      ) |>
      mutate(
        iotype = "winp"
      )
  } else {
    wea <- list(
      lab_par = NULL,
      lab_txt = NULL
    )
  }
  wea

  # center labels
  #
  # `@cap2stg` is a DATA FRAME (vintage, cluster, region, year, cap2stg) --
  # not a scalar like technology's `@cap2act`. `paste0()` over it vectorises
  # across COLUMNS, which printed one "cap2stg: NA" per key column and then
  # the real value. Take the value column, drop NAs, de-duplicate.
  cap2stg_vals <- unique(object@cap2stg[["cap2stg"]])
  cap2stg_vals <- cap2stg_vals[!is.na(cap2stg_vals)]
  cap2stg_label <- if (length(cap2stg_vals) == 0L) NULL else
    paste0("cap2stg: ", paste(cap2stg_vals, collapse = ", "))

  center_labels <- c(stg_par$lab_par, cap2stg_label)
  # drop empties, or an unset `lab_par` leaves a blank leading line
  center_labels <- center_labels[!is.na(center_labels) & nzchar(center_labels)]
  center_labels <- paste(center_labels, collapse = "\n")

  # arrow_label ####
  arrow_labels <- c(com_txt$lab_txt, aux$lab_txt, wea$lab_txt)
  names(arrow_labels) <- c(com_txt$comm, aux$acomm, wea$ioname)

  draw_process(
    process_name = object@name,
    process_desc = object@desc,
    single_com_inputs = com_inp,
    single_com_outputs = com_out,
    aux_inputs = aux_inputs,
    aux_outputs = aux_outputs,
    weather_factors = wea,
    # storage = stg_par,
    arrow_labels = arrow_labels,
    center_label = center_labels,
    # show_inputs = TRUE,
    # show_outputs = TRUE,
    # show_aux = TRUE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    show_iuao_labels = FALSE
  )
}
#' @export
#'
#' @family draw storage
#' @rdname draw
#'
#' @examples
#' STG_ELC <- newStorage(
#'   name = "STG_ELC", # used in sets
#'   desc = "Electricity storage (battery)", # for own reference
#'   commodity = "ELECTRICITY", # must match the commodity name in the model
#'   aux = data.frame(
#'     acomm = "LITHIUM", # auxiliary commodity for battery production
#'     unit = "ton" # unit of the auxiliary commodity
#'   ),
#'   start = data.frame(
#'     start = 2020 # the first year of the process is available for installation
#'   ),
#'   end = data.frame(
#'     end = 2030 # last year of the process is available for installation
#'   ),
#'   olife = data.frame(
#'     olife = 20 # operational life of the storage in years
#'   ),
#'   seff = data.frame(
#'     stgeff = 0.999, # storage efficiency
#'     inpeff = 0.9, # charging efficiency
#'     outeff = 0.9 # discharging efficiency
#'   ),
#'   aeff = data.frame(
#'     acomm = "LITHIUM", # track lithium use for battery production
#'     ncap2ainp = convert(4 * 250, "Wh/kg", "GWh/kt") # lithium per energy capacity
#'   ),
#'   af = data.frame(
#'     # af.lo = 0., # lower bound for the capacity factor
#'     af.up = 1. # upper bound for the capacity factor
#'   ),
#'   fixom = data.frame(
#'     # region = "R1",
#'     # year = 2020,
#'     fixom = 0.9 # fixed operation and maintenance cost
#'   ),
#'   cap2stg = 4, # four-hours of storage
#'   invcost = data.frame(
#'     region = c("R1", NA), # region R1 and all other regions
#'     invcost = c(1e3, 1.1e3) # investment cost in MUSD/GWh of 4-hour storage
#'   ),
#'   fullYear = TRUE, # full year storage cycle
#'   weather = data.frame(
#'     weather = "AMBIENT_TEMP", # weather factor for capacity factor
#'     waf.up = 1 # affects upper boundary of capacity factor
#'     # waf.lo = 0.9 # affects lower boundary of capacity factor
#'   )
#'   # region = c("R1", "R2", "R3"),
#' )
#' draw(STG_ELC)
#'
#' @exportMethod draw
setMethod("draw", "storage", draw.storage)


## draw.supply ####
draw.supply <- function(object, ...) {
  # keys <- c("region", "year", "timeslice", "comm", "acomm",
  #           # "value",
  #           "lab_par", "lab_txt",
  #           "tech", "group", "weather", "unit", "io", "parameter")
  # browser()
  if (nrow(object@supply) == 0) {
    sup_par <- data.frame(
      lab_par = "",
      lab_txt = "",
      iotype = "cout",
      ioname = object@commodity,
      group = NA_character_,
      parameter = "sup"
    )
  } else {
    sup_par <- object@supply |>
      pivot_longer(
        cols = matches("ava|cost"),
        names_to = "parameter",
        values_to = "value"
      ) |>
      filter(!is.na(value)) |>
      group_by(parameter) |>
      summarize(
        lab_par = make_label(
          paste0(unique(parameter), ":"),
          in_brackets = value,
          two_lines = FALSE,
          bracket_type = "square"
        ),
        .groups = "drop"
      ) |>
      # mutate(
      #   iotype = "cout",
      #   ioname = object@commodity,
      #   group = NA_character_,
      #   lab_txt = make_label(
      #     object@commodity,
      #     in_brackets = object@unit,
      #     return_name_if_empty = TRUE,
      #     two_lines = FALSE
      #   )
      # ) |>
      mutate(
        iotype = "cout",
        ioname = object@commodity,
        group = NA_character_
      ) |>
      group_by(ioname, iotype, group) |>
      summarize(
        lab_par = paste0(lab_par, collapse = "\n"),
        .groups = "drop"
      ) |>
      mutate(
        lab_txt = make_label(
          ioname,
          in_brackets = object@unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        ),
        parameter = "sup"
      )
  }

  arrow_labels <- make_label(
    object@commodity,
    in_brackets = object@unit,
    return_name_if_empty = TRUE,
    two_lines = FALSE
  )
  names(arrow_labels) <- object@commodity

  # reserve
  if (nrow(object@reserve) > 0) {
    res_par <- object@reserve |>
      pivot_longer(
        cols = matches("res"),
        names_to = "parameter",
        values_to = "value"
      ) |>
      filter(!is.na(value)) |>
      group_by(parameter) |>
      summarize(
        lab_par = make_label(
          paste0(unique(parameter), ":"),
          in_brackets = value,
          two_lines = FALSE,
          bracket_type = "square"
        ),
        .groups = "drop"
      ) |>
      mutate(
        iotype = "cinp",
        ioname = object@commodity,
        group = NA_character_,
        lab_txt = make_label(
          object@commodity,
          in_brackets = object@unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        )
      ) |>
      mutate(
        iotype = "cout",
        ioname = object@commodity,
        group = NA_character_
      ) |>
      group_by(ioname, iotype, group) |>
      summarize(
        lab_par = paste0(lab_par, collapse = "\n"),
        .groups = "drop"
      ) |>
      mutate(
        lab_txt = make_label(
          ioname,
          in_brackets = object@unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        ),
        parameter = "sup"
      )
    res_par
  } else {
    res_par <- list(
      lab_par = NULL,
      lab_txt = NULL
    )
  }

  draw_process(
    ...,
    process_name = object@name,
    process_desc = object@desc,
    single_com_outputs = sup_par,
    center_label = res_par$lab_par,
    arrow_labels = arrow_labels,
    show_inputs = FALSE, # !!! add weather factors
    show_outputs = TRUE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    show_iuao_labels = FALSE,
    box_width = 0.25,
    box_height = .4 * 1.5
  )
}
#' @family draw supply
#' @rdname draw
#'
#' @export
#'
#' @examples
#' SUP_COA <- newSupply(
#'   name = "SUP_COA",
#'   desc = "Coal supply",
#'   commodity = "COA",
#'   unit = "PJ",
#'   reserve = data.frame(
#'     region = c("R1", "R2", "R3"),
#'     res.up = c(2e5, 1e4, 3e6) # total reserves/deposits
#'   ),
#'   supply = data.frame(
#'     region = c("R1", "R2", "R3"),
#'     year = NA_integer_,
#'     timeslice = "ANNUAL",
#'     ava.up = c(1e3, 1e2, 2e2), # annual availability
#'     cost = c(10, 20, 30) # cost of the resource (currency per unit)
#'   ),
#'   region = c("R1", "R2", "R3")
#' )
#' draw(SUP_COA)
setMethod("draw", "supply", draw.supply)

## draw.demand ####
draw.demand <- function(object, ...) {
  # browser()
  dem_par <- object@demand |>
    pivot_longer(
      cols = matches("demand"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    mutate(
      iotype = "cinp",
      ioname = object@commodity,
      group = NA_character_,
      lab_txt = make_label(
        object@commodity,
        in_brackets = object@unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      )
    ) |>
    group_by(ioname, iotype, group) |>
    summarize(
      lab_par = paste0(lab_par, collapse = "\n"),
      .groups = "drop"
    ) |>
    mutate(
      lab_txt = make_label(
        ioname,
        in_brackets = object@unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      ),
      parameter = "demand"
    )
  dem_par

  arrow_labels <- make_label(
    object@commodity,
    in_brackets = object@unit,
    return_name_if_empty = TRUE,
    two_lines = FALSE
  )
  names(arrow_labels) <- object@commodity

  draw_process(
    ...,
    process_name = object@name,
    process_desc = object@desc,
    single_com_inputs = dem_par,
    arrow_labels = arrow_labels,
    show_inputs = TRUE,
    show_outputs = FALSE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    show_iuao_labels = FALSE,
    box_width = 0.2,
    box_height = .4 * 1.5
  )
}

#' @family draw demand
#' @rdname draw
#'
#' @examples
#' DSTEEL <- newDemand(
#'   name = "DSTEEL",
#'   desc = "Steel demand",
#'   commodity = "STEEL",
#'   unit = "Mt",
#'   dem = data.frame(
#'     region = "UTOPIA", # NA for every region
#'     year = c(2020, 2030, 2050),
#'     timeslice = "ANNUAL",
#'     dem = c(100, 200, 300)
#'   ),
#'   region = "UTOPIA", # optional, to narrow the specification of the demand
#' )
#' draw(DSTEEL)
#' @exportMethod draw
setMethod(
  "draw", signature(object = "demand"),
  function(object, ...) draw.demand(object, ...)
)

## draw.export ####
draw.export <- function(object, ...) {
  # browser()

  # key columns

  # export parameters
  exp_par <-
    object@export |>
    pivot_longer(
      cols = matches("exp|price"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        # in_brackets = format_number(value),
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    # mutate(
    #   lab_par = if_else(
    #     is.na(lab_regions),
    #     lab_par,
    #     paste(lab_par, lab_regions, sep = " ")
    #   ),
    #   lab_par = if_else(
    #     is.na(lab_years),
    #     lab_par,
    #     paste(lab_par, lab_years, sep = " ")
    #   )
    # ) |>
    # select(-lab_regions, -lab_years) |>
    mutate(
      iotype = "cinp",
      ioname = object@commodity,
      group = NA_character_,
    ) |>
    group_by(ioname, iotype, group) |>
    summarize(
      lab_par = paste0(lab_par, collapse = "\n"),
      #   lab_regions = if_else(
      #     all(is.na(region)),
      #     NA_character_,
      #     paste0(
      #       # "{R(", length(unique(object@export$region)), "):",
      #       "Regions: {",
      #       shorten_string(
      #         paste0(sort(unique(object@export$region)), collapse = ","),
      #         n = 15, add_number = length(unique(object@export$region))),
      #       "}")
      #   ),
      #   lab_years = if_else(
      #     all(is.na(year)),
      #     NA_character_,
      #     paste0(
      #       "Years: [",
      #       shorten_string(
      #         paste0(range(object@export$year, na.rm = TRUE), collapse = ","),
      #         15),
      #       "]")
      #   ),
      .groups = "drop"
    ) |>
    mutate(
      lab_txt = make_label(
        ioname,
        in_brackets = object@unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      ),
      parameter = "exp"
    )
  exp_par


  # arrow_label ####
  arrow_labels <- make_label(
    object@commodity,
    in_brackets = object@unit,
    two_lines = FALSE
  )
  names(arrow_labels) <- object@commodity

  draw_process(
    ...,
    process_name = object@name,
    process_desc = object@desc,
    single_com_inputs = exp_par,
    arrow_labels = arrow_labels,
    show_inputs = TRUE,
    show_outputs = FALSE,
    # show_aux = FALSE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    show_iuao_labels = FALSE,
    box_width = 0.2,
    box_height = .4 * 1.5
  )
}
#' @exportMethod draw
#' @family draw export
#' @rdname draw
#'
#' @return
#' A figure with a schematic representation of the export process.
#' @export
#' @examples
#' EXPOIL <- newExport(
#'   name = "EXPOIL", # used in sets
#'   desc = "Oil export from the model to RoW", # for own reference
#'   commodity = "OIL", # must match the commodity name in the model
#'   unit = "Mtoe", # for own reference
#'   exp = data.frame(
#'     region = rep(c("R1", "R2"), each = 2), # export region(s)
#'     year = rep(c(2020, 2050)), # export years
#'     price = 500, # export price in MUSD/Mtoe (USD/t),
#'     exp.up = rep(c(1e3, 1e4), each = 2), # upper bound for export in each year
#'     exp.lo = rep(c(5e2, 0), each = 2) # lower bound for export in each year
#'   )
#' )
#' draw(EXPOIL)
setMethod("draw", "export", draw.export)

## draw.import ####
draw.import <- function(object, ...) {
  # key columns
  # browser()
  # import parameters
  imp_par <-
    object@import |>
    pivot_longer(
      cols = matches("imp|price"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        # in_brackets = format_number(value),
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    # mutate(
    #   lab_par = if_else(
    #     is.na(lab_regions),
    #     lab_par,
    #     paste(lab_par, lab_regions, sep = " ")
    #   ),
    #   lab_par = if_else(
    #     is.na(lab_years),
    #     lab_par,
    #     paste(lab_par, lab_years, sep = " ")
    #   )
    # ) |>
    # select(-lab_regions, -lab_years) |>
    mutate(
      iotype = "cout",
      ioname = object@commodity,
      group = NA_character_,
    ) |>
    group_by(ioname, iotype, group) |>
    summarize(
      lab_par = paste0(lab_par, collapse = "\n"),
      #   lab_regions = if_else(
      #     all(is.na(region)),
      #     NA_character_,
      #     paste0(
      #       # "{R(", length(unique(object@import$region)), "):",
      #       "Regions: {",
      #       shorten_string(
      #         paste0(sort(unique(object@import$region)), collapse = ","),
      #         n = 15, add_number = length(unique(object@import$region))),
      #       "}")
      #   ),
      #   lab_years = if_else(
      #     all(is.na(year)),
      #     NA_character_,
      #     paste0(
      #       "Years: [",
      #       shorten_string(
      #         paste0(range(object@import$year, na.rm = TRUE), collapse = ","),
      #         15),
      #       "]")
      #   ),
      .groups = "drop"
    ) |>
    mutate(
      lab_txt = make_label(
        ioname,
        in_brackets = object@unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      ),
      parameter = "imp"
    )
  imp_par


  # arrow_label ####
  arrow_labels <- make_label(
    object@commodity,
    in_brackets = object@unit,
    return_name_if_empty = TRUE,
    two_lines = FALSE
  )
  names(arrow_labels) <- object@commodity

  draw_process(
    ...,
    process_name = object@name,
    process_desc = object@desc,
    single_com_outputs = imp_par,
    arrow_labels = arrow_labels,
    show_inputs = FALSE,
    show_outputs = TRUE,
    # show_aux = FALSE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    box_width = 0.2,
    box_height = 0.2 * 1.5 * 2
  )
}
#' @return
#' A figure with a schematic representation of the import process.
#' @exportMethod draw
#' @family draw import
#' @rdname draw
#'
#' @export
#'
#' @examples
#' IMPOIL <- newImport(
#'   name = "IMPOIL", # used in sets
#'   desc = "Oil import to the model to RoW", # for own reference
#'   commodity = "OIL", # must match the commodity name in the model
#'   unit = "Mtoe", # for own reference
#'   imp = data.frame(
#'     region = rep(c("R1", "R2"), each = 2), # import region(s)
#'     year = rep(c(2020, 2050)), # import years
#'     price = 600, # import price in MUSD/Mtoe (USD/t),
#'     imp.up = rep(c(1e4, 1e6), each = 2), # upper bound for import in each year
#'     imp.lo = rep(c(1e4, 1e5), each = 2) # lower bound for import in each year
#'   )
#' )
#' draw(IMPOIL)
setMethod("draw", "import", draw.import)

## draw.trade ####
draw.trade <- function(object, ...) {
  arg <- list(...)
  # browser()
  if (!is.null(arg$region)) {
    node <- arg$region
  } else if (!is.null(arg$node)) {
    node <- arg$node
  } else {
    node <- unique(object@routes$src)[1]
  }

  inp_par <- object@routes |>
    filter(dst == node) |>
    left_join(object@trade, by = c("src", "dst")) |>
    pivot_longer(
      cols = matches("ava|eff"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(src, parameter) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    group_by(src) |>
    summarize(
      lab_par = paste0(lab_par, collapse = "\n"),
      parameter = "trade_inp",
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      comm = object@commodity,
      iotype = "cinp",
      ioname = make_label(
        object@commodity,
        in_brackets = src,
        two_lines = F
      ),
      group = NA_character_,
      lab_txt = ioname # !!! since 'unit' is NA
    )
  inp_par

  out_par <- object@routes |>
    filter(src == node) |>
    left_join(object@trade, by = c("src", "dst")) |>
    pivot_longer(
      cols = matches("ava|eff"),
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(dst, parameter) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    group_by(dst) |>
    summarize(
      lab_par = paste0(lab_par, collapse = "\n"),
      parameter = "trade_out",
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      comm = object@commodity,
      iotype = "cout",
      ioname = make_label(
        object@commodity,
        in_brackets = dst,
        two_lines = FALSE
      ),
      group = NA_character_,
      lab_txt = ioname # !!! since 'unit' is NA
    )
  out_par

  # aux
  aux_inp <-
    object@aux |>
    full_join(object@aeff, by = "acomm") |>
    filter(dst == node) |>
    pivot_longer(
      cols = matches("2"), # non-grouped-comm-params have "2" in their names
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter, acomm, unit, src) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    filter(grepl("ainp", parameter)) |>
    rowwise() |>
    mutate(
      iotype = "ainp",
      ioname = paste0(acomm, ", ", src),
      lab_txt = make_label(
        ioname,
        in_brackets = unit,
        return_name_if_empty = TRUE,
        two_lines = FALSE
      )
    )
  aux_inp
  aux_inp$lab_txt
  aux_inp$lab_par

  aux_out <-
    object@aux |>
    full_join(object@aeff, by = "acomm") |>
    filter(src == node) |>
    pivot_longer(
      cols = matches("2"), # non-grouped-comm-params have "2" in their names
      names_to = "parameter",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    group_by(parameter, acomm, unit, dst) |>
    summarize(
      lab_par = make_label(
        paste0(unique(parameter), ":"),
        in_brackets = value,
        two_lines = FALSE,
        bracket_type = "square"
      ),
      .groups = "drop"
    ) |>
    filter(grepl("aout", parameter)) |>
  # if (nrow(aux_out) > 0) {
    # aux_out <- aux_out |>
      rowwise() |>
      mutate(
        iotype = "aout",
        ioname = paste0(acomm, ", ", dst),
        lab_txt = make_label(
          ioname,
          in_brackets = unit,
          return_name_if_empty = TRUE,
          two_lines = FALSE
        )
      )
  # } else {
  #   aux_out$lab_par <- NULL
  #   aux_out$lab_txt <- NULL
  # }
  aux_out

  cap2act_label <- make_label(
    "cap2act:",
    in_brackets = object@cap2act,
    two_lines = FALSE,
    bracket_type = "square"
  )

  all_nodes <- unique(object@routes$src, object@routes$dst)

  center_label <- paste0(
    cap2act_label,
    "\n",
    make_label(
      "Nodes:",
      in_brackets = paste0(all_nodes, collapse = ", "),
      two_lines = FALSE,
      bracket_type = "square"
    )
  )

  arrow_labels <- c(
    inp_par$lab_txt,
    out_par$lab_txt,
    aux_inp$lab_txt,
    aux_out$lab_txt
  )

  names(arrow_labels) <- c(
    inp_par$ioname,
    out_par$ioname,
    aux_inp$ioname,
    aux_out$ioname
  )

  arrow_labels <- arrow_labels[!duplicated(arrow_labels)]


  draw_process(
    process_name = paste0(object@name, ", node: ", node),
    process_desc = object@desc,
    single_com_inputs = inp_par,
    single_com_outputs = out_par,
    aux_inputs = aux_inp,
    aux_outputs = aux_out,
    arrow_labels = arrow_labels,
    center_label = center_label,
    # show_inputs = TRUE,
    # show_outputs = TRUE,
    show_use_bar = FALSE,
    show_act_bar = FALSE,
    show_iuao_labels = FALSE,
    box_width = 0.3,
    box_height = .4 * 1.5
  )
}
#' @param region A node to draw the trade process for.
#' `node` is an alias for `region`.
#' Default is the first node in the trade object.
#'
#' @export
#'
#' @exportMethod draw
#' @family draw trade
#' @rdname draw
#' @examples
#' PIPELINE2 <- newTrade(
#'   name = "PIPELINE2",
#'   desc = "Some transport pipeline",
#'   commodity = "OIL",
#'   routes = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2")
#'   ),
#'   trade = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2"),
#'     teff = c(0.912, 0.913, 0.923, 0.932)
#'   ),
#'   aux = data.frame(
#'     acomm = c("ELC", "CH4"),
#'     unit = c("MWh", "kt")
#'   ),
#'   aeff = data.frame(
#'     acomm = c("ELC", "CH4", "ELC", "CH4"),
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R2", "R3", "R2"),
#'     csrc2ainp = c(.5, NA, .3, NA),
#'     cdst2ainp = c(.4, NA, .6, NA),
#'     csrc2aout = c(NA, .1, NA, .2)
#'   ),
#'   olife = list(olife = 60)
#' )
#' draw(PIPELINE2, node = "R1")
#' draw(PIPELINE2, node = "R2")
#' draw(PIPELINE2, node = "R3")
setMethod("draw", "trade", draw.trade)

## draw.weather ####

#' An internal function to create a character string with a label
#'
#' @param name A character string with the name as the first part of the label
#' @param in_brackets A character string with the content to put in brackets
#' @param make_range A logical value to indicate if the content should be
#' formatted as a range
#' @param two_lines A logical value to indicate if the label should be in
#' two lines
#' @param return_name_if_empty A logical value to return the name if the content
#' of the brackets is NA or empty.
#' @param bracket_type A character string with the type of brackets to use,
#'  one of "round", "square", "curly", "angle", or NULL
#' @noRd
#' @return A character string with the label
make_label <- function(
    name,
    in_brackets = NULL,
    make_range = TRUE,
    two_lines = FALSE,
    return_name_if_empty = FALSE,
    bracket_type = "round", # "round", "square", "curly", "angle", or NULL
    comma = ",") {
  # browser()

  # if (all(is.na(in_brackets))) return(NA)

  if (is.null(bracket_type)) {
    bracket <- c("", "")
  } else {
    bracket <- switch(bracket_type,
      "round" = c("(", ")"),
      "square" = c("[", "]"),
      "curly" = c("{", "}"),
      "angle" = c("<", ">"),
      "comma" = c(",", ""),
    )
  }
  in_brackets <- in_brackets[!is.na(in_brackets)]
  in_brackets <- in_brackets[in_brackets != ""]
  if (is_empty(in_brackets)) {
    # browser()
    if (isTRUE(return_name_if_empty)) return(name)
    return("")
  }
  if (is.numeric(in_brackets)) {
    if (length(unique(in_brackets)) > 1) {
      in_brackets <- paste0(
        bracket[1],
        format_number(min(in_brackets)),
        comma,
        format_number(max(in_brackets)),
        bracket[2]
      ) |>
        # format_number()
        prettyNum(digits = 2, big.mark = "")
    } else {
      in_brackets <- unique(in_brackets) |> format_number()
    }
  } else {
    in_brackets <- unique(in_brackets)
    if (length(in_brackets) > 1) browser()
    # stopifnot(length(in_brackets) == 1)
    if (is.null(in_brackets) || is.na(in_brackets)) {
      in_brackets <- NULL
    } else {
      in_brackets <- paste0(bracket[1], in_brackets, bracket[2])
    }
  }

  if (two_lines) {
    label <- paste0(name, "\n", in_brackets)
  } else {
    label <- paste0(name, " ", in_brackets)
  }
  label
}



#' Drafted function to convert an S4 object to a data frame
#'
#' @param object An S4 object
#' @param sets A character vector with the names of the sets,
#' colnames to create in the resulting data frame.
#' Default is c("region", "year", "timeslice", "comm", "acomm")
#' @param verbose A logical value if to print messages
#' @noRd
en_obj2df <- function(object, sets = NULL, verbose = FALSE) {
  # browser()
  if (!isS4(object)) {
    stop("Object must be an S4 class")
  }

  if (is.null(sets)) {
    sets <- c("region", "year", "timeslice", "comm", "acomm")
  }

  # object <- tech
  slots <- slotNames(object)

  ll <- list()
  for (s in slots) {
    if (verbose) cat("Processing slot: ", s, "\n")

    if (inherits(slot(object, s), "data.frame")) {
      ll[[s]] <- slot(object, s) |>
        pivot_by_type(sets = sets, slot_name = s)
    } else if (inherits(slot(object, s), c("character", "numeric", "logical"))) {
      ll[[s]] <- data.frame(
        parameter = if (is_empty(slot(object, s))) NA else slot(object, s)
      ) |>
        pivot_by_type(sets = sets, slot_name = s)
    } else if (inherits(slot(object, s), "list")) {
      if (length(slot(object, s)) > 0) {
        message("Skipping list slot: ", s)
      }
      # ll2 <-
    } else {
      message("Skipping slot: ", s, " of class: ", class(slot(object, s)))
    }
  }
  ll |>
    rbindlist(use.names = TRUE, fill = TRUE) |>
    select(
      matches(c("slot", "parameter", sets)),
      "character_val", "logical_val", "numeric_val", everything()
    ) |>
    # filter(slot != "name") |>
    mutate(
      class = class(object),
      name = object@name,
      .before = 1
    )
  # !!! ToDO: add status column (T/F coercion success)
  # !!! ToDO: process list slots (1st level at least)
}

if (F) {
  x <- en_obj2df(TECH01)
}

#' An internal function to pivot a data frame by column type
#'
#' @param x data frame to pivot
#' @param sets character vector with the names of the sets, keys to keep.
#' Default is c("region", "year", "timeslice", "comm", "acomm")
#' @param slot_name character string with the name of the slot,
#' where the data frame comes from to add to the resulting data frame.
#' @noRd
#' @return A data frame with the pivoted data
pivot_by_type <- function(x, sets = NULL, slot_name = NULL) {
  # browser()
  if (is.null(sets)) {
    sets <- c("region", "year", "timeslice", "comm", "acomm")
  }

  df <- data.frame()

  # pivot_longer for character columns
  char_df <- x |> select(any_of(sets), where(is.character))
  cond <- any(sapply(select(char_df, -any_of(sets)), is.character))
  if (ncol(char_df) > 0 && cond) {
    char_df <- char_df |>
      pivot_longer(
        cols = -any_of(sets),
        names_to = "parameter",
        values_to = "character_val"
      ) |>
      filter(!is.na(character_val))
    # merge with existing df
    if (nrow(char_df) > 0) {
      if (nrow(df) > 0) {
        df <- full_join(df, char_df, by = intersect(names(df), names(char_df)))
      } else {
        df <- char_df
      }
    }
  } else {
    char_df <- data.frame()
  }

  # pivot_longer for numeric columns
  num_df <- x |> select(any_of(sets), where(is.numeric))
  cond <- any(sapply(select(num_df, -any_of(sets)), is.numeric))
  if (ncol(num_df) > 0 && cond) {
    num_df <- num_df |>
      pivot_longer(
        cols = -any_of(sets),
        names_to = "parameter",
        values_to = "numeric_val"
      ) |>
      filter(!is.na(numeric_val))
    # merge with existing df
    if (nrow(num_df) > 0) {
      if (nrow(df) > 0) {
        df <- full_join(df, num_df, by = intersect(names(df), names(num_df)))
      } else {
        df <- num_df
      }
    }
  } else {
    num_df <- data.frame()
  }

  # pivot_longer for logical columns
  logical_df <- x |> select(any_of(sets), where(is.logical))
  cond <- any(sapply(select(logical_df, -any_of(sets)), is.logical))
  if (ncol(logical_df) > 0 && cond) {
    logical_df <- logical_df |>
      pivot_longer(
        cols = -any_of(sets),
        names_to = "parameter",
        values_to = "logical_val"
      ) |>
      filter(!is.na(logical_val))
    # merge with existing df
    if (nrow(logical_df) > 0) {
      if (nrow(df) > 0) {
        df <- full_join(df, logical_df, by = intersect(names(df), names(logical_df)))
      } else {
        df <- logical_df
      }
    }
  } else {
    logical_df <- data.frame()
  }

  x <- list(char_df, num_df, logical_df) |>
    rbindlist(use.names = TRUE, fill = TRUE)
  if (!is.null(slot_name)) {
    x <- mutate(x, slot = slot_name, .before = 1)
  }
  return(x)
}

if (F) {
  pivot_by_type(tech@ceff)
}

# draw_process ####

#' An internal function to draw a process
#'
#' @param process_name A character string with the name of the process
#' @param process_desc A character string with the description of the process
#' @param grouped_com_inputs A data frame with the grouped commodity inputs' labels
#' @param single_com_inputs A data frame with the single commodity inputs' labels
#' @param aux_inputs A data frame with the auxiliary inputs' labels
#' @param weather_factors A data frame with the weather factors' labels
#' @param grouped_com_outputs A data frame with the grouped commodity outputs' labels
#' @param single_com_outputs A data frame with the single commodity outputs' labels
#' @param arrow_weather_color A character string with the color of the weather arrows
#' @param arrow_labels A named character vector with the labels of the arrows
#' @param center_label A character string with the label of the cap2act arrow
#' @param box_width A numeric value with the width of the process box
#' @param box_height A numeric value with the height of the process box
#' @param box_fill A character string with the fill color of the process box
#' @param box_border A character string with the border color of the process box
#' @param box_lwd A numeric value with the line width of the process box
#' @param process_name_fontsize A numeric value with the font size of the process name
#' @param process_desc_fontsize A numeric value with the font size of the process description
#' @param arrow_comm_color A character string with the color of the commodity arrows
#' @param arrow_aux_color A character string with the color of the auxiliary arrows
#'
#' @noRd
draw_process <- function(
    process_name = "Process",
    process_desc = "Process Description",
    # inputs
    grouped_com_inputs = NULL,
    single_com_inputs = NULL,
    aux_inputs = NULL,
    weather_factors = NULL,
    # ginp2use = NULL,
    # cap2act = NULL,
    # outputs
    grouped_com_outputs = NULL,
    single_com_outputs = NULL,
    # com_outputs = NULL,
    aux_outputs = NULL,
    # labels
    arrow_labels = NULL,
    center_label = NULL,
    show_inputs = any(
      !is.null(c(
        grouped_com_inputs, single_com_inputs,
        aux_inputs, weather_factors
      )),
      na.rm = T
    ),
    show_outputs = any(!is.null(c(grouped_com_outputs, single_com_outputs)),
      na.rm = T
    ),
    show_use_bar = any(
      !is.null(c(
        grouped_com_inputs, single_com_inputs,
        aux_inputs, weather_factors
      )),
      na.rm = T
    ),
    show_act_bar = any(!is.null(c(grouped_com_outputs, single_com_outputs)),
      na.rm = T
    ),
    show_iuao_labels = FALSE,
    show_all = NULL,
    # draw parameters
    box_width = 0.4,
    box_height = box_width * 1.5,
    arrow_length = 0.175,
    box_fill = rgb(220 / 255, 230 / 255, 242 / 255),
    box_border = "royalblue4",
    box_lwd = 3,
    process_name_fontsize = 14,
    process_desc_fontsize = 10,
    font_spacing = .06,
    fig_background = "white",
    arrow_comm_color = "red3",
    arrow_aux_color = "royalblue4",
    arrow_weather_color = "forestgreen",
    # Corner radius of the process box, in npc. 0 (the default) draws the plain
    # rectangle. Rounding preserves the bounding box, so no label inside moves;
    # keep it small, because `inp`/`out` sit only 0.02 npc in from the vertical
    # edges and a larger radius starts cutting into them.
    box_round = 0,
    # Which vertical edge of the box carries no border. "auto" (the default)
    # drops the border on a side that has NO ARROWS, derived from
    # `show_inputs`/`show_outputs`: a supply or import is open on the left, a
    # demand or export on the right, and a technology/storage/trade -- which
    # flows both ways -- stays fully closed. The open edge makes the box read as
    # a flow entering or leaving the system rather than a sealed container.
    # "none" always draws the closed rectangle.
    # NB an open edge implies a straight outline, so `box_round` is ignored
    # while one is open (a corner radius only makes sense on a closed shape).
    box_open = c("auto", "none"),
    # `newpage = FALSE` draws into whatever viewport is already current instead
    # of clearing the device. That is what lets several processes be composed on
    # one figure -- the faded "ghost" vintages behind a selected one, and the
    # `vintage = "all"` facet layout. It also suppresses the background rect,
    # which would otherwise paint over anything already drawn.
    newpage = TRUE) {
  box_open <- match.arg(box_open)
  result <- tryCatch(
    {
      # browser()

      if (isTRUE(show_all)) {
        show_inputs <- TRUE
        show_outputs <- TRUE
        show_use_bar <- TRUE
        show_act_bar <- TRUE
        show_iuao_labels <- TRUE
      }

      # try(dev.off())
      if (isTRUE(newpage)) {
        grid::grid.newpage()
        if (!is.null(fig_background) && !is.na(fig_background)) {
          grid::grid.rect(
            x = 0.5, y = 0.5,
            width = 1, height = 1,
            gp = grid::gpar(fill = fig_background, col = NA)
          )
        }
      }
      # Set a viewport
      vp <- grid::viewport(
        width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc"),
        just = "center"
      )
      grid::pushViewport(vp)
      # Pop exactly the one viewport pushed above, not `popViewport(0)` -- that
      # unwinds the WHOLE stack, including any viewport the caller pushed, which
      # is what previously made this function impossible to compose.
      on.exit(grid::popViewport(1))

      # NB this IGNORES `fontsize` and returns the constant `font_spacing`. It
      # is a layout nudge, not a font metric -- do not use it to clear something
      # whose size grid measures for real (a grob height in npc changes with the
      # device; this does not). That mismatch is what made arrow-label pads
      # touch their arrows; see `.draw_arrow_label()`.
      font_in_npc <- function(fontsize) {
        # vp_height_in <- grid::convertUnit(unit(1, "npc"), "in", valueOnly = TRUE)
        # fontsize / 72 / vp_height_in * 2
        font_spacing
      }

      spacing_bw_titles <- 0.2 * font_in_npc(
        max(process_name_fontsize, process_desc_fontsize)
      )

      # Which vertical edge is open: the side with no arrows on it.
      open_side <- if (identical(box_open, "none")) "none" else {
        if (!isTRUE(show_inputs) && isTRUE(show_outputs)) "left"
        else if (isTRUE(show_inputs) && !isTRUE(show_outputs)) "right"
        else "none"
      }

      # Process box. Rounding keeps the same bounding box, so nothing inside
      # moves -- but `inp` sits only 0.02 npc in from the left edge (and `out`
      # mirrors it), so a radius much past that starts eating those labels.
      if (identical(open_side, "none")) {
        # Closed box: ONE grob, exactly as before. Kept on its own path so the
        # common case is byte-for-byte unchanged.
        if (box_round > 0) {
          grid::grid.roundrect(
            x = 0.5, y = 0.5,
            width = box_width,
            height = box_height,
            r = grid::unit(box_round, "npc"),
            gp = gpar(fill = box_fill, col = box_border, lty = "solid",
                      lwd = box_lwd)
          )
        } else {
          grid::grid.rect(
            x = 0.5, y = 0.5,
            width = box_width,
            height = box_height,
            gp = gpar(fill = box_fill, col = box_border, lty = "solid",
                      lwd = box_lwd)
          )
        }
      } else {
        # Open edge: fill with no border, then stroke the three closed edges.
        # Top and bottom span the full width, so only the vertical edge is
        # missing and the box keeps its exact footprint.
        l <- 0.5 - box_width / 2; r <- 0.5 + box_width / 2
        b <- 0.5 - box_height / 2; t <- 0.5 + box_height / 2
        grid::grid.rect(
          x = 0.5, y = 0.5, width = box_width, height = box_height,
          gp = gpar(fill = box_fill, col = NA)
        )
        keep_x <- if (identical(open_side, "left")) r else l
        grid::grid.segments(
          x0 = c(l, l, keep_x), y0 = c(t, b, b),
          x1 = c(r, r, keep_x), y1 = c(t, b, t),
          gp = gpar(col = box_border, lty = "solid", lwd = box_lwd)
        )
      }

      # Process description subtitle
      # browser()
      if (!is_empty(process_desc) && process_desc != "") {
        txt_x <- 0.5 + box_height / 2 + spacing_bw_titles +
          font_in_npc(process_desc_fontsize) / 2
        # txt_x <- 0.5 + box_height / 2 + .05
        grid::grid.text(
          process_desc,
          x = 0.5,
          y = txt_x,
          gp = gpar(fontsize = process_desc_fontsize, just = c("center", "bottom"))
        )
      } else {
        txt_x <- 0.5 + box_height / 2
      }

      # Process label
      grid::grid.text(
        process_name,
        x = 0.5,
        y = txt_x + spacing_bw_titles *
          max(process_name_fontsize, process_desc_fontsize) /
          min(process_name_fontsize, process_desc_fontsize) +
          font_in_npc(process_desc_fontsize) / 2,
        gp = gpar(fontsize = process_name_fontsize)
      )

      # # Process label
      # grid::grid.text(
      #   process_name,
      #   x = 0.5,
      #   y = 0.5 + box_height / 2 +
      #     1.1 * font_in_npc(process_name_fontsize) / 2 +
      #     1.2 * font_in_npc(process_desc_fontsize),
      #   gp = gpar(fontsize = process_name_fontsize)
      # )
      #

      y_inp_use_act <- 0.5 + box_height / 2 - 0.03

      # Inputs ####

      if (show_inputs) {
        # combine all inputs
        inputs <- bind_rows(
          grouped_com_inputs,
          single_com_inputs,
          aux_inputs,
          weather_factors
        )

        # arrow colors
        inputs <- inputs |>
          filter(!is.na(ioname)) |>
          mutate(
            arrow_color = case_when(
              grepl("cinp", iotype) ~ arrow_comm_color,
              grepl("ainp", iotype) ~ arrow_aux_color,
              grepl("winp", iotype) ~ arrow_weather_color
            ),
            order = ifelse(grepl("winp", iotype), 1,
              ifelse(grepl("ainp", iotype), 2,
                ifelse(is.na(group), 3, 4)
              )
            )
          ) |>
          arrange(order, desc(group), desc(ioname))

        # One arrow per commodity: a commodity carrying several inp-side
        # parameters (e.g. cinp2use + cinp2ginp) must not draw twice; its
        # parameter labels stack into one multi-line in-box label.
        inputs <- inputs |>
          group_by(ioname) |>
          mutate(lab_par = paste(unique(lab_par[!is.na(lab_par)]),
                                 collapse = "\n")) |>
          slice_head(n = 1) |>
          ungroup() |>
          arrange(order, desc(group), desc(ioname))
        # inputs

        if (is.null(inputs[["label_hjust"]])) inputs$label_hjust <- 0
        if (is.null(inputs[["label_vjust"]])) inputs$label_vjust <- 0
        if (is.null(inputs[["label_font"]])) inputs$label_font <- 6
        if (is.null(inputs[["x"]])) inputs$x <- NA
        if (is.null(inputs[["y"]])) inputs$y <- NA

        n_inputs <- nrow(inputs)
        inp_coords <- list() # Store coordinates where arrows touch the box

        if (length(n_inputs) > 0) {
          if (show_iuao_labels) {
            # Add 'inp' label
            grid::grid.text(
              label = "inp",
              x = 0.5 - box_width / 2 + 0.02,
              # y = y_end + 0.02,
              y = y_inp_use_act,
              just = "bottom",
              gp = gpar(fontsize = 8)
            )
          }

          for (i in seq_len(n_inputs)) {
            # x and y position of the input on the process box
            x_pos <- 0.5 - box_width * 0.5
            y_pos <- 0.5 + (i - (n_inputs + 1) / 2) * (box_height / (n_inputs + 1))
            inputs$x[i] <- x_pos
            inputs$y[i] <- y_pos

            # draw arrow i
            grid::grid.lines(
              x = c(0.5 - 0.5 * box_width - arrow_length, x_pos),
              y = c(y_pos, y_pos),
              arrow = grid::arrow(
                type = "closed", angle = 15,
                length = grid::unit(0.15, "inches"),
                ends = "last"
              ),
              gp = gpar(col = inputs$arrow_color[i], lwd = 2)
            )

            # Centred along the arrow, sitting just above the line
            .draw_arrow_label(
              arrow_labels[inputs$ioname[i]],
              x0 = 0.5 - 0.5 * box_width - arrow_length, x1 = x_pos,
              y_line = y_pos, side = "input"
            )

            # combustion point
            # grid::grid.points(
            #   x = x_pos,
            #   y = y_pos, pch = 16,
            #   gp = gpar(col = inputs$arrow_color[i], cex = 0.1)
            # )

            # Add label near the dot, inside the box
            grid::grid.text(
              inputs$lab_par[i],
              x = 0.5 - box_width * 0.48 + inputs$label_hjust[i],
              y = y_pos + inputs$label_vjust[i] + .00,
              just = "left",
              gp = gpar(fontsize = inputs$label_font[i])
            )
          } # end for (i in seq_len(n_inputs))

          inp_arrow_spacing <- diff(inputs$y) |> mean(na.rm = TRUE)
          if (is.nan(inp_arrow_spacing) | is.na(inp_arrow_spacing)) {
            inp_arrow_spacing <- 0.1
          }

          # Draw grouping brackets for inputs
          ginp <- inputs |> filter(!is.na(group))

          if (nrow(ginp) > 1) {
            for (g in unique(ginp$group)) {
              ii <- which(ginp$group == g)
              if (length(ii) == 1) {
                warning("Group with only one input", ginp$ioname[ii])
                next
              }

              y1 <- min(ginp$y[ii]) - .4 * inp_arrow_spacing
              y2 <- max(ginp$y[ii]) + .4 * inp_arrow_spacing
              bracket_x <- 0.5 - box_width * 0.23

              # group bracket ####
              grid::grid.lines(
                x = c(
                  bracket_x,
                  bracket_x
                ),
                y = c(y1, y2),
                gp = gpar(lwd = 1.25, col = "red3")
              )

              grid::grid.lines(
                x = c(
                  bracket_x - box_width * 0.02,
                  bracket_x
                ),
                y = c(y1, y1),
                gp = gpar(lwd = 1.25, col = "red3")
              )
              grid::grid.lines(
                x = c(
                  bracket_x - box_width * 0.02,
                  bracket_x
                ),
                y = c(y2, y2),
                gp = gpar(lwd = 1.25, col = "red3")
              )

              # group circle ####
              circle_y <- (y1 + y2) / 2
              circle_x <- bracket_x
              grid::grid.circle(
                x = circle_x, y = circle_y, r = grid::unit(0.07, "inches"),
                gp = gpar(fill = "white", col = "red3", lwd = 1.0)
              )
              # group number ####
              grid::grid.text(
                label = g,
                x = circle_x,
                y = circle_y,
                gp = gpar(fontsize = 8)
              )

              # ginp2use ####
              # stop()
              ginp2use <- grouped_com_inputs |>
                filter(group == g, parameter == "ginp2use")
              if (nrow(ginp2use) == 1) {
                grid::grid.text(
                  label = ginp2use$lab_par,
                  x = circle_x + 0.048,
                  y = circle_y,
                  just = "center",
                  gp = gpar(fontsize = 6)
                )
              } else if (nrow(ginp2use) > 1) {
                stop("More than one ginp2use labels for group", g)
              }
            } # end for (g in unique(ginp$group))
            # Add 'ginp' label
            grid::grid.text(
              label = "ginp",
              x = bracket_x,
              # y = y_end + 0.02,
              y = y_inp_use_act,
              just = "bottom",
              gp = gpar(fontsize = 8)
            )
          } # end of groups

          ## "use" bracket #####
          if (show_use_bar) {
            cinp <- inputs |> filter(grepl("cinp", iotype))
            y_use <- range(cinp$y, na.rm = TRUE) + 0.4 * inp_arrow_spacing * c(-1, 1)

            use_x <- 0.5 - box_width * 0.00
            grid::grid.lines(
              x = c(use_x, use_x),
              y = y_use,
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )
            grid::grid.lines(
              x = c(use_x - box_width * 0.02, use_x),
              y = c(y_use[1], y_use[1]),
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )
            grid::grid.lines(
              x = c(use_x - box_width * 0.02, use_x),
              y = c(y_use[2], y_use[2]),
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )

            ## "use" label ####
            if (show_iuao_labels) {
              grid::grid.text(
                label = "use",
                x = use_x,
                y = y_inp_use_act,
                just = "bottom",
                gp = gpar(fontsize = 8)
              )
            }
          }
        } # end of (length(n_inputs) > 0)
      } # end of show_inputs

      # Outputs ####
      if (show_outputs) {
        # browser()
        # combine all outputs
        outputs <- bind_rows(
          grouped_com_outputs,
          single_com_outputs,
          aux_outputs
        )

        # arrow colors
        outputs <- outputs |>
          filter(!is.na(ioname)) |>
          mutate(
            arrow_color = case_when(
              grepl("cout", iotype) ~ arrow_comm_color,
              grepl("aout", iotype) ~ arrow_aux_color
            ),
            order = ifelse(grepl("aout", iotype), 1,
              if_else(is.na(group), 2, 3)
            )
          ) |>
          arrange(order, desc(group), desc(ioname))

        if (is.null(outputs[["label_hjust"]])) outputs$label_hjust <- 0
        if (is.null(outputs[["label_vjust"]])) outputs$label_vjust <- 0
        if (is.null(outputs[["label_font"]])) outputs$label_font <- 6
        if (is.null(outputs[["x"]])) outputs$x <- NA
        if (is.null(outputs[["y"]])) outputs$y <- NA

        out_coms <- outputs$ioname |> unique()
        out_pars <- outputs |>
          filter(grepl("out|teff", parameter) |
            (is.na(group) & grepl("cinp2use|use2cact|imp|sup|trade", parameter)))
        n_outputs <- length(out_coms)

        # One arrow per commodity: a commodity carrying several out-side
        # parameters (e.g. use2cact + cact2cout) must not draw twice; its
        # parameter labels stack into one multi-line in-box label. Keep
        # out_coms order so arrows stay inside the act bracket.
        out_pars <- out_pars |>
          group_by(ioname) |>
          mutate(lab_par = paste(unique(lab_par[!is.na(lab_par)]),
                                 collapse = "\n")) |>
          slice_head(n = 1) |>
          ungroup()
        out_pars <- out_pars[match(out_coms, out_pars$ioname), , drop = FALSE]
        out_pars <- out_pars[!is.na(out_pars$ioname), , drop = FALSE]

        if (length(n_outputs) > 0) {
          stopifnot(nrow(out_pars) == n_outputs)

          if (show_iuao_labels) {
            # Add 'out' label
            grid::grid.text(
              label = "out",
              x = 0.5 + box_width / 2 - 0.02,
              y = y_inp_use_act,
              just = "bottom",
              gp = gpar(fontsize = 8)
            )

            # add 'act' label
            grid::grid.text(
              label = "act",
              x = 0.5 + box_width * 0.22,
              y = y_inp_use_act,
              just = "bottom",
              gp = gpar(fontsize = 8)
            )
          }

          for (i in seq_len(n_outputs)) {
            ii <- which(outputs$ioname == out_coms[i]) # can be several parameters

            # x and y position of the output on the process box
            x_pos <- 0.5 + box_width * 0.5
            y_pos <- 0.5 + (i - (n_outputs + 1) / 2) * (box_height / (n_outputs + 1))
            outputs$x[ii] <- x_pos
            outputs$y[ii] <- y_pos

            # draw arrow o
            # browser()
            grid::grid.lines(
              x = c(x_pos, 0.5 + 0.5 * box_width + arrow_length),
              y = c(y_pos, y_pos),
              arrow = grid::arrow(
                type = "closed", angle = 15,
                length = grid::unit(0.15, "inches"),
                ends = "last"
              ),
              gp = gpar(col = out_pars$arrow_color[i], lwd = 2)
            )

            # Centred along the arrow, sitting just above the line
            .draw_arrow_label(
              arrow_labels[out_pars$ioname[i]],
              x0 = x_pos, x1 = 0.5 + 0.5 * box_width + arrow_length,
              y_line = y_pos, side = "output"
            )

            # Add label near the dot, inside the box
            grid::grid.text(
              out_pars$lab_par[i],
              x = 0.5 + box_width * 0.48 + out_pars$label_hjust[i],
              y = y_pos + out_pars$label_vjust[i] + .00,
              just = "right",
              gp = gpar(fontsize = out_pars$label_font[i])
            )
          } # end for (i in seq_len(n_outputs))

          out_arrow_spacing <- diff(unique(outputs$y)) |> mean(na.rm = TRUE)

          # Draw grouping brackets for outputs
          gout <- outputs |>
            filter(!is.na(group)) |>
            select(group, ioname, y, lab_txt, x, y)

          if (nrow(gout) > 1) {
            for (g in unique(gout$group)) {
              ii <- which(gout$group == g)
              if (length(ii) == 1) {
                warning("Group with only one output", gout$ioname[ii])
                next
              }

              y1 <- min(gout$y[ii], na.rm = TRUE) - .4 * out_arrow_spacing
              y2 <- max(gout$y[ii], na.rm = TRUE) + .4 * out_arrow_spacing
              bracket_x <- 0.5 + box_width * 0.25

              # group bracket ####
              grid::grid.lines(
                x = c(
                  bracket_x,
                  bracket_x
                ),
                y = c(y1, y2),
                gp = gpar(lwd = 1.25, col = "red3")
              )

              grid::grid.lines(
                x = c(
                  bracket_x + box_width * 0.02,
                  bracket_x
                ),
                y = c(y1, y1),
                gp = gpar(lwd = 1.25, col = "red3")
              )
              grid::grid.lines(
                x = c(
                  bracket_x + box_width * 0.02,
                  bracket_x
                ),
                y = c(y2, y2),
                gp = gpar(lwd = 1.25, col = "red3")
              )

              # group circle ####
              circle_y <- (y1 + y2) / 2
              circle_x <- bracket_x
              grid::grid.circle(
                x = circle_x, y = circle_y, r = grid::unit(0.07, "inches"),
                gp = gpar(fill = "white", col = "red3", lwd = 1.0)
              )
              # group number ####
              grid::grid.text(
                label = g,
                x = circle_x,
                y = circle_y,
                gp = gpar(fontsize = 8)
              )
            }
          } # end of groups

          ## "act" bracket ####
          if (show_act_bar) {
            cout <- outputs |>
              filter(grepl("cout", iotype))
            y_act <- range(cout$y, na.rm = TRUE) + 0.42 * out_arrow_spacing * c(-1, 1)
            act_x <- 0.5 + box_width * 0.2

            grid::grid.lines(
              x = c(act_x, act_x),
              y = y_act,
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )
            grid::grid.lines(
              x = c(act_x + box_width * 0.02, act_x),
              y = c(y_act[1], y_act[1]),
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )
            grid::grid.lines(
              x = c(act_x + box_width * 0.02, act_x),
              y = c(y_act[2], y_act[2]),
              gp = gpar(lwd = 1.25, col = "red3", lty = "solid")
            )
          } # end of show_act_bar

          ## use2cact labels ####
          # browser()
          if (show_act_bar && show_use_bar &&
            any(grepl("use2cact", outputs$parameter))) {
            use2cact <- outputs |>
              filter(grepl("use2cact", parameter)) |>
              filter(!is.na(group))

            use2cact_x <- (act_x + use_x) / 2

            if (nrow(use2cact) > 0) {
              for (com in unique(use2cact$ioname)) {
                ii <- which(use2cact$ioname == com)

                grid::grid.text(
                  label = use2cact$lab_par[ii],
                  x = use2cact_x,
                  y = use2cact$y[ii],
                  just = "center",
                  gp = gpar(fontsize = 6)
                )
              } # end of for loop
            } # end of (nrow(use2cact) > 0)
          } # end of show_act_bar && show_use_bar
        } # end of (length(n_outputs) > 0)
      } # end of show_outputs

      # cap2act ####
      if (!is.null(center_label)) {
        grid::grid.text(
          label = center_label,
          x = 0.5,
          y = 0.5 - box_height / 2 + 0.05,
          just = "center",
          gp = gpar(fontsize = 6)
        )
      } # end of cap2act

      return(invisible(TRUE))
    },
    error = function(e) {
      message("Error in draw_process: ", e)
      # `on.exit()` above already pops the viewport this function pushed; do not
      # unwind the caller's stack as well.
      return(invisible(FALSE))
    }
  )
} # end of draw_process

# # dev.off()
# if (F) {
#   "#DCE6F2"
#   "#F0FFF0"
#   "#E6E6FA"
#   "#F0F8FF"
#   "#228B22"
#   "#FFD700"
#   "#556B2F" # Dark Olive Green
#   "#6B8E23"
#   "#808000"
#   "#556B2F"
#   "#B8860B" # Dark Goldenrod
#   "#DAA520"
#   "#708090"
#   "#2F4F4F"
#   "#FF6347"
#   "#FF4500"
#   "#FFA07A"
#   "#FFA500"
#   "#FFD700"
#   "#FF8C00"
# }



# Function to shorten a string
shorten_string <- function(string, n, add_number = NULL) {
  if (nchar(string) > n) {
    # Subtract 2 from n to account for the ".."
    shortened <- substr(string, 1, n)
    if (nchar(string) > n) {
      if (!is.null(add_number)) {
        shortened <- paste0(shortened, "..(", add_number, ")")
      } else {
        shortened <- paste0(shortened, "..")
      }
    }
    return(shortened)
  } else {
    return(string)
  }
}

# format_number <- function(x) {
#   # browser()
#   # if (length(x) > 1) {
#   #   return(sapply(x, format_number))
#   # }
#   # Use scientific notation if the number is larger than 100 or smaller than 0.01 (positive or negative)
#   if (any(abs(x) > 100) || any(abs(x) < 0.01)) {
#     return(prettyNum(format(x, scientific = TRUE), big.mark = ","))
#   } else {
#     return(prettyNum(format(x, nsmall = 2), big.mark = ","))
#   }
# }

# x <- c(0.01224201, 1002360.1)

# Define a function for conditional formatting
# format_number <- function(x, threshold = 1e5, small_threshold = 1e-3) {
#   sapply(x, function(n) {
#     if (abs(n) >= threshold || (abs(n) < small_threshold && n != 0)) {
#       # Use scientific notation for very large or very small numbers
#       formatC(n, format = "e", digits = 3)
#     } else {
#       # Use fixed decimal format
#       formatC(n, format = "f", digits = 3)
#     }
#   })
# }

# format_number <- function(x, accuracy = 3) {
#   scales::label_number(accuracy = accuracy)(x)
# }


format_number <- function(x, accuracy = .01) {
  sapply(x, function(value) {
    if (is.na(value) || is.nan(value) || is.infinite(value)) {
      return(value)
    } else if (value == 0) {
      "0"
    } else if (value == 1) {
      "1"
    } else if (value >= 1e4) {
      # For large numbers, show fewer digits with scale suffix
      scales::label_scientific(accuracy = 0)(value)
    } else if (value <= accuracy) {
      # For smaller numbers, allow more precision
      scales::label_scientific(accuracy = accuracy)(value)
    } else {
      scales::label_number(accuracy = accuracy, big.mark = "")(value)
    }
  })
}
# # Apply the function
# formatted_x <- format_numbers(x)
# print(formatted_x)


# Example usage
# numbers <- c(45.678, 12345.678, -12345.678, 0.1234, 0.009, -0.008, 1e8)

# sapply(numbers, format_number)
# print(formatted_numbers)
