## Auto-generates R/recast-rules.R from the `recast:` blocks in classes.yml.
## Runnable on its own; also called from data-raw/DATASET.R.
y <- yaml::read_yaml("data-raw/classes.yml")
rows <- list()
for (cn in names(y$class)) for (sn in names(y$class[[cn]])) {
  s <- y$class[[cn]][[sn]]
  if (!is.list(s) || is.null(s$columns)) next
  for (co in names(s$columns)) {
    r <- s$columns[[co]]$recast
    if (is.null(r)) next
    rows[[length(rows) + 1L]] <- data.frame(
      class = cn, slot = sn, column = co,
      space = if (is.null(r$space)) NA_character_ else r$space,
      time  = if (is.null(r$time))  NA_character_ else r$time,
      stringsAsFactors = FALSE)
  }
}
d <- do.call(rbind, rows)
d <- d[order(d$class, d$slot, d$column), ]

# Row-wise and column-aligned, so a rule reads off the file at a glance
# instead of counting positions across five parallel vectors.
lit <- function(x) ifelse(is.na(x), "NA", paste0('"', x, '"'))
pad <- function(x, w) paste0(x, strrep(" ", pmax(0L, w - nchar(x))))

f <- list(lit(d$class), lit(d$slot), lit(d$column), lit(d$space), lit(d$time))
f[1:4] <- lapply(f[1:4], function(z) paste0(z, ","))
w <- vapply(f, function(z) max(nchar(z)), integer(1))
labs <- c("class", "slot", "column", "space", "time")

body <- paste0("  ", pad(f[[1]], w[1]), " ", pad(f[[2]], w[2]), " ",
               pad(f[[3]], w[3]), " ", pad(f[[4]], w[4]), " ", f[[5]])
body[-length(body)] <- paste0(body[-length(body)], ",")
hdr <- paste0("  # ", pad(labs[1], w[1] - 2L), " ", pad(labs[2], w[2]), " ",
              pad(labs[3], w[3]), " ", pad(labs[4], w[4]), " ", labs[5])

readr::write_lines(c(
  "# Auto-created from the `recast:` blocks of data-raw/classes.yml;",
  "# see data-raw/recast_rules.R. Do not edit by hand.",
  "",
  "#' Declared recast rule per class, slot and column",
  "#' @noRd",
  ".defRecast <- .recast_rules(",
  hdr,
  body,
  ")"
), file = "R/recast-rules.R")
cat("wrote R/recast-rules.R with", nrow(d), "rules\n")
