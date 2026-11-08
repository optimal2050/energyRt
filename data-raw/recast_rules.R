## Auto-generates R/recast-rules.R from the recast rules in parameters.yml.
## Runnable on its own; also called from data-raw/DATASET.R, which sources
## data-raw/col_param.R first.
##
## `.defRecast` is keyed class/slot/column, and `.col_param` has already done
## the one hard part -- deciding which parameter owns which declared column,
## through class inheritance and `colAlias`. So each rule is read off the owning
## parameter (one rule covers its .lo/.up/.fx columns) or, for a column no
## parameter owns, off the `columns:` section.
if (!exists(".col_param", inherits = TRUE)) {
  source("data-raw/col_param.R")
}
p <- yaml::read_yaml("data-raw/parameters.yml")
orphan <- p$columns
p$columns <- NULL

rows <- list()
for (i in seq_len(nrow(.col_param))) {
  cp <- .col_param[i, ]
  r <- if (is.na(cp$parameter)) {
    orphan[[paste(cp$class, cp$slot, cp$column, sep = "/")]]$recast
  } else {
    p[[cp$parameter]]$recast
  }
  if (is.null(r)) next
  # the key is the class whose schema DECLARES the column, which is the class
  # an object of that shape reports; NA means no `columns:` block documents it
  if (is.na(cp$declared_on)) {
    stop("recast rule on a column no class declares: ",
         cp$class, "/", cp$slot, "/", cp$column)
  }
  rows[[length(rows) + 1L]] <- data.frame(
    class = cp$declared_on, slot = cp$slot, column = cp$column,
    space = if (is.null(r$space)) NA_character_ else r$space,
    time  = if (is.null(r$time))  NA_character_ else r$time,
    stringsAsFactors = FALSE)
}
d <- do.call(rbind, rows)
d <- d[order(d$class, d$slot, d$column), ]
stopifnot(!anyDuplicated(paste(d$class, d$slot, d$column, sep = "/")))

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
  "# Auto-created from the recast rules of data-raw/parameters.yml;",
  "# see data-raw/recast_rules.R. Do not edit by hand.",
  "",
  "#' @include recast.R",
  "NULL",
  "",
  "#' Declared recast rule per class, slot and column",
  "#' @noRd",
  ".defRecast <- .recast_rules(",
  hdr,
  body,
  ")"
), file = "R/recast-rules.R")
cat("wrote R/recast-rules.R with", nrow(d), "rules\n")
