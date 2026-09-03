# Extracted from test-draw-open.R:152

# prequel ----------------------------------------------------------------------
do_repo <- function() utopia$modules$electricity$R3$repo
do_obj  <- function(cl) {
  o <- getObject(do_repo(), class = cl, drop = FALSE)
  if (length(o) == 0L) NULL else o[[1]]
}
do_kinds <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  force(expr)
  table(sub("[.][0-9]+$", "", grid::grid.ls(print = FALSE)$name))
}
do_n <- function(k, what) if (what %in% names(k)) as.integer(k[[what]]) else 0L

# test -------------------------------------------------------------------------
o <- do_obj("storage")
skip_if(is.null(o), "no storage in the fixture repo")
grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)
grid::grid.newpage()
draw(o)
nm <- grid::grid.ls(print = FALSE)$name
labs <- vapply(grep("^GRID.text", nm, value = TRUE),
                 function(n) paste(grid::grid.get(n)$label, collapse = ""),
                 character(1))
cap <- grep("duration", labs, value = TRUE)
expect_length(cap, 1L)
