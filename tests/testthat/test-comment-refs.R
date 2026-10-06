# Every `R/<file>.R` named in a comment must exist.
#
# The Sept-Oct 2025 regrouping merged ~45 files into 107 and left ~30 comments
# pointing at paths that no longer existed -- including a `file:line` citation
# into a deleted file. Nothing noticed, because a comment cannot fail. This does.
#
# Two kinds of reference are legitimate and are NOT failures:
#   * provenance, `# (was R/map_value.R, merged 2026-09-20)` -- a true statement
#     about history, left at the seam where a file was merged in. Judged per LINE,
#     because it is a per-line idiom.
#   * a path in another tree -- multimod, IDEEA, depreciated/R/. Judged per
#     comment BLOCK, because such a note names the tree once and then lists
#     several files over the following lines.

# Covers the R API: nothing -- this guards comments, not code.
# NOT an `@covers` tag: see the note in test-getdata-run.R.

.cr_blocks <- function(lines) {
  # contiguous runs of comment lines, as (start, end) index pairs
  is_c <- grepl("^[[:space:]]*#", lines)
  out <- list(); i <- 1L
  while (i <= length(lines)) {
    if (!is_c[[i]]) { i <- i + 1L; next }
    j <- i
    while (j < length(lines) && is_c[[j + 1L]]) j <- j + 1L
    out[[length(out) + 1L]] <- c(i, j)
    i <- j + 1L
  }
  out
}

test_that("every R/<file>.R named in a comment exists", {
  rdir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(rdir), "package R/ not reachable from the test dir")
  have <- list.files(rdir, pattern = "[.]R$")

  bad <- character()
  for (fn in have) {
    lines <- readLines(file.path(rdir, fn), warn = FALSE)
    for (b in .cr_blocks(lines)) {
      blk <- paste(lines[b[[1]]:b[[2]]], collapse = " ")
      # a note about another tree names it once for the whole block
      if (grepl("multimod|IDEEA|depreciated/R/", blk)) next
      for (i in b[[1]]:b[[2]]) {
        ln <- lines[[i]]
        if (grepl("(was R/", ln, fixed = TRUE)) next          # provenance
        m <- regmatches(ln, gregexpr("R/[A-Za-z0-9_.-]+[.]R(?![A-Za-z])", ln,
                                     perl = TRUE))[[1]]
        for (ref in m)
          if (!sub("^R/", "", ref) %in% have)
            bad <- c(bad, paste0(fn, ":", i, "  -> ", ref))
      }
    }
  }
  # One accurate exception: the comment states that the file IS deleted.
  bad <- bad[!grepl("^solve[.]R:", bad)]

  expect_equal(length(bad), 0L,
               label = paste0(length(bad), " dangling R/ reference(s): ",
                              paste(utils::head(bad, 20), collapse = " | ")))
})
