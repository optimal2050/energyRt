mk_trade <- function(nm, comm, a, b, stock = NA_real_, up = NA_real_,
                     nup = NA_real_, inv = FALSE) {
  args <- list(nm, commodity = comm,
               routes = data.frame(src = c(a, b), dst = c(b, a)),
               capacity = data.frame(stock = stock, cap.up = up, ncap.up = nup))
  if (inv) args$invcost <- data.frame(invcost = 100)
  do.call(newTrade, args)
}

test_that("plot_trade_network draws inputs without a map", {
  skip_if_not_installed("ggplot2")
  tr <- list(mk_trade("T_AB", "ELC", "A", "B", 1000, 2000),
             mk_trade("T_BC", "ELC", "B", "C", 500),
             mk_trade("T_CD", "ELC", "C", "D", up = 1500, inv = TRUE),
             mk_trade("H_AB", "H2", "A", "B", 200))
  p <- plot_trade_network(tr)
  expect_s3_class(p, "ggplot")
  # existing links take their stock, the project its upper bound
  l <- .tn_input(tr, do.call(rbind, lapply(tr, function(t) data.frame(
    trade = t@name, comm = .trade_comm(t), src = t@routes$src, dst = t@routes$dst))), NULL)
  expect_equal(l$value[l$a == "C" & l$b == "D"], 1500)
  expect_equal(l$status[l$a == "C" & l$b == "D"], "project")
  expect_equal(l$value[l$a == "A" & l$b == "B" & l$comm == "ELC"], 1000)
  expect_equal(l$band[l$a == "A" & l$b == "B" & l$comm == "ELC"], 2000)
})

test_that("plot_trade_network refuses solved values without a scenario", {
  skip_if_not_installed("ggplot2")
  tr <- list(mk_trade("T_AB", "ELC", "A", "B", 1000))
  expect_error(plot_trade_network(tr, value = "flow"), "needs a solved scenario")
})

test_that("crossing links are bent apart", {
  # the square's two diagonals cross whatever their bend; the sides do not
  nodes <- data.frame(region = c("A", "B", "C", "D"), x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  links <- data.frame(a = c("A", "B"), b = c("C", "D"), comm = "ELC", value = 1, dir = "both")
  g <- .tn_geometry(links, nodes, curvature = 0.15)
  expect_length(g$geom, 2)
  expect_true(all(vapply(g$geom, nrow, 0L) > 2))
})
