# Solver-free invariants of the fold, on every tier fixture plus the UTOPIA R1
# layout and the two-region fold fixture, on the sparse and the dense path:
#   lossless     unfolding every folded value parameter gives the data the
#                unfolded build of the same model carries
#   whole-column a folded dimension's column is entirely wildcard; maps never
#                carry a wildcard
#   idempotent   folding a folded scenario changes nothing
#   accounted    model_size() reports exactly the rows the fold_info sums to
#   order-free   the folded dimensions do not depend on the order they were
#                requested in

.fi_models <- function() {
  env <- .mapping_fixture_env()
  m <- list(fold_fixture = function() fold_model())
  if (!is.null(env)) {
    for (nm in c("tm_core", "tm_flows", "tm_io", "tm_policy", "tm_weather")) {
      m[[nm]] <- local({ f <- env[[nm]]; function() f() })
    }
  }
  m$utopia_R1 <- function() ut_build("R1", "s4_h24")
  m
}

.fi_interp <- function(build, tag, ...) {
  suppressMessages(suppressWarnings(
    interpolate_model(build(), name = tag, ondisk = FALSE, overwrite = TRUE,
                      verbose = FALSE, ...)))
}

for (model_nm in names(.fi_models())) {
  for (sparse in c(TRUE, FALSE)) {
    local({
      mn <- model_nm; sp <- sparse
      lab <- paste0(mn, if (sp) " sparse" else " dense")
      test_that(paste0("fold invariants: ", lab), {
        skip_if_tier_below("fast")
        if (grepl("^tm_", mn)) skip_if_no_fixtures()
        build <- .fi_models()[[mn]]
        tag <- gsub("[^A-Za-z0-9_]", "_", paste0("fi_", mn, if (sp) "_s" else "_d"))
        uf <- .fi_interp(build, paste0(tag, "_uf"), fold = FALSE, sparse = sp)
        fo <- .fi_interp(build, paste0(tag, "_fo"), fold = FOLD_ALL, sparse = sp)
        folded <- fold_folded(fo)
        expect_gt(length(folded), 0L, label = paste(lab, "folds something"))

        # lossless + whole-column
        for (nm in names(folded)) {
          d <- as.data.frame(get_data_slot(fo@modInp@parameters[[nm]]))
          for (wd in folded[[nm]]$wildcard_dims) {
            expect_true(all(is.na(d[[wd]])), label = paste(lab, nm, wd, "all NA"))
          }
          a <- fold_norm(unfold_scenario_parameter(fo, fo@modInp@parameters[[nm]],
                                                   dims = FOLD_ALL))
          b <- fold_param_data(uf, nm)
          expect_equal(a, b, label = paste(lab, nm, "lossless"), ignore_attr = TRUE)
        }
        # maps never carry a wildcard on a foldable dim
        for (nm in names(fo@modInp@parameters)) {
          p <- fo@modInp@parameters[[nm]]
          if (as.character(p@type) != "map") next
          d <- as.data.frame(get_data_slot(p))
          for (wd in intersect(FOLD_ALL, names(d))) {
            expect_false(any(is.na(d[[wd]]) | is_any(d[[wd]])),
                         label = paste(lab, nm, wd, "map explicit"))
          }
        }
        # idempotent
        again <- fold_scenario_parameters(fo, dims = FOLD_ALL)
        expect_equal(energyRt:::.value_param_rows(again),
                     energyRt:::.value_param_rows(fo), label = paste(lab, "idempotent"))
        # accounted: the fold step's saving is what fold_info sums to, less the
        # rows the later trade-route expansion adds back
        ms <- model_size(fo)
        expect_equal(ms$rows_saved, ms$before_fold - ms$param_rows,
                     label = paste(lab, "rows_saved"))
        expect_lte(ms$rows_saved,
                   sum(vapply(folded, function(fi) fi$original_rows - fi$folded_rows, 0)))
        expect_gt(ms$rows_saved, 0)
      })
    })
  }
}

test_that("the folded dimensions do not depend on the fold order", {
  skip_if_tier_below("fast")
  builds <- list(fold_fixture = function() fold_model())
  env <- .mapping_fixture_env()
  if (!is.null(env)) builds$tm_core <- function() env$tm_core()
  for (mn in names(builds)) {
    a <- .fi_interp(builds[[mn]], paste0("fo_a_", mn), fold = FOLD_ALL)
    b <- .fi_interp(builds[[mn]], paste0("fo_b_", mn), fold = rev(FOLD_ALL))
    fa <- fold_folded(a); fb <- fold_folded(b)
    expect_setequal(names(fa), names(fb))
    for (nm in names(fa)) {
      expect_setequal(fa[[nm]]$wildcard_dims, fb[[nm]]$wildcard_dims)
      expect_equal(fa[[nm]]$folded_rows, fb[[nm]]$folded_rows, label = paste(mn, nm))
    }
  }
})
