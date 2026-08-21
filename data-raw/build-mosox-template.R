# Generate the mosox MathProg template from the GLPK one ######################
#
# mosox (https://github.com/, `mosox --version` 0.6.2) is a Rust GMPL matrix
# generator with HiGHS attached. Its MathProg dialect is a SUBSET of glpsol's,
# so `glpk/energyRt.mod` does not compile under it. Rather than maintain a
# second 1200-line template by hand -- which would drift from the GLPK one on
# the first equation edit -- the mosox variant is DERIVED from it here, by a
# small set of mechanical rewrites.
#
# `glpk/energyRt.mod` is never modified. Run this from the package root:
#
#   source("data-raw/build-mosox-template.R")
#
# `data-raw/DATASET.R` sources it before building `.modelCode`, and
# test-mosox.R asserts the committed output still matches what this produces.
#
# Every rewrite below was isolated in a minimal .mod against mosox 0.6.2 and is
# quoted with the exact error it fixes. When a rewrite becomes unnecessary
# (mosox gains the construct), delete the block -- do not silently keep it.
###############################################################################

.mosox_src <- "glpk/energyRt.mod"
.mosox_out <- "glpk/energyRt-mosox.mod"

build_mosox_template <- function(src = .mosox_src, out = .mosox_out,
                                 write = TRUE) {
  code <- readLines(src)
  n0 <- length(code)

  ## -- 1. no `prod` aggregate -------------------------------------------------
  # mosox: "expected subscript, add, sub, mul, div, pow, rel_op, or COMMENT"
  #
  # The 16 weather terms are the model's only iterated products:
  #   prod{wth1 in weather:((wth1,t) in mTechWeatherAfLo)}
  #       (pTechWeatherAfLo[wth1,t]*pWeather[wth1,r,y,s])
  #
  # Over a set of AT MOST ONE element, prod(B) == 1 + sum(B - 1):
  #   0 elements -> 1 + 0        = 1   (the empty product)
  #   1 element  -> 1 + (b - 1)  = b
  # Two elements would give 1+b1+b2-2, NOT b1*b2 -- so the mosox write path
  # refuses a model with several weather factors on one object; see
  # `.check_single_weather()` in R/mosox.R. Every other backend keeps the real
  # product (NEWS.md, "prod() over several weather factors").
  #
  # Safe as a regex: no `}` occurs inside the domain and no `)` inside the body.
  prod_n <- sum(vapply(gregexpr("prod\\{", code), function(m) sum(m > 0), 1L))
  code <- gsub("prod\\{([^}]*)\\}\\(([^)]*)\\)", "(1 + sum{\\1}(\\2 - 1))", code)

  ## -- 2. no implicit dummy index ---------------------------------------------
  # mosox: "Mismatched tuple/non-tuple indexes" (at matrix generation)
  #
  # `sum{FORIF: cond}(expr)` uses MathProg's implicit dummy over the one-element
  # set FORIF as an "include this term if cond" switch. mosox needs the dummy
  # spelled out. `fi` is unused in the body, exactly as the implicit one was.
  forif_n <- sum(vapply(gregexpr("sum\\{FORIF:", code), function(m) sum(m > 0), 1L))
  code <- gsub("sum{FORIF:", "sum{fi in FORIF:", code, fixed = TRUE)

  ## -- 3. no scalar set membership --------------------------------------------
  # mosox: "expected set_infix_op, subscript, add, sub, ..., subset_op"
  #
  # `t1 in mTradeOlifeInf` tests membership of a scalar in a 1-dimension set.
  # mosox supports the tuple form `(a,b) in M` only; the 1-tuple `(t1) in S`
  # fails too. Rewritten as a counting test over the same set.
  # `mTradeOlifeInf` is the model's ONLY scalar membership test -- every other
  # `dimen 1` set appears as an iteration domain, which mosox handles.
  memb_n <- sum(vapply(gregexpr("\\bt1 in mTradeOlifeInf\\b", code),
                       function(m) sum(m > 0), 1L))
  code <- gsub("\\bt1 in mTradeOlifeInf\\b",
               "(sum{t1x in mTradeOlifeInf : t1x = t1} 1) > 0", code)

  ## -- 4. no leading unary plus -----------------------------------------------
  # mosox: "expected expr or COMMENT", pointing at the `+` in `eqCost`'s
  # `vTotalCost[r,y]  =  +sum{...}`. Give the `+` a left operand.
  unary_n <- sum(vapply(gregexpr("=(\\s+)\\+", code), function(m) sum(m > 0), 1L))
  code <- gsub("=(\\s+)\\+", "=\\10+", code)

  ## -- 5. cannot subtract a variable from a constant --------------------------
  # mosox: "no vars allowed in expr sub" (at matrix generation)
  #
  # mosox 0.6.2 evaluates the left operand of `-` first and cannot then negate a
  # variable expression on the right. Measured:
  #   p[y] - z[y]              FAIL      z[y] - sum{..}(z[..])   OK
  #   p[y] - sum{..}(z[..])    FAIL      p[y] + sum{..}(z[..])   OK
  #   0 - sum{..}(z[..])       FAIL      -sum{..}(z[..])         OK  (unary)
  # so only CONSTANT-minus-VARIABLE breaks; var-minus-anything and unary minus
  # are fine. `a - b` becomes the identical `a + (-1)*b`.
  #
  # Today this matches exactly one site, `eqTechCap`'s
  #   pTechStock[t,r,y]-sum{fi in FORIF: ...}(vTechRetiredStockCum[t,r,y])
  # The pattern is written generally (any parameter reference followed by `-`
  # and a variable expression) so an equation edit that introduces another one
  # is fixed rather than silently reintroducing the error. The template's other
  # subtractions all have a variable on the left and are left alone.
  minus_n <- sum(vapply(gregexpr("(p[A-Za-z0-9]*\\[[^]]*\\])-(sum\\{|v[A-Z])", code),
                        function(m) sum(m > 0), 1L))
  code <- gsub("(p[A-Za-z0-9]*\\[[^]]*\\])-(sum\\{|v[A-Z])", "\\1+(-1)*\\2", code)

  ## -- 6. drop the post-solve output block ------------------------------------
  # mosox parses the `printf`/`for` block after `solve;` but never EXECUTES it:
  # no output/*.csv is produced (verified on a 2-variable model). energyRt reads
  # GLPK results through those 234 statements, so on mosox there is nothing to
  # read back either way -- see the "Out of scope" note in the plan. Carrying
  # ~400 dead lines only slows the parser and risks generation errors on
  # constructs (variables inside `for{}` conditions) mosox cannot evaluate.
  #
  # `.write_model_GLPK_CBC()` slices `run_code` on three markers -- the uuid
  # comment, `^minimize` and `^end;` -- so all three must survive.
  i_solve <- grep("^solve;", code)
  i_end <- grep("^end;", code)
  if (length(i_solve) != 1L || length(i_end) < 1L || i_end[1] <= i_solve) {
    stop("Cannot locate the post-solve block in ", src,
         ": expected exactly one `solve;` followed by `end;`.")
  }
  dropped <- i_end[1] - i_solve - 1L
  code <- code[-seq.int(i_solve + 1L, i_end[1] - 1L)]

  stopifnot(
    "no `prod{}` left"          = !any(grepl("prod{", code, fixed = TRUE)),
    "no bare FORIF left"        = !any(grepl("sum{FORIF:", code, fixed = TRUE)),
    "writer markers preserved"  = length(grep("22b584bd-a17a-4fa0-9cd9-f603ab684e47", code)) == 1L &&
      length(grep("^minimize", code)) == 1L && length(grep("^end;", code)) >= 1L
  )

  message(sprintf(paste(
    "mosox template: %d -> %d lines",
    "(prod %d, FORIF %d, membership %d, unary+ %d, const-minus-var %d,",
    "post-solve %d)"),
    n0, length(code), prod_n, forif_n, memb_n, unary_n, minus_n, dropped))

  if (write) writeLines(code, out)
  invisible(code)
}

if (identical(environment(), globalenv())) build_mosox_template()
