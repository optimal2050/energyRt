# A null device for the whole run.
#
# Eleven test files call `draw()` or `autoplot()` for their side effect, and
# `draw()` plots rather than returning an object. With no device open R opens
# the DEFAULT one, which in a non-interactive session is `pdf()` writing
# `Rplots.pdf` into the working directory -- so every test run left a stray
# file in the package root, one `git add -A` away from being committed.
#
# `test-draw-cluster.R` already guards itself this way per test_that(); one
# device here covers every file and makes that local guard redundant rather
# than wrong.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), teardown_env())
