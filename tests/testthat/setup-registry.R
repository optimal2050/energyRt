# save_scenario() records every save in the project registry
# (get_registry_file(), default `energyRt_registry.csv` in the working
# directory). Point it into tempdir() for the whole suite so test saves never
# leave a registry file in tests/testthat/.
set_registry_file(file.path(tempdir(), "energyRt_registry_tests.csv"))

# save_scenario() also writes the model store now (embed_model = NULL stores
# the model and references it), and models_path defaults to a project-relative
# `models/`. Redirect it for the same reason.
set_models_path(file.path(tempdir(), "energyRt_models_tests"))

# The temporary tiers for derived artifacts of in-memory objects default to
# project-relative `levcosts/` and `reports/`; during tests the working
# directory is tests/testthat/, so point both into tempdir() as well.
set_levcost_cache_path(file.path(tempdir(), "energyRt_levcosts_tests"))
set_reports_path(file.path(tempdir(), "energyRt_reports_tests"))

# Repositories and datasets are project-relative by the same default rule and
# were missed above. Latent rather than live -- no test writes them at the
# default path today -- but they belong with their four siblings.
set_repositories_path(file.path(tempdir(), "energyRt_repositories_tests"))
set_datasets_path(file.path(tempdir(), "energyRt_datasets_tests"))

# Scenarios: the one shared write target the redirects above missed, and the
# reason `dev/TESTING.md` forbade running two test processes at once. Scenario
# names are deterministic per test (`ff_<family>_<case>`, `st_<tier>`) and the
# fork helpers pass overwrite = TRUE / force = TRUE, so two processes silently
# overwrote each other's scenario directory mid-solve -- the documented
# "phantom failures". tempdir() is per-SESSION and stable for the whole run, so
# caches that reuse a scenario across test FILES (solved_tier(),
# .fork_solve()) keep working, while two runs can no longer see each other.
#
# It also stops the accumulation: this directory had reached 818 MB over 697
# entries in the working tree, invisible to git status because it is ignored.
#
# ENERGYRT_TEST_SCENARIOS pins it instead. That matters for debugging, not
# convenience: a tempdir scenario dies with the session, so without a pin,
# isolation makes post-mortem inspection of a failed solve harder than before.
# A pinned directory never self-cleans.
set_scenarios_path(local({
  pin <- Sys.getenv("ENERGYRT_TEST_SCENARIOS", "")
  if (nzchar(pin)) pin else file.path(tempdir(), "energyRt_scenarios_tests")
}))
