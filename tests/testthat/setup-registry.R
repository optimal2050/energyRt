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
