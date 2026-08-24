# save_scenario() records every save in the project registry
# (get_registry_file(), default `energyRt_registry.csv` in the working
# directory). Point it into tempdir() for the whole suite so test saves never
# leave a registry file in tests/testthat/.
set_registry_file(file.path(tempdir(), "energyRt_registry_tests.csv"))
