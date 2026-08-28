# energyRt Options

Internally used, package-specific options. All options will prioritize R
options() values, and fall back to environment variables if undefined.
If neither the option nor the environment variable is set, a default
value is used.

## Checking Option Values

Option values specific to `energyRt` can be accessed by passing the
package name to `env`.

    options::opts(env = "energyRt")

    options::opt(x, default, env = "energyRt")

## Options

- gams_path:

  default:

  :   NULL

  option:

  :   en.gams_path

  envvar:

  :   ENERGYRT_GAMS_PATH (evaluated if possible, raw string otherwise)

- gdxlib_path:

  default:

  :   NULL

  option:

  :   en.gdxlib_path

  envvar:

  :   ENERGYRT_GDXLIB_PATH (evaluated if possible, raw string otherwise)

- python_path:

  default:

  :   NULL

  option:

  :   en.python_path

  envvar:

  :   ENERGYRT_PYTHON_PATH (evaluated if possible, raw string otherwise)

- julia_path:

  default:

  :   NULL

  option:

  :   en.julia_path

  envvar:

  :   ENERGYRT_JULIA_PATH (evaluated if possible, raw string otherwise)

- glpk_path:

  default:

  :   NULL

  option:

  :   en.glpk_path

  envvar:

  :   ENERGYRT_GLPK_PATH (evaluated if possible, raw string otherwise)

- neos_email:

  default:

  :   NULL

  option:

  :   en.neos_email

  envvar:

  :   NEOS_EMAIL (evaluated if possible, raw string otherwise)

- neos_endpoint:

  default:

  :   "https://neos-server.org:3333"

  option:

  :   en.neos_endpoint

  envvar:

  :   ENERGYRT_NEOS_ENDPOINT (evaluated if possible, raw string
      otherwise)

- solver:

  default:

  :   list(name = "glpk", lang = "GLPK")

  option:

  :   en.solver

  envvar:

  :   ENERGYRT_SOLVER (evaluated if possible, raw string otherwise)

- scenarios_path:

  default:

  :   "scenarios/"

  option:

  :   en.scenarios_path

  envvar:

  :   ENERGYRT_SCENARIOS_PATH (evaluated if possible, raw string
      otherwise)

- registry_file:

  default:

  :   "energyRt_registry.csv"

  option:

  :   en.registry_file

  envvar:

  :   ENERGYRT_REGISTRY_FILE (evaluated if possible, raw string
      otherwise)

- models_path:

  default:

  :   "models/"

  option:

  :   en.models_path

  envvar:

  :   ENERGYRT_MODELS_PATH (evaluated if possible, raw string otherwise)

- repositories_path:

  default:

  :   "repositories/"

  option:

  :   en.repositories_path

  envvar:

  :   ENERGYRT_REPOSITORIES_PATH (evaluated if possible, raw string
      otherwise)

- datasets_path:

  default:

  :   "datasets/"

  option:

  :   en.datasets_path

  envvar:

  :   ENERGYRT_DATASETS_PATH (evaluated if possible, raw string
      otherwise)

- log_file:

  default:

  :   ""

  option:

  :   en.log_file

  envvar:

  :   ENERGYRT_LOG_FILE (evaluated if possible, raw string otherwise)

- store_versioning:

  default:

  :   "none"

  option:

  :   en.store_versioning

  envvar:

  :   ENERGYRT_STORE_VERSIONING (evaluated if possible, raw string
      otherwise)

- reports_path:

  default:

  :   "reports/"

  option:

  :   en.reports_path

  envvar:

  :   ENERGYRT_REPORTS_PATH (evaluated if possible, raw string
      otherwise)

- levcost_cache_path:

  default:

  :   "levcosts/"

  option:

  :   en.levcost_cache_path

  envvar:

  :   ENERGYRT_LEVCOST_CACHE_PATH (evaluated if possible, raw string
      otherwise)

- arrow_format:

  default:

  :   "feather"

  option:

  :   en.arrow_format

  envvar:

  :   ENERGYRT_ARROW_FORMAT (evaluated if possible, raw string
      otherwise)

- arrow_compression:

  default:

  :   "zstd"

  option:

  :   en.arrow_compression

  envvar:

  :   ENERGYRT_ARROW_COMPRESSION (evaluated if possible, raw string
      otherwise)

- arrow_compression_level:

  default:

  :   15L

  option:

  :   en.arrow_compression_level

  envvar:

  :   ENERGYRT_ARROW_COMPRESSION_LEVEL (evaluated if possible, raw
      string otherwise)

- verbose:

  default:

  :   0

  option:

  :   en.verbose

  envvar:

  :   ENERGYRT_VERBOSE (evaluated if possible, raw string otherwise)

- debug:

  default:

  :   0

  option:

  :   en.debug

  envvar:

  :   ENERGYRT_DEBUG (evaluated if possible, raw string otherwise)

- progress_bar:

  default:

  :   TRUE

  option:

  :   en.progress_bar

  envvar:

  :   ENERGYRT_PROGRESS_BAR (evaluated if possible, raw string
      otherwise)

## See also

options getOption Sys.setenv Sys.getenv
