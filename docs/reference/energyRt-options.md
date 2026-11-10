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

  :     NULL

  option:

  : energyRt.gams_path

  envvar:

  : ENERGYRT_GAMS_PATH (evaluated if possible, raw string otherwise)

- gdxlib_path:

  default:

  :     NULL

  option:

  : energyRt.gdxlib_path

  envvar:

  : ENERGYRT_GDXLIB_PATH (evaluated if possible, raw string otherwise)

- python_path:

  default:

  :     NULL

  option:

  : energyRt.python_path

  envvar:

  : ENERGYRT_PYTHON_PATH (evaluated if possible, raw string otherwise)

- julia_path:

  default:

  :     NULL

  option:

  : energyRt.julia_path

  envvar:

  : ENERGYRT_JULIA_PATH (evaluated if possible, raw string otherwise)

- glpk_path:

  default:

  :     NULL

  option:

  : energyRt.glpk_path

  envvar:

  : ENERGYRT_GLPK_PATH (evaluated if possible, raw string otherwise)

- neos_email:

  default:

  :     NULL

  option:

  : energyRt.neos_email

  envvar:

  : NEOS_EMAIL (evaluated if possible, raw string otherwise)

- neos_endpoint:

  default:

  :     "https://neos-server.org:3333"

  option:

  : energyRt.neos_endpoint

  envvar:

  : ENERGYRT_NEOS_ENDPOINT (evaluated if possible, raw string otherwise)

- solver:

  default:

  :     list(name = "glpk", lang = "GLPK")

  option:

  : energyRt.solver

  envvar:

  : ENERGYRT_SOLVER (evaluated if possible, raw string otherwise)

- scenarios_path:

  default:

  :     "scenarios/"

  option:

  : energyRt.scenarios_path

  envvar:

  : ENERGYRT_SCENARIOS_PATH (evaluated if possible, raw string
    otherwise)

- registry_file:

  default:

  :     "energyRt_registry.csv"

  option:

  : energyRt.registry_file

  envvar:

  : ENERGYRT_REGISTRY_FILE (evaluated if possible, raw string otherwise)

- models_path:

  default:

  :     "models/"

  option:

  : energyRt.models_path

  envvar:

  : ENERGYRT_MODELS_PATH (evaluated if possible, raw string otherwise)

- repositories_path:

  default:

  :     "repositories/"

  option:

  : energyRt.repositories_path

  envvar:

  : ENERGYRT_REPOSITORIES_PATH (evaluated if possible, raw string
    otherwise)

- datasets_path:

  default:

  :     "datasets/"

  option:

  : energyRt.datasets_path

  envvar:

  : ENERGYRT_DATASETS_PATH (evaluated if possible, raw string otherwise)

- log_file:

  default:

  :     ""

  option:

  : energyRt.log_file

  envvar:

  : ENERGYRT_LOG_FILE (evaluated if possible, raw string otherwise)

- profile_dir:

  default:

  :     ""

  option:

  : energyRt.profile_dir

  envvar:

  : ENERGYRT_PROFILE_DIR (evaluated if possible, raw string otherwise)

- reports_path:

  default:

  :     "reports/"

  option:

  : energyRt.reports_path

  envvar:

  : ENERGYRT_REPORTS_PATH (evaluated if possible, raw string otherwise)

- levcost_cache_path:

  default:

  :     "levcosts/"

  option:

  : energyRt.levcost_cache_path

  envvar:

  : ENERGYRT_LEVCOST_CACHE_PATH (evaluated if possible, raw string
    otherwise)

- path_builders:

  default:

  :     list()

  option:

  : energyRt.path_builders

  envvar:

  : ENERGYRT_PATH_BUILDERS (evaluated if possible, raw string otherwise)

- storage_format:

  default:

  :     "feather"

  option:

  : energyRt.storage_format

  envvar:

  : ENERGYRT_STORAGE_FORMAT (evaluated if possible, raw string
    otherwise)

- storage_compression:

  default:

  :     "zstd"

  option:

  : energyRt.storage_compression

  envvar:

  : ENERGYRT_STORAGE_COMPRESSION (evaluated if possible, raw string
    otherwise)

- storage_compression_level:

  default:

  :     15L

  option:

  : energyRt.storage_compression_level

  envvar:

  : ENERGYRT_STORAGE_COMPRESSION_LEVEL (evaluated if possible, raw
    string otherwise)

- exchange_format:

  default:

  :     "feather"

  option:

  : energyRt.exchange_format

  envvar:

  : ENERGYRT_EXCHANGE_FORMAT (evaluated if possible, raw string
    otherwise)

- exchange_compression:

  default:

  :     "lz4"

  option:

  : energyRt.exchange_compression

  envvar:

  : ENERGYRT_EXCHANGE_COMPRESSION (evaluated if possible, raw string
    otherwise)

- exchange_compression_level:

  default:

  :     15L

  option:

  : energyRt.exchange_compression_level

  envvar:

  : ENERGYRT_EXCHANGE_COMPRESSION_LEVEL (evaluated if possible, raw
    string otherwise)

- fx_equ:

  default:

  :     TRUE

  option:

  : energyRt.fx_equ

  envvar:

  : ENERGYRT_FX_EQU (evaluated if possible, raw string otherwise)

- parent_levels:

  default:

  :     FALSE

  option:

  : energyRt.parent_levels

  envvar:

  : ENERGYRT_PARENT_LEVELS (evaluated if possible, raw string otherwise)

- on_missing_input:

  default:

  :     "stop"

  option:

  : energyRt.on_missing_input

  envvar:

  : ENERGYRT_ON_MISSING_INPUT (evaluated if possible, raw string
    otherwise)

- threads:

  default:

  :     0L

  option:

  : energyRt.threads

  envvar:

  : ENERGYRT_THREADS (evaluated if possible, raw string otherwise)

- map_rules:

  default:

  :     TRUE

  option:

  : energyRt.map_rules

  envvar:

  : ENERGYRT_MAP_RULES (evaluated if possible, raw string otherwise)

- map_rules_verify:

  default:

  :     FALSE

  option:

  : energyRt.map_rules_verify

  envvar:

  : ENERGYRT_MAP_RULES_VERIFY (evaluated if possible, raw string
    otherwise)

- base_maps:

  default:

  :     FALSE

  option:

  : energyRt.base_maps

  envvar:

  : ENERGYRT_BASE_MAPS (evaluated if possible, raw string otherwise)

- base_maps_verify:

  default:

  :     FALSE

  option:

  : energyRt.base_maps_verify

  envvar:

  : ENERGYRT_BASE_MAPS_VERIFY (evaluated if possible, raw string
    otherwise)

- model_checks_stop:

  default:

  :     FALSE

  option:

  : energyRt.model_checks_stop

  envvar:

  : ENERGYRT_MODEL_CHECKS_STOP (evaluated if possible, raw string
    otherwise)

- mm_keep_wildcards:

  default:

  :     FALSE

  option:

  : energyRt.mm_keep_wildcards

  envvar:

  : ENERGYRT_MM_KEEP_WILDCARDS (evaluated if possible, raw string
    otherwise)

- save_mode:

  default:

  :     "ref"

  option:

  : energyRt.save_mode

  envvar:

  : ENERGYRT_SAVE_MODE (evaluated if possible, raw string otherwise)

- ondisk_min_rows:

  default:

  :     1e+05

  option:

  : energyRt.ondisk_min_rows

  envvar:

  : ENERGYRT_ONDISK_MIN_ROWS (evaluated if possible, raw string
    otherwise)

- param_ref_min_rows:

  default:

  :     1e+06

  option:

  : energyRt.param_ref_min_rows

  envvar:

  : ENERGYRT_PARAM_REF_MIN_ROWS (evaluated if possible, raw string
    otherwise)

- scen_file_mb_limit:

  default:

  :     256

  option:

  : energyRt.scen_file_mb_limit

  envvar:

  : ENERGYRT_SCEN_FILE_MB_LIMIT (evaluated if possible, raw string
    otherwise)

- lazy_hash:

  default:

  :     TRUE

  option:

  : energyRt.lazy_hash

  envvar:

  : ENERGYRT_LAZY_HASH (evaluated if possible, raw string otherwise)

- mem_gc_full:

  default:

  :     FALSE

  option:

  : energyRt.mem_gc_full

  envvar:

  : ENERGYRT_MEM_GC_FULL (evaluated if possible, raw string otherwise)

- project_root:

  default:

  :     quote(file.path(tempdir(), "energyRt"))

  option:

  : energyRt.project_root

  envvar:

  : ENERGYRT_PROJECT_ROOT (evaluated if possible, raw string otherwise)

- verbose:

  default:

  :     1

  option:

  : energyRt.verbose

  envvar:

  : ENERGYRT_VERBOSE (evaluated if possible, raw string otherwise)

- debug:

  default:

  :     0

  option:

  : energyRt.debug

  envvar:

  : ENERGYRT_DEBUG (evaluated if possible, raw string otherwise)

- progress_bar:

  default:

  :     TRUE

  option:

  : energyRt.progress_bar

  envvar:

  : ENERGYRT_PROGRESS_BAR (evaluated if possible, raw string otherwise)

## See also

options getOption Sys.setenv Sys.getenv
