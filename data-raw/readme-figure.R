# Builds man/figures/readme-model.png: the README's central argument as a
# three-stage figure - declare reusable bricks, choose the model's time and
# space axes, then solve and analyze. TOPIA supplies a real multi-region model
# for every panel. Run from the package root; the solve is written to a scratch
# project so nothing lands in the repository.
suppressMessages({
  # load_all(), not library(): `data(package = "energyRt")` reads the INSTALLED
  # copy, so after a `data/topia.rda` rebuild this driver silently drew the
  # previous dataset -- it rendered the uncapped hydrogen chain for several
  # rounds after the cap had been changed. A data-raw driver has to see the
  # source tree it is run from.
  pkgload::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(patchwork)
})
options(warn = 1)

# The solve is kept in the package's own project store (gitignored), not in
# tempdir(): iterating on the figure should not cost another eight-minute
# solve. It is reused only when it came from the same model -- reusing a solve
# that no longer matches is the same class of error as reading a stale
# dataset, which this driver has already made once.
open_project(path = ".")
readme_scenario <- "README_FIGURE"
# HiGHS barrier through multimod: 72 s against glpsol's 450 on this model, and
# GLPK is for small models by the stack's own guidance -- this is the eleven
# region hourly case. cuOpt on a local GPU (`mm_cuopt_wsl_py`) is roughly
# twice as fast again where WSL has it installed.
readme_solver <- solver_options$mm_highs_barrier

# The whole shipped model, all eleven regions, at hourly resolution: a summer
# day's dispatch and the battery's cycle are two of the three result panels,
# so the time axis cannot be coarsened, and the map panel draws the full
# island. The solve takes a few minutes; this driver is run by hand.
topia_electricity <- topia$modules$electricity
topia_calendar <- topia$modules$calendars$s4_h24
topia_horizon <- topia$modules$horizons$BY2025_50_by10

# Keep storage visible in this compact demonstration. A 1 GW existing battery
# in each region is a sunk asset, so the dispatch exposes charging and
# discharging even when TOPIA would not choose to invest in a new battery at
# the reference costs.
readme_repository <- topia_electricity$repo
readme_storage <- readme_repository@data$STG_ELC
readme_storage_capacity <- data.frame(
  out.stock = 1
)
readme_storage <- update(
  object = readme_storage,
  desc = "Lithium-ion battery, four-hour reservoir",
  capacity = readme_storage_capacity
)
readme_repository@data$STG_ELC <- readme_storage

TOPIA <- newModel(
  name = "TOPIA",
  data = readme_repository,
  calendar = topia_calendar,
  region = topia_electricity$regions,
  horizon = topia_horizon,
  discount = 0.05
)
# The model needs its geoscale: COA and GAS are balanced per zone, so their
# supply, imports and fuel trades are declared on zone codes.
TOPIA <- setGeoscale(
  obj = TOPIA,
  geoscale = topia_electricity$geoscale
)

stored_runs <- tryCatch(
  load_scenarios(names = readme_scenario, verbose = FALSE),
  error = function(e) list()
)
TOPIA_README <- NULL
if (length(stored_runs) >= 1L && methods::is(stored_runs[[1]], "scenario")) {
  stored_scenario <- stored_runs[[1]]
  stored_matches <- isTRUE(stored_scenario@status$solved) &&
    identical(model_hash(stored_scenario@model), model_hash(TOPIA))
  if (stored_matches) {
    message("reusing the stored solve '", readme_scenario, "'")
    TOPIA_README <- stored_scenario
  } else {
    message("the stored solve is from another model; solving again")
  }
}
if (is.null(TOPIA_README)) {
  TOPIA_README <- solve_model(
    object = TOPIA,
    name = readme_scenario,
    solver = readme_solver
  )
  TOPIA_README <- save_scenario(TOPIA_README)
}

# Space axis: one map plane per geoframe, nation on top, every unit named.
map_geoframes <- c("nation", "zone", "region")
p_map <- autoplot(
  x = topia$geoscales$island,
  type = "stack",
  view = "oblique",
  depth = 1,
  fill = "region",
  labels = map_geoframes,
  frame = TRUE
) +
  labs(title = "2a  Space: nested geographies") +
  theme(legend.position = "none")
map_label_layer <- 7L
map_label_data <- p_map$layers[[map_label_layer]]$data
map_label_data$gf[map_label_data$gf == "nation"] <- "island"
p_map$layers[[map_label_layer]]$data <- map_label_data

# Time axis: a modeler-defined nested calendar, with the planning horizon below
# it. The energyRt calendar is built from the timescales catalog entry of the
# same name; neither the hierarchy nor the number of slices is fixed here.
timescales_calendar <- timescales::calendar(name = "s4_h24")
calendar_breaks <- seq(
  from = 0,
  to = 1,
  by = 0.25
)
calendar_x_expansion <- expansion(mult = c(0, 0.06))
p_cal <- autoplot(object = timescales_calendar) +
  labs(
    title = "2b  Time: nested calendar and horizon",
    subtitle = NULL,
    x = NULL
  ) +
  theme(legend.position = "none")
calendar_x_scale <- p_cal$scales$get_scales("x")
calendar_x_scale$breaks <- calendar_breaks
calendar_x_scale$expand <- calendar_x_expansion
horizontal_year_text <- element_text(
  angle = 0,
  hjust = 0.5,
  vjust = 0.5
)
horizon_breaks <- seq(
  from = 2020,
  to = 2050,
  by = 10
)
horizon_x_expansion <- expansion(mult = c(0.03, 0.07))
p_hor <- autoplot(object = topia_horizon) +
  labs(
    title = NULL,
    subtitle = NULL
  ) +
  theme(
    axis.text.x = horizontal_year_text,
    legend.position = "none"
  )
horizon_x_scale <- p_hor$scales$get_scales("x")
horizon_x_scale$breaks <- horizon_breaks
horizon_x_scale$expand <- horizon_x_expansion
time_heights <- c(0.72, 0.18)
p_time <- p_cal / p_hor
p_time <- p_time + plot_layout(heights = time_heights)

# Bricks: draw() lays out each process on a device; rendered separately and
# placed as rasters so labels keep their size in the left stage of the figure.
brick <- function(object) {
  image_file <- tempfile(fileext = ".png")
  ragg::agg_png(
    filename = image_file,
    width = 1200,
    height = 800,
    res = 300,
    background = "white"
  )
  draw(object = object)
  closed_device <- dev.off()

  image <- png::readPNG(source = image_file)
  image_grob <- grid::rasterGrob(
    image = image,
    interpolate = TRUE
  )
  unlink(x = image_file)
  wrap_elements(full = image_grob)
}

brick_solar <- brick(object = topia_electricity$repo@data$ESOL)
brick_gas <- brick(object = topia_electricity$repo@data$EGAS)
brick_storage <- brick(object = topia_electricity$repo@data$STG_ELC)
brick_trade <- brick(object = topia_electricity$repo@data$TBD_ELC_W1_W2)

p_bricks <- brick_solar / brick_gas / brick_storage / brick_trade
p_bricks <- wrap_elements(p_bricks) +
  ggtitle("1  Declare reusable bricks")

# Results. Processes are relabelled with the carrier ids of energypal's
# PyPSA palette, baseload first; the display names go on the legend.
carrier <- c(
  ENUC = "nuclear",
  ECOA = "coal",
  EHYD = "hydro",
  EGAS = "gas",
  EBIO = "biomass",
  EWIN = "onwind",
  ESOL = "solar",
  STG_ELC = "battery",
  H2ELC = "electrolysis",
  export = "electricity trade"
)
# Every electricity route is one legend key, however many the model carries:
# naming them one by one left the other nine unmapped and their rows NA.
trade_processes <- grep("^TBD_ELC_", names(topia_electricity$repo),
                        value = TRUE)
carrier[trade_processes] <- "electricity trade"
carrier_label <- c(
  nuclear = "Nuclear",
  coal = "Coal",
  hydro = "Hydro",
  gas = "Gas",
  biomass = "Bioenergy",
  onwind = "Wind",
  solar = "Solar",
  battery = "Storage",
  electrolysis = "Electrolysis",
  "electricity trade" = "Trade"
)

p_day <- autoplot(
  object = TOPIA_README,
  type = "generation",
  timeslice = "^SUM_",
  year = 2030
)
# 3b draws the battery's LEVEL across the whole year, not its charge and
# discharge on one day: on a summer day the fleet barely cycles and the panel
# came out nearly empty. At s4_h24 a year is four typical days, 96 slices, so
# one panel shows the daily cycle and lets the seasons be compared.
# `autoplot(type = "storage")` gives the in/out flows only; the level is
# `vStorageLevel`.
storage_level <- getData(
  object = TOPIA_README,
  name = "vStorageLevel",
  year = 2030,
  merge = TRUE
)
storage_level <- aggregate(
  x = value ~ timeslice,
  data = storage_level,
  FUN = sum
)
# Season and hour come from the calendar, not from the label text: the
# timetable names the season of every timeslice, and tsl2hour() reads the
# hour off the calendar rather than off the string.
storage_level <- merge(
  x = storage_level,
  y = topia_calendar@timetable[, c("timeslice", "SEASON")],
  by = "timeslice"
)
storage_level$hour <- timescales::tsl2hour(
  tsl = as.character(storage_level$timeslice),
  calendar = timescales_calendar
)
storage_level$season <- factor(
  x = as.character(storage_level$SEASON),
  levels = unique(as.character(topia_calendar@timetable$SEASON))
)
p_year <- autoplot(
  object = TOPIA_README,
  type = "generation",
  comm = "ELC"
)
# 3c is faceted by region by default, and eleven panels in a third of the
# figure's width cannot carry a year axis. Faceting by zone instead put nearly
# all of the hydrogen in EAST, several times that zone's own demand, which
# reads as a defect rather than as the resource concentration it is. The panel
# is national: one set of bars for the island.
#
# `facet = NULL` does not do this -- autoplot reads NULL as "decide for me"
# and still facets when the model has more than one region -- so the region
# column is dropped and the facet removed after the fact. by_carrier()
# aggregates on every remaining column, which collapses the regions.
drop_region <- function(d) {
  d$region <- NULL
  d
}
p_year$data <- drop_region(p_year$data)
# The demand overlay is a second layer with its own data, and it has to be
# summed over the regions too or the load line is drawn eleven times.
p_year$layers[[2]]$data <- aggregate(
  x = value ~ .,
  data = drop_region(p_year$layers[[2]]$data),
  FUN = sum
)

# one level set for both panels, so the collected legend is shared
day_process <- as.character(p_day$data$process)
year_process <- as.character(p_year$data$process)
processes <- c(day_process, year_process)
processes <- unique(processes)
present <- carrier[processes]
carrier_levels <- unname(carrier)
carrier_levels <- unique(carrier_levels)
levels_shared <- carrier_levels[carrier_levels %in% present]
energy_palette <- "carriers"

by_carrier <- function(plot, load_geom) {
  process <- as.character(plot$data$process)
  process_carrier <- carrier[process]
  plot$data$process <- factor(
    x = process_carrier,
    levels = levels_shared
  )
  plot$data <- aggregate(
    x = value ~ .,
    data = plot$data,
    FUN = sum
  )
  stack_position <- position_stack(reverse = TRUE)
  plot$layers[[1]]$position <- stack_position
  # Draw every key, including carriers absent from this panel, so the two
  # legends are identical.
  plot$layers[[1]]$show.legend <- c(fill = TRUE, colour = FALSE)
  # Redraw the demand overlay with a colour mapping so it reaches the legend.
  load_data <- plot$layers[[2]]$data
  plot$layers[[2]] <- NULL
  # Use one fill scale in both panels so patchwork collects a single legend.
  fill_values <- energypal::energypal_colors(
    sources = levels_shared,
    palette = energy_palette
  )
  fill_labels <- carrier_label[levels_shared]
  load_layer <- load_geom(data = load_data)
  fill_scale <- scale_fill_manual(
    values = fill_values,
    labels = fill_labels,
    drop = FALSE,
    name = NULL
  )
  load_values <- c(Load = "red")
  load_scale <- scale_colour_manual(
    values = load_values,
    name = NULL
  )

  plot <- plot + load_layer
  plot <- plot + fill_scale
  plot <- plot + load_scale
  plot
}

day_load_mapping <- aes(
  x = hour,
  y = value,
  colour = "Load"
)
day_load_geom <- function(data) {
  geom_line(
    data = data,
    mapping = day_load_mapping,
    inherit.aes = FALSE,
    linewidth = 0.9
  )
}
p_day <- by_carrier(
  plot = p_day,
  load_geom = day_load_geom
) +
  labs(
    title = "3a  Dispatch: a summer day in 2030",
    subtitle = NULL,
    x = "hour",
    y = NULL
  ) +
  # 3a and 3c carry the same keys, and patchwork drew both rather than
  # merging them -- two legends, overlaying the panels beside them. 3c is the
  # only one that keeps a legend; `drop = FALSE` there shows every key.
  theme(legend.position = "none")

battery_color <- energypal::energypal_colors(
  sources = "battery",
  palette = energy_palette
)
battery_color <- unname(battery_color)
storage_mapping <- aes(
  x = hour,
  y = value
)
storage_hour_breaks <- c(0, 6, 12, 18)
p_storage <- ggplot(
  data = storage_level,
  mapping = storage_mapping
) +
  geom_area(fill = battery_color) +
  # one typical day per season, named on top; the hours run along the axis
  facet_wrap(
    facets = vars(season),
    nrow = 1
  ) +
  scale_x_continuous(
    breaks = storage_hour_breaks,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "3b  Storage level",
    subtitle = "four-hour battery, 1 GW stock per region · 2030, all regions",
    x = "hour",
    y = NULL
  ) +
  theme(legend.position = "none")

year_load_mapping <- aes(
  x = factor(year),
  ymin = value,
  ymax = value,
  colour = "Load"
)
year_load_geom <- function(data) {
  geom_errorbar(
    data = data,
    mapping = year_load_mapping,
    inherit.aes = FALSE,
    width = 0.8,
    linewidth = 0.9,
    key_glyph = "path"
  )
}
p_year <- by_carrier(
  plot = p_year,
  load_geom = year_load_geom
) +
  facet_null() +
  # one column: `legend.direction = "vertical"` stacks the two guides but lets
  # the fill guide itself wrap, which gave three columns
  guides(
    fill = guide_legend(ncol = 1, order = 2),
    colour = guide_legend(ncol = 1, order = 1)
  ) +
  labs(
    title = "3c  Generation mix over the horizon",
    y = NULL
  ) +
  theme(
    axis.text.x = horizontal_year_text
  )

mix_heights <- c(0.82, 0.58, 1)
p_mix <- p_day / p_storage / p_year
# Guides are collected ONCE, at the outer layout below. Collecting here as
# well drew the legend twice, overlaying the panels it sat beside.
p_mix <- p_mix + plot_layout(heights = mix_heights)

# The README now explains energyRt as a flow: reusable components are combined
# with independent time and space axes, then compiled, solved and analyzed.
# Preserve that reading order rather than presenting four unrelated quadrants.
axes_heights <- c(1.2, 0.8)
p_axes <- p_map / p_time
p_axes <- p_axes + plot_layout(heights = axes_heights)

figure_widths <- c(0.72, 1.08, 1.28)
fig <- p_bricks | p_axes | p_mix
fig <- fig + plot_layout(widths = figure_widths, guides = "collect")

figure_subtitle <- paste(
  "TOPIA in energyRt: reusable components × modeler-defined time and space",
  "→ capacity expansion and dispatch"
)
figure_annotation <- plot_annotation(
  title = "Build once, vary the axes, solve and analyze",
  subtitle = figure_subtitle
)
fig <- fig + figure_annotation

panel_margin <- margin(
  t = 4,
  r = 4,
  b = 4,
  l = 4
)
legend_key_size <- unit(
  x = 0.4,
  units = "cm"
)
legend_spacing <- unit(
  x = 0.1,
  units = "cm"
)
figure_title_text <- element_text(
  size = 11,
  face = "plain"
)
figure_subtitle_text <- element_text(
  size = 10,
  colour = "grey35"
)
legend_text <- element_text(size = 8.5)
figure_theme <- theme(
  plot.title = figure_title_text,
  plot.subtitle = figure_subtitle_text,
  plot.margin = panel_margin,
  legend.key.size = legend_key_size,
  legend.text = legend_text,
  legend.spacing.y = legend_spacing,
  # one column, sitting low beside the results rather than centred against
  # them: the collected legend belongs to 3c, which is the bottom panel
  legend.direction = "vertical",
  legend.box = "vertical",
  # low beside the results, but lifted clear of the bottom edge
  legend.justification = "bottom",
  legend.box.margin = margin(b = 70)
)
fig <- fig & figure_theme

# What the results panel drew, so a stale dataset or a changed aggregation is
# visible in the log rather than only in the picture.
cat("\n3c totals by carrier (GWh over the horizon):\n")
print(aggregate(value ~ process, data = p_year$data, FUN = sum),
      row.names = FALSE)

figure_file <- "man/figures/readme-model.png"
# ragg warns rather than errors when it cannot write (a viewer holding the
# file open is enough), and the report below used to print regardless -- a
# run then looked successful while the figure on disk was the previous one.
figure_before <- if (file.exists(figure_file)) file.mtime(figure_file) else NA
ggsave(
  filename = figure_file,
  plot = fig,
  width = 15,
  height = 9.5,
  dpi = 200,
  bg = "white"
)
if (!file.exists(figure_file) ||
    (!is.na(figure_before) && file.mtime(figure_file) <= figure_before)) {
  stop("ggsave() did not write ", figure_file,
       " -- it is most likely open in another program.", call. = FALSE)
}
cat("written ", figure_file, " - ",
    round(file.size(figure_file) / 1024), " KB\n", sep = "")
