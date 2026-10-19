# Builds man/figures/readme-model.png: the README's central argument as a
# three-stage figure - declare reusable bricks, choose the model's time and
# space axes, then solve and analyze. TOPIA supplies a real multi-region model
# for every panel. Run from the package root; the solve is written to a scratch
# project so nothing lands in the repository.
suppressMessages({
  library(energyRt)
  library(ggplot2)
  library(patchwork)
})
options(warn = 1)

data(
  list = "topia",
  package = "energyRt"
)

scratch_root <- tempdir()
scratch <- file.path(scratch_root, "readme-figure")
dir.create(
  path = scratch,
  showWarnings = FALSE,
  recursive = TRUE
)
if (exists("open_project")) {
  open_project(path = scratch)
}

topia_electricity <- topia$modules$electricity$R3
topia_calendar <- topia$modules$calendars$s4_h24
topia_horizon <- topia$modules$horizons$base

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
TOPIA <- setGeoscale(
  obj = TOPIA,
  geoscale = topia$geoscale$island
)

TOPIA_README <- solve_model(
  object = TOPIA,
  name = "TOPIA_README",
  solver = solver_options$glpk
)

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
  TBD_ELC_W1_W2 = "electricity trade",
  TBD_ELC_W2_C1 = "electricity trade",
  export = "electricity trade"
)
carrier_label <- c(
  nuclear = "Nuclear",
  coal = "Coal",
  hydro = "Hydro",
  gas = "Gas",
  biomass = "Bioenergy",
  onwind = "Wind",
  solar = "Solar",
  battery = "Storage",
  "electricity trade" = "Trade"
)

p_day <- autoplot(
  object = TOPIA_README,
  type = "generation",
  timeslice = "^SUM_",
  year = 2030
)
p_storage <- autoplot(
  object = TOPIA_README,
  type = "storage",
  timeslice = "^SUM_",
  year = 2030
)
p_year <- autoplot(
  object = TOPIA_README,
  type = "generation",
  comm = "ELC"
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
  )

battery_color <- energypal::energypal_colors(
  sources = "battery",
  palette = energy_palette
)
battery_color <- unname(battery_color)
storage_values <- c(STG_ELC = battery_color)
storage_scale <- scale_fill_manual(
  values = storage_values,
  name = NULL
)
p_storage <- p_storage + storage_scale
p_storage <- p_storage +
  labs(
    title = "3b  Storage operation: four-hour battery",
    subtitle = "1 GW stock per region · discharge above zero · charge below zero",
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
  labs(
    title = "3c  Generation mix over the horizon",
    y = NULL
  ) +
  theme(
    axis.text.x = horizontal_year_text
  )

mix_heights <- c(0.82, 0.58, 1)
p_mix <- p_day / p_storage / p_year
p_mix <- p_mix + plot_layout(
  heights = mix_heights,
  guides = "collect"
)

# The README now explains energyRt as a flow: reusable components are combined
# with independent time and space axes, then compiled, solved and analyzed.
# Preserve that reading order rather than presenting four unrelated quadrants.
axes_heights <- c(1.2, 0.8)
p_axes <- p_map / p_time
p_axes <- p_axes + plot_layout(heights = axes_heights)

figure_widths <- c(0.72, 1.08, 1.28)
fig <- p_bricks | p_axes | p_mix
fig <- fig + plot_layout(widths = figure_widths)

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
  legend.spacing.y = legend_spacing
)
fig <- fig & figure_theme

ggsave(
  filename = "man/figures/readme-model.png",
  plot = fig,
  width = 15,
  height = 9.5,
  dpi = 200,
  bg = "white"
)
cat("written man/figures/readme-model.png\n")
