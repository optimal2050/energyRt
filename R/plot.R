#' @include print.R


plot_horizon <- function(x, ...) {

  check_package("ggplot2")

  args <- list(...)
  if (!is.null(args$hjust)) {
    hjust <- args$hjust
    stopifnot(hjust >= 0 && hjust <= 1)
  } else {
    hjust <- 1
  }

  y <- x@intervals |>
    dplyr::mutate(w = end - start + 1)

  p <- ggplot2::ggplot(y) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = start - hjust, xmax = end + (1 - hjust),
        ymin = 0, ymax = 1,
        fill = mid),
      color = "black") +
    ggplot2::geom_vline(xintercept = y$mid, color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = y$mid, linetype = "dashed") +
    ggplot2::scale_fill_viridis_c(option = "C", name = "") +
    ggplot2::labs(y = NULL, x = "milestone year") +
    ggplot2::scale_x_continuous(
      breaks = unique(c(y$start[1], y$mid)), expand = c(0, 0),
      # minor_breaks = unique(c(y$start[1], y$mid, y$mid))) +
      minor_breaks = seq(min(y$start), max(y$end), by = 1),
      guide = guide_axis(minor.ticks = TRUE)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = .05, hjust = .5),
      # axis.minor.ticks = element_line(size = 0.5),
      panel.border = ggplot2::element_rect(color = NA, fill = NA),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  if (!is_empty(x@name)) {p <- p + ggplot2::labs(title = x@name)}
  if (!is_empty(x@desc)) {p <- p + ggplot2::labs(subtitle = x@desc)}
  p
}

#' Visualize a Horizon object
#'
#' @param x An object of class `horizon`
#' @param ... Additional optional arguments:
#' `hjust` (numeric) to adjust the horizontal position of the intervals,
#' accepts values between 0 and 1.
#'
#' @return
#' @export
#' @examples
#' NULL
setMethod("plot", c("horizon", "ANY"), plot_horizon)
# setMethod("plot", "horizon", plot_horizon)


energy_palettes <- list(
  "default" = c(
    "Coal" = "#4B4B4B",
    "Gas" = "#1F78B4",
    "Oil" = "#FF7F00",
    "Nuclear" = "#A6CEE3",
    "Renewables" = "#33A02C",
    "Hydro" = "#6A3D9A",
    "Solar" = "#FDBF6F",
    "Wind" = "#CAB2D6"
  ),
  "renewables_focus" = c(
    "Solar" = "#FDBF6F",
    "Wind" = "#CAB2D6",
    "Hydro" = "#6A3D9A",
    "Other" = "#B2DF8A"
  ),
  "high_contrast" = c(
    "Coal" = "#000000",
    "Gas" = "#E31A1C",
    "Oil" = "#FF7F00",
    "Nuclear" = "#6A3D9A",
    "Renewables" = "#33A02C"
  )
)
