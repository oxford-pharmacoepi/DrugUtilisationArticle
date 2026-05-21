
col0 <- "black"
colA <- "#1f77b4"
colB <- "#ff7f0e"
colC <- "#2ca02c"
font_family <- "sans"

exposures <- dplyr::tibble(
  person_id = c(1, 1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 5),
  start = c(-62, -34, -10, 25, 0, 43, 43, -70, -40, -8, 14, 27),
  end = c(-53, -25, 5, 30, 43, 70, 70, 5, -25, 16, 70, 33),
  type = c("C", "C", "A", "B", "A", "B", "C", "A", "A", "B", "C", "A"),
  offset = 0.1 * c(0, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1, 0)
) |>
  dplyr::mutate(
    type = paste0("Treatment ", type),
    group = dplyr::row_number(),
    y = person_id + offset
  ) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")

observation <- dplyr::tibble(
  person_id = 1:5,
  start = c(-75, -75, -75, -75, -10),
  end = c(75, 75, 28, 75, 75)
) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")
notObs <- dplyr::tibble(
  person_id = c(3, 5),
  start = c(28, -70),
  end = c(70, -10),
  type = "Not in observation"
) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")

eps <- 0
windows <- dplyr::tibble(
  start = c(-60, -30, 0, 1, 31) - eps,
  end = c(-31, -1, 0, 30, 60) + eps,
  y = 6.05,
  id = 1:5
)

# legend
y <- 7.65
x0 <- -68
w <- 6
s <- 2
s1 <- 34
s2 <- 25
legendlabs <- dplyr::tribble(
  ~x, ~lab,
  w + s, "Not in observation",
  2 * w + 2* s + s1, "Treatment A",
  3 * w + 3* s + s1 + s2, "Treatment B",
  4 * w + 4* s + s1 + 2 * s2, "Treatment C"
) |>
  dplyr::mutate(x = x + x0, y = y)
legendLines <- dplyr::tibble(
  x1 = c(0, w + s + s1, 2 * w + 2 * s + s1 + s2, 3 * w + 3 * s + s1 + 2 * s2),
  type = c("Not in observation", "Treatment A", "Treatment B", "Treatment C")
) |>
  dplyr::mutate(x1 = x1 + x0, x2 = x1 + w, y = y, id = dplyr::row_number()) |>
  tidyr::pivot_longer(c("x1", "x2"), names_to = NULL, values_to = "x")

# tables
dplyr::tibble(
  "Treatment" = rep(c("Treatment A", "Treatment B", "Treatment C", "untreated"), 5),
  "window" = c(rep("[-60, -31]", 4), rep("[-30, -1]", 4), rep("[0, 0]", 4), rep("[1, 30]", 4), rep("[31, 60]", 4)),
  "estimate_type" = "character",
  "estimate_name" = "%",
  "estimate_value" = c("50", "0", "25", "25", "80", "20", "0", "40", "60", "20", "0", "20", "60", "40", "20", "0", "50", "25", "50", "25")
) |>
  dplyr::mutate(estimate_value = paste0(estimate_value, "%")) |>
  visOmopResults::visTable(header = "window", hide = c("estimate_type", "estimate_name")) ->
  table_b
dplyr::tibble(
  "Treatment" = rep(c("A", "B", "C", "A and B", "A and C", "B and C", "A, B and C", "untreated"), 5),
  "window" = c(rep("[-60, -31]", 8), rep("[-30, -1]", 8), rep("[0, 0]", 8), rep("[1, 30]", 8), rep("[31, 60]", 8)),
  "estimate_type" = "character",
  "estimate_name" = "%",
  "estimate_value" = c("50", "0", "25", "0", "0", "0", "0", "25", "20", "0", "0", "20", "20", "0", "0", "40", "60", "20", "0", "0", "0", "0", "0", "20", "60", "0", "0", "20", "0", "20", "0", "0", "25", "0", "25", "0", "0", "0", "25", "25")
) |>
  dplyr::mutate(estimate_value = paste0(estimate_value, "%")) |>
  visOmopResults::visTable(header = "window", hide = c("estimate_type", "estimate_name")) ->
  table_c

p <- ggplot2::ggplot() +
  # exposures
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group, color = type),
    data = exposures,
    linewidth = 2.6,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  # windows
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = start, xmax = end, y = y, group = id),
    data = windows,
    linewidth = 0.45,
    width = 0.1,
    inherit.aes = FALSE
  ) +
  # observation
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, colour = type),
    data = notObs,
    linewidth = 4.3,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  # persons
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -70, y = 1:5, image = "./person.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.105
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -70, y = 1:5, label = 1:5),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    color = "white",
    nudge_y = 0.08,
    size = 3,
    family = font_family
  ) +
  # window labels
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = lab),
    data = dplyr::tibble(
      x = c(-45, -15, 0, 15, 45),
      y = c(0, 0, 0.6, 0, 0) + 6.35,
      lab = c("window 1 [-60, -31]", "window 2 [-30, -1]", "window 3 [0, 0]", "window 4 [1, 30]", "window 5 [31, 60]")
    ),
    size = 3,
    family = font_family,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(x = 0, y = 6.82, xend = 0, yend = 6.15),
    arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"),
    color = "black",
    linewidth = 0.25
  ) +
  # vertical lines
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group),
    data = dplyr::tibble(
      x = c(-60.5, -30.5, -0.5, 0.5, 30.5, 60.5),
      y1 = 0.5,
      y2 = 6.1,
    ) |>
      dplyr::mutate(group = dplyr::row_number()) |>
      tidyr::pivot_longer(c("y1", "y2"), names_to = NULL, values_to = "y"),
    linetype = "dashed",
    colour = "black",
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  # legend
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Treatment A" = colA, "Treatment B" = colB, "Treatment C" = colC, "Not in observation" = col0)
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = lab),
    data = legendlabs,
    size = 3.4,
    family = font_family,
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = legendLines |>
      dplyr::filter(id == 1),
    linewidth = 4,
    alpha = 0.5
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = legendLines |>
      dplyr::filter(id != 1),
    linewidth = 2.6,
    alpha = 0.8
  ) +
  # tables
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 120, y = 6.7, image = "./table1.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.56,
    nudge_x = 0,
    nudge_y = 0
  ) +
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 120, y = 2.35, image = "./table2.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.56,
    nudge_x = 0,
    nudge_y = 0
  ) +
  # axis
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y),
    data = dplyr::tibble(x = c(-75, 75), y = 0.15),
    colour = "black",
    linewidth = 0.4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = c(-74, 85, 85),
      y = c(8.75, 8.2, 4.8),
      label = c(
        "Illustrative example of prescription sequence",
        "Non-mutually exclusive categories",
        "Mutually exclusive categories"
      )
    ),
    data = NULL,
    size = 4,
    family = font_family,
    fontface = "italic",
    hjust = 0
  ) +
  ggplot2::coord_cartesian(xlim = c(-70, 150), ylim = c(0.5, 9.05), clip = "off") +
  ggplot2::scale_y_continuous(breaks = NULL, name = "") +
  ggplot2::scale_x_continuous(name = "Time (days)", breaks = seq(-60, 60, by = 30)) +
  ggplot2::theme(
    legend.position = "none",
    axis.line.x = ggplot2::element_line(linewidth = 0),
    axis.title.x = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 6)),
    axis.text.x = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    text = ggplot2::element_text(size = 14, family = font_family)
  )

if (interactive()) {
  print(p)
}

ggplot2::ggsave(
  filename = "./Figures/Diagram6.png",
  plot = p,
  width = 974*3,
  height = 480*3,
  units = "px",
  dpi = 300
)
