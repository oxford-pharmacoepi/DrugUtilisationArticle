## ── Figure 3 (uniform font + size + closer metrics) ───────────────────────────
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggimage)
library(ggtext)

## colours
col_c <- "#003366"   # cohort bar
col_e <- "#1f77b4"   # exposure bars

## positions
y_axis <- 0.385
y_coh  <- 2.7
y_exp  <- c(2, 1, 1.5)

## unified font + size
font_family <- "sans"
fs_pt  <- 16                         # point size for theme text
.pt    <- 72.27 / 25.4               # ggplot2 internal pt→mm factor
fs_geom <- fs_pt / .pt               # use same visual size for geoms

## input
exposures_raw <- tibble(
  drug     = c("500 mg", "300 mg", "750 mg"),
  start    = c(0, 15, 30),
  end      = c(20, 25, 45),
  qty      = c(63, 22, 32),
  strength = c(500, 300, 750)
)

## for geom_line
exposures <- exposures_raw |>
  mutate(group = row_number(), y = y_exp) |>
  pivot_longer(c(start, end), values_to = "x")

## metrics
exposures_raw <- exposures_raw |>
  mutate(days = end - start + 1,
         dose = qty * strength)

cumulative_dose <- sum(exposures_raw$dose)

days_p_txt <- "**Days prescribed**<br>(20−0+1) + (25−15+1) + (45−30+1)= **48**"
days_e_txt <- "**Days exposed**<br>(25−0+1) + (45−30+1) = **42**"
cum_dose_txt <- paste0(
  "**Cumulative dose**<br>",
  paste0(exposures_raw$qty, "×", exposures_raw$strength, collapse = " + "),
  "= **", format(cumulative_dose, big.mark = ","), "** mg"
)

## text placement (closer to bars)
x_metrics <- max(exposures_raw$end) + 1

metrics <- tibble(
  txt = c(days_p_txt, days_e_txt, cum_dose_txt),
  x   = x_metrics,
  y   = y_coh - c(0.05, 1.0, 2.0)
)

## cohort bar
cohort <- tibble(xmin = 0, xmax = 45, y = y_coh)

## plot
p <- ggplot() +
  geom_segment(
    data = tibble(x = c(15, 20, 25, 30, 45)),
    aes(x = x, xend = x, y = y_axis, yend = y_exp[1]),
    colour = "grey70", linetype = "dashed"
  ) +
  geom_errorbar(
    data = cohort,
    aes(xmin = xmin, xmax = xmax, y = y, colour = "cohort"),
    width = 0.25, size = 1
  ) +
  geom_line(
    data = exposures,
    aes(x = x, y = y, group = group, colour = "exposures"),
    linewidth = 2, alpha = 0.8
  ) +
  geom_text(
    data = tibble(
      x = c(10, 20, 37.5), y = y_exp,
      lab = c("Acetaminophen 500 mg (19020053)\nquantity = 63",
              "Acetaminophen 300 mg (19096574)\nquantity = 22",
              "Acetaminophen 750 mg (19107439)\nquantity = 32")
    ),
    aes(x, y, label = lab),
    size = fs_geom, family = font_family
  ) +
  ggimage::geom_image(aes(x = -4, y = 1.6, image = "person.png"), size = 0.7) +
  geom_segment(aes(x = -3, xend = 45, y = y_axis, yend = y_axis)) +
  annotate("text", x = 22.5, y = 0, label = "Time (days)",
           size = fs_geom, family = font_family) +
  ggtext::geom_richtext(
    data = metrics,
    aes(x, y, label = txt),
    hjust = 0, size = fs_geom, lineheight = 1.1,
    fill = NA, label.color = NA, family = font_family
  ) +
  labs(x = "Time (days)", y = NULL) +
  scale_colour_manual(values = c(cohort = col_c, exposures = col_e), name = "") +
  coord_cartesian(xlim = c(-3, x_metrics + 12), ylim = c(-0.2, y_coh), clip = "off") +
  scale_x_continuous(breaks = c(0, 10, 15, 20, 25, 30, 40, 45)) +
  theme_minimal(base_size = fs_pt) +
  theme(
    legend.position = "top",
    text         = element_text(family = font_family, size = fs_pt),
    legend.text  = element_text(size = fs_pt),
    axis.title.x = element_text(margin = margin(t = 10), size = fs_pt),
    axis.text.x  = element_text(size = fs_pt),
    axis.text.y  = element_blank(),
    panel.grid   = element_blank()
  )

p
