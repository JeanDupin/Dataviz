# Packages ----

library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(hms)
library(forcats)
library(showtext)

# Polices
font_add_google("Atkinson Hyperlegible", "atkinson")
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 192)


# Données ----

url <- "https://sportinnovation.fr/api/races/80626/results"

resp <- request(url) |>
  req_headers(
    "Accept"     = "*/*",
    "User-Agent" = "Mozilla/5.0",
    "Referer"    = "https://results.timeto.com/"
  ) |>
  req_perform()

data <- resp_body_json(resp, simplifyVector = TRUE)

# Vérification et parts H/F
str(data, max.level = 2)
janitor::tabyl(data, sex)


# Graphique ----

se_dark    <- "#141414"
se_grey    <- "#5A5A5A"
se_light   <- "#DCE1E6"
col_femmes <- "#E2725B"
col_hommes <- "#2B4C7E"

df <-
  data |>
  mutate(
    realtime_sec = as.numeric(as_hms(realTime)),
    realtime_min = realtime_sec / 60,
    sex = fct_relevel(sex, "F", "M")
  ) |>
  filter(!is.na(realtime_sec), realtime_sec > 0)

# Pour la légende et le texte
n_total     <- nrow(df)
n_sub4      <- sum(df$realtime_min < 240)
pct_sub4    <- round(100 * n_sub4 / n_total)
median_time <- median(df$realtime_min)
median_lbl  <- sprintf("%dh%02d", median_time %/% 60, round(median_time %% 60))
q25 <- quantile(df$realtime_min, 0.25)
q25_lbl <- sprintf("%dh%02d", q25 %/% 60, round(q25 %% 60))

ymax <-
  df |>
  mutate(bin = floor(realtime_min)) |>
  count(bin) |>
  summarise(m = max(n)) |>
  pull(m)

t_h <- 2*60 + 5 + 18/60
t_f <- 2*60 + 18 + 34/60
legend_x <- 430
legend_y_h <- ymax * 0.95
legend_y_f <- ymax * 0.88

p <-
  ggplot(df, aes(x = realtime_min, fill = sex)) +
  geom_histogram(binwidth = 1, boundary = 0, position = "stack",
                 color = NA) +
  # Barrière des 4h
  geom_vline(xintercept = 240, color = se_dark, linewidth = 0.5) +
  annotate("label", x = 240, y = ymax * 1.01,
           label = sprintf("Barrière des 4h\n%d %% des finishers en-dessous", pct_sub4),
           family = "roboto", fontface = "bold", size = 3.8,
           color = se_dark, fill = NA,
           label.size = 0, label.padding = unit(0.3, "lines"),
           hjust = 0, lineheight = 1.1) +
  # Top 25%
  geom_vline(xintercept = q25, color = se_grey, linewidth = 0.3,
             linetype = "dashed") +
  annotate("text", x = q25-1, y = ymax * 1.02,
           label = sprintf("25 %% en moins de %s", q25_lbl),
           family = "roboto", fontface = "italic", size = 3.2,
           color = se_grey, hjust = 1) +
  # Vainqueur hommes
  annotate("curve",
           x = 140, xend = t_h + 0.5,
           y = ymax * 0.39, yend = ymax * 0.02,
           curvature = 0.3, color = col_hommes, linewidth = 0.5,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  annotate("text", x = 142, y = ymax * 0.42,
           label = "Yemaneberhan Crippa\n2h05'18\"",
           family = "roboto", fontface = "bold", size = 3.4,
           color = col_hommes, hjust = 0, lineheight = 1.1) +
  # Vainqueur femmes
  annotate("curve",
           x = 140, xend = t_f + 0.5,
           y = ymax * 0.25, yend = ymax * 0.02,
           curvature = 0.3, color = col_femmes, linewidth = 0.5,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  annotate("text", x = 142, y = ymax * 0.28,
           label = "Shure Demise\n2h18'34\"",
           family = "roboto", fontface = "bold", size = 3.4,
           color = col_femmes, hjust = 0, lineheight = 1.1) +
  # Légende
  annotate("point", x = legend_x, y = legend_y_h,
           color = col_hommes, size = 4, shape = 15) +
  annotate("text", x = legend_x + 6, y = legend_y_h,
           label = "Hommes (70 %)", family = "roboto", fontface = "bold",
           size = 3.8, color = se_dark, hjust = 0) +
  annotate("point", x = legend_x, y = legend_y_f,
           color = col_femmes, size = 4, shape = 15) +
  annotate("text", x = legend_x + 6, y = legend_y_f,
           label = "Femmes (30 %)", family = "roboto", fontface = "bold",
           size = 3.8, color = se_dark, hjust = 0) +
  # Scales
  scale_x_continuous(
    limits = c(120, 540),
    breaks = seq(120, 540, 30),
    labels = function(m) sprintf("%dh%02d", m %/% 60, m %% 60),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(
    values = c("F" = col_femmes, "M" = col_hommes),
    guide = "none"
  ) +
  # Légende 2
  labs(
    x = NULL,
    y = "Nombre de coureur·euse·s",
    title = "Marathon de Paris 2026 : qui passe sous les 4 heures ?",
    subtitle = sprintf("Distribution des %s finishers · Temps médian : %s",
                       format(n_total, big.mark = " "), median_lbl),
    caption = "Données : results.timeto.com\nDataviz : Jean Dupin"
  ) +
  # Thème
  theme_minimal(base_family = "atkinson", base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#F4F6F8", color = '#F4F6F8'),
    panel.background   = element_rect(fill = "#F4F6F8", color = "#F4F6F8"),
    panel.grid.major.y = element_line(color = se_light, linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_line(color = se_grey, linewidth = 0.3),
    axis.ticks.length  = unit(4, "pt"),
    axis.text          = element_text(family = "roboto", color = se_grey, size = 11),
    axis.title.y       = element_text(family = "roboto", color = se_dark,
                                      face = "bold", margin = margin(r = 10), size = 11),
    plot.title         = element_text(family = "atkinson", color = se_dark,
                                      face = "bold", size = 19, margin = margin(b = 6)),
    plot.subtitle      = element_text(family = "roboto", color = se_grey,
                                      size = 12.5, margin = margin(b = 20)),
    plot.caption       = element_text(family = "roboto", color = se_grey,
                                      size = 9, hjust = 1, margin = margin(t = 14)),
    plot.margin        = margin(25, 25, 18, 25)
  )

# Vérification (sans camcorder)
p

ggsave("Marathon de Paris 2026/marathon_paris_2026.png", p,
       width = 1200/96, height = 628/96, dpi = 192, bg = "#F4F6F8")

