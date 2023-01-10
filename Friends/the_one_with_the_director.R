# Packages ----

library(tidyverse)
library(friends)

library(ggstream)
library(patchwork)

library(ggtext)
library(extrafont)
loadfonts(device="win")


# Données ----


directors <- friends_info |> 
  group_by(season, directed_by) |> 
  summarise(nb = n()) |> 
  ungroup()

# 10 biggest directors

big_10 <- directors |> 
  group_by(directed_by) |> 
  summarise(a = sum(nb)) |> 
  arrange(desc(a)) |> 
  slice(1:10) |>
  pull(directed_by)


# Annotations ----


add_season <- function(season_number) {
  geom_richtext(aes(x = season_number - .2, y = -24,
                    label = season_number),
                family = "Gabriel Weiss' Friends Font",
                size = 5,
                #angle = 90,
                fill = NA, label.colour = NA)
}







sous_titre <- "10 directors have directed more than 200 episodes, among the 236 of the series.<br>In a decade, they were able to install Friends as the essential series of the 90s."
ndl <- "**Data**: *{friends}* package | **Traitements** : Jean Dupin - @JeanDup1n"



# Plot ----

ggplot() +
  geom_segment(aes(x = seq(1, 10, 1), xend = seq(1, 10, 1),
                   y = -25, yend = 25), colour = "grey10",
               size = 0.3, lty = 5) +
  map(1:10, add_season) +
  # Flux
  geom_stream(data = directors |> 
                filter(directed_by %in% big_10) |> 
                rbind(data.frame(season = 0,
                                 directed_by = big_10,
                                 nb = 0),
                      data.frame(season = 11,
                                 directed_by = big_10,
                                 nb = 0)) |> 
                arrange(season),
              aes(x = season,
                  y = nb,
                  fill = directed_by),
              extra_span = .2,
              bw = .8,
              sorting = "onset",
              n_grid = 5000) +
  scale_x_continuous(breaks = 1:10,
                     labels = paste("Season",1:10)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Manet",10)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#bfaed4",
                                       color = "#bfaed4"),
        panel.background = element_rect(fill = "#bfaed4",
                                        color = "#bfaed4"),
        legend.position = "bottom",
        legend.margin = margin(unit = "cm",
                               t = .5,
                               b = 1),
        legend.text = element_text(family = "Montserrat",
                                   margin = margin(r = .5, unit = "cm")),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(.5, "cm"),
        plot.margin = margin(unit = "cm",
                             t = 1),
        plot.title = element_markdown(family = "Gabriel Weiss' Friends Font", size = 30,
                                      colour = "#134130",
                                      margin = margin(l = 1.5,
                                                      b = 1,
                                                      unit = "cm")),
        plot.subtitle = element_markdown(family = "Montserrat", size = 15,
                                         colour = "#134130",
                                         margin = margin(l = 1.5,
                                                         b = .5,
                                                         unit = "cm"),
                                         lineheight = 1.3),
        plot.caption = element_markdown(family = "Montserrat", size = 8,
                                        colour = "#134130",
                                        margin = margin(b = .2,
                                                        r = .2,
                                                        unit = "cm")),
        axis.title.y = element_text(family = "Montserrat", size = 8,
                                    angle = 90,
                                    margin = margin(unit = "cm",
                                                    r = -.8,
                                                    l = .8))) +
  labs(fill = "",
       title = "THE ONE WITH THE DIRECTOR",
       subtitle = sous_titre,
       caption = ndl,
       y = "Number of episodes directed") +
  guides(fill = guide_legend(byrow = T))


# Save ----

ggsave("directors.pdf",
       width = 29.7,
       height = 17,
       units = "cm",
       device = cairo_pdf)