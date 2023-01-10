# Packages ----

library(tidyverse)
library(friends)

library(ggstream)
library(patchwork)

library(ggtext)
library(extrafont)
loadfonts(device="win")


# Données ----


test <- friends_info |> 
  select(season, written_by) |> 
  mutate(not_cool = str_locate(written_by,"Teleplay by :")[,"start"],
         written_by = ifelse(!is.na(not_cool),
                             str_sub(written_by,1,not_cool-1),
                             written_by),
         written_by = str_remove(written_by,"Story by : "),
         is_not_nice = str_locate(written_by,"[a-z][A-Z]")[,"end"],
         mac = str_detect(written_by,"Mc"),
         written_by = ifelse(isFALSE(mac) & !is.na(is_not_nice),
                             paste0(str_sub(written_by,1,is_not_nice - 1),
                                    " & ",
                                    str_sub(written_by,is_not_nice)),
                             written_by),
         authors = str_split(written_by," & ")) |> 
  select(season, authors) |> 
  unnest(authors)

big_10_writters <- test |> 
  group_by(authors) |> 
  summarise(total = n()) |> 
  ungroup() |> 
  arrange(desc(total)) |> 
  slice_head(n = 10) |> 
  pull(authors)


writers <- test |> 
  filter(authors %in% big_10_writters) |> 
  group_by(season, authors) |> 
  summarise(nb = n()) |> 
  ungroup(); rm(test)


# Annotations ----


add_season <- function(season_number) {
  geom_richtext(aes(x = season_number - .2, y = -24,
                    label = season_number),
                family = "Gabriel Weiss' Friends Font",
                size = 5,
                #angle = 90,
                fill = NA, label.colour = NA)
}







sous_titre <- "10 authors have written more than 160 episodes, among the 236 of the series.<br>In a decade, they were able to install Friends as the essential series of the 90s."
ndl <- "**Data**: *{friends}* package | **Traitements** : Jean Dupin - @JeanDup1n"



# Plot ----

ggplot() +
  geom_segment(aes(x = seq(1, 10, 1), xend = seq(1, 10, 1),
                   y = -25, yend = 25), colour = "grey10",
               size = 0.3, lty = 5) +
  map(1:10, add_season) +
  # Flux
  geom_stream(data = writers |> 
                rbind(data.frame(season = 0,
                                 authors = big_10_writters,
                                 nb = 0),
                      data.frame(season = 11,
                                 authors = big_10_writters,
                                 nb = 0)) |> 
                arrange(season),
              aes(x = season,
                  y = nb,
                  fill = authors),
              extra_span = .2,
              bw = .8,
              sorting = "onset",
              n_grid = 5000) +
  scale_x_continuous(breaks = 1:10,
                     labels = paste("Season",1:10)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Homer1",10)) +
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
       title = "THE ONE WITH THE AUTHOR",
       subtitle = sous_titre,
       caption = ndl,
       y = "Number of episodes written") +
  guides(fill = guide_legend(byrow = T))


# Save ----

ggsave("writters.pdf",
       width = 29.7,
       height = 17,
       units = "cm",
       device = cairo_pdf)
