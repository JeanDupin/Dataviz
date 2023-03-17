# Packages ----

library(tidyverse)
library(data.table)

library(sf)

# Données ----

noms <-
  fread("Saint Patrick/dpt2021.csv")


# Table des Patrick

noms <-
  noms |> 
  filter(preusuel == "PATRICK") |> 
  group_by(DEP = dpt) |> 
  summarise(patrick = sum(nombre)) |> 
  left_join(
    noms |> 
      group_by(DEP = dpt) |> 
      summarise(total = sum(nombre)),
    by = "DEP"
  ) |> 
  mutate(part = 10000*patrick / (patrick + total)) |> 
  arrange((part))




# Fonds de carte ----



france <- st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 10)




regions <- st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)


lignes <-
  st_read("...")


deps <-
  st_read("...") |> 
  mutate(code = ifelse(code == "2A" | code == "2B",
                       "20",
                       code))|> 
  left_join(noms,
            by = c("code" = "DEP")) |> 
  mutate(part = floor(part/20)) |>
  arrange(part) |> 
  mutate(part = fct_inorder(factor(part)))



# Carte ----


camcorder::gg_record(width = 21, height = 21, units = "cm")


ggplot() +
  # France
  geom_sf(data = st_buffer(france,10000),
          size = .2,
          fill = NA,
          color = alpha("#868686",.5)) +
  geom_sf(data = st_buffer(france,5000),
          size = .2,
          fill = NA,
          color = alpha("#868686",.8)) +
  
  geom_sf(data = deps,
          aes(fill = part),
          size = .5,
          color = alpha("#262626",.25)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Paquin", 12)[8:12],
                    na.value = "#B0AEAE") +
  # Habillage
  geom_sf(data = lignes,
          color = "#868686",
          size = .4) +
  geom_sf(data = regions,
          fill = NA,
          color = "#262626",
          size = .2) +
  theme_void() +
  theme(legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.position = c(.215,.9),
        legend.title.align = .5,
        legend.key.width = unit(1.3,"cm"),
        legend.key.height = unit(.4,"cm"),
        plot.background = element_rect(fill = "#F2F2F2",
                                       color = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2",
                                        color = "#F2F2F2")) +
  guides(fill = guide_colorsteps(title.position = "top"),
         color = "none") +
  coord_sf(st_bbox(france)[c(1,3)],
           st_bbox(france)[c(2,4)]) +
  labs(fill = "")


ggsave("Saint Patrick/patrick.png",
       width = 21, height = 21, units = "cm")



