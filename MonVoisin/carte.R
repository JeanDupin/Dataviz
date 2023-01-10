# Packages ----

library(sf)
library(tidyverse)

# Données ----



communes <- 
  st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2022/franceentiere/commune_franceentiere_2022.gpkg")



communes_proches <- 
  data.table::fread("communes_proches.csv",
                    colClasses = "character",
                    encoding = "UTF-8")


communes_proches <-
  communes_proches |> 
  select(code = code.org,
         lib = org) |> 
  mutate(couleur = "A") |> 
  bind_rows(
    communes_proches |> 
      select(code = code.dst,
             lib = dst) |> 
      mutate(couleur = "B")
  ) |> 
  left_join(communes |> 
              select(code),
            by = "code") |> 
  st_as_sf() |> 
  st_transform(crs = 2154)

communes_proches <-
  communes_proches |>
  st_drop_geometry() |> 
  bind_cols(communes_proches |> 
              st_centroid() |> 
              st_coordinates()); rm(communes)



france <-
  st_read("Regions_France_Metro.gpkg") |> 
  filter(code != "94") |> 
  st_union() |> 
  st_as_sf() |> 
  smoothr::smooth("ksmooth", smoothness = 5) 



eau <- 
  st_read("Mer.gpkg")

europe <- 
  st_read("Pays_Europe.gpkg")




# Carte ----


camcorder::gg_record(width = 21,
                     height = 21,
                     units = "cm")


ggplot() +
  geom_sf(data = eau,
          size = 0,
          fill = "#8DB0E1") +
  geom_sf(data = europe,
          fill = "#C6C6C6",
          size = .2,
          color = "#868686") +
  geom_sf(data = france,
          size = .2,
          fill = "#f7f4e3",
          color = "#868686") +
  geom_point(data = communes_proches,
          aes(X, Y,
              color = couleur),
          size = 3,
          shape = 16) +
  scale_color_manual(values = c(alpha("#286AC7",.8), alpha("#EB617F",.8))) +
  theme_void() +
  theme(legend.direction = "horizontal",
        legend.text = element_blank(),
        # legend.position = c(.815,.06),
        legend.position = "none",
        legend.title.align = .5,
        legend.key.width = unit(1.3,"cm"),
        legend.key.height = unit(.4,"cm"),
        plot.background = element_rect(fill = "#fdf9fb",
                                       color = "#fdf9fb"),
        panel.background = element_rect(fill = "#fdf9fb",
                                        color = "#fdf9fb")) +
  guides(fill = guide_colorsteps(title.position = "top")) +
  coord_sf(st_bbox(france)[c(1,3)],
           st_bbox(france)[c(2,4)]) +
  labs(color = "")




ggsave("copieur.png",
       width = 21,
       height = 21,
       units = "cm")

