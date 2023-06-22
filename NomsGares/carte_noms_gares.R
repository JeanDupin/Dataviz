# Packages ----

library(tidyverse)
library(sf)
library(data.table)
library(showtext)
library(ggtext)

# Autres paramètres ----

transform_shapefile <- function(shp){
  
  
  continent <-
    shp %>% dplyr::filter(reg != "94")
  
  corse <- shp %>% dplyr::filter(reg == "94") %>%
    dplyr::mutate(geometry = geometry + c(-166400,0)) %>%
    sf::st_set_crs(2154)
  
  zoom <- shp %>% dplyr::filter(dep %in% c(75,92,93,94)) %>%
    # dplyr::mutate(geometry = geometry * 2.78 + c(-1634506,-12046235)) |> 
    dplyr::mutate(geometry = geometry * 2.78 + c(-800506,-12046235))
  
  shp_dep_trans = continent %>%
    dplyr::bind_rows(corse) %>%
    dplyr::bind_rows(zoom)
  
  return(shp_dep_trans)
}

font_add_google("Atkinson Hyperlegible", "atkinson")
font_paths("...")
font_add("montserratbold","Montserrat-Bold.ttf")
font_add("montserratmedium","Montserrat-Medium.ttf")
showtext_auto()
showtext_opts(dpi = 600)

# Données ----

gares <- 
  fread("NomsGares/gares.csv") |> 
  mutate(libelle = str_to_lower(libelle),
         indicatrice = ifelse(
           str_detect(libelle,"gare"),
           "Oui",
           "Non"
         ),
         indicatrice = factor(indicatrice,
                              levels = c("Oui","Non"))) |> 
  st_as_sf(coords = c("x",
                      "y"),
           crs = 2154) |> 
  transform_shapefile()




# Fonds de carte ----


france <- st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 10)

france_10 <- st_buffer(france,10000)
france_5 <- st_buffer(france,5000)


regions <- st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)


idf <-
  st_read("...") |> 
  slice(1:8) |> 
  mutate(geometry = geometry + c(834000,0)) |> 
  st_set_crs(2154)


# Décalage du rond
lignes <-
  st_read("...") |> 
  filter(ID %in% c("2","1"))


lignes2 <-
  st_read("...") |> 
  filter(ID %in% c("4")) |> 
  mutate(geometry = geometry + c(834000,0)) |> 
  st_set_crs(2154)


lignes3 <-
  st_read("...",
          quiet = T) |> 
  filter(ID %in% c("3"))

lignes3$geometry[[1]][1][[1]] <- matrix(c(651748.1 + 2*(651748.1 - 651748.1),
                                          418627.1 + 2*(651748.1 - 418627.1),
                                          273514.4 + 2*(651748.1 - 273514.4),
                                          6861891, 7029174,7030518),
                                        3,2, byrow = F)

lignes3$geometry[[1]][2][[1]] <- matrix(c(1029982, 7030518,
                                          1029982, 7030518),
                                        2,2, byrow = T)


lignes3 <-
  st_difference(lignes3,
                st_polygonize(lignes2))





# Carte ----


camcorder::gg_record(width = 21, height = 21, units = "cm", dpi = 600)


ggplot() +
  # France - Fond de carte
  geom_sf(data = france_10,
          linewidth = .2,
          fill = NA,
          color = alpha("#868686",.5)) +
  geom_sf(data = france_5,
          linewidth = .2,
          fill = NA,
          color = alpha("#868686",.8)) +
  geom_sf(data = regions,
          fill = "#F2E6D8",
          color = "#F2E6D8",
          linewidth = .2) +
  geom_sf(data = idf,
          fill = "#F2E6D8",
          color = "#262626",
          linewidth = .2) +
  # Gares
  geom_sf(data = gares,
          aes(fill = indicatrice,
              color = indicatrice),
          size = .7,
          alpha = .9,
          shape = 19,
          key_glyph = draw_key_point) +
  # Habillage
  geom_sf(data = lignes,
          color = "#868686",
          linewidth = .4) +
  geom_sf(data = lignes2,
          color = "#868686",
          linewidth = .4) +
  geom_sf(data = lignes3,
          color = "#868686",
          linewidth = .4) +
  geom_sf(data = regions,
          fill = NA,
          color = "#262626",
          linewidth = .2) +
  geom_sf(data = idf,
          fill = NA,
          color = "#262626",
          linewidth = .2) +
  # Couleurs et scales
  scale_fill_manual(values = c("#FFA95E","#8988E8"),
                    guide = "none") +
  scale_color_manual(values = c("#FFA95E","#8988E8"),
                     labels = c("... de la gare", "... pas de la gare")) +
  guides(color = guide_legend(title.position = "top",
                              direction = "vertical",
                              override.aes = list(size = 5,
                                                  alpha = 1))) +
  labs(color = "Rue / Avenue / ...") +
  # Source
  annotate("richtext",
           -Inf, -Inf,
           label = "<span style='font-family:montserratbold'>Source : </span><span style='font-family:montserratmedium'>SNCF - Gare de voyageurs</span><br><span style='font-family:montserratbold'>Traitements : </span><span style='font-family:montserratmedium'>Jean Dupin - @JeanDup1n</span>",
           hjust = 0, vjust = -.25,
           label.color = NA,
           fill = NA,
           size = 10*.36,
           color = "black") +
  # Theme
  theme_void() +
  theme(legend.direction = "horizontal",
        legend.text = element_markdown(
          family = "atkinson",
          size = 16,
          color = "black"
        ),
        legend.title = element_text(
          family = "atkinson",
          size = 18,
          face = "bold",
          color = "black",
          hjust = 0,
          margin = margin(b = 0,unit = "mm")
        ),
        legend.position = c(.175,.91),
        legend.title.align = .5,
        legend.key.width = unit(.2,"cm"),
        legend.key.height = unit(.8,"cm"),
        plot.background = element_rect(fill = "#F2F2F2",
                                       color = "#F2F2F2"),
        panel.background = element_rect(fill = "#F2F2F2",
                                        color = "#F2F2F2")) +
  coord_sf(st_bbox(france)[c(1,3)],
           st_bbox(france)[c(2,4)],
           crs = 2154)


ggsave("NomsGares/gares.png",
       width = 21, height = 21, units = "cm", dpi = 600)
