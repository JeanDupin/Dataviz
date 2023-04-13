# Packages ----

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)


font_add_google("Atkinson Hyperlegible", "atkinson")
font_paths("...")
font_add("montserratbold","Montserrat-Bold.ttf")
font_add("montserratmedium","Montserrat-Medium.ttf")
showtext_auto()
showtext_opts(dpi = 600)


# Données ----

source("Population Density/data_prep.R",
       encoding = "UTF-8")


bbox <-
  st_bbox(france)

latitudes <-
  seq(bbox["ymax"], bbox["ymin"],length.out = 900)


# Habillage ----

france <-
  st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 10)

france_10 <- st_buffer(france,10000)
france_5 <- st_buffer(france,5000)



regions <-
  st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)

lignes <-
  st_read("...")


departements <-
  st_read("...") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)



villes <-
  population |>
  arrange(desc(IPONDI)) |>
  slice_head(n = 15) |>
  mutate(LIB = ifelse(LIB == "Saint-Étienne", "Saint-\nÉtienne", LIB)) |>
  (\(x) {
    cbind(st_drop_geometry(x),
          data.frame(st_coordinates(st_centroid(x))))
  })() |>
  mutate(nudge_Y = case_when(
    LIB %in% c("Lille", "Bordeaux") ~ Y - 15000,
    LIB == "Saint-\nÉtienne" ~ Y - 25000,
    T ~ Y + 15000
  ),
  hjust = case_when(
    LIB %in% c("Le Havre","Rennes","Nantes","Bordeaux","Marseille","Toulon","Lyon") ~ 0,
    LIB == "Saint-\nÉtienne" ~ .5,
    T ~ 1
  ))

# Génération ----

map(latitudes,
    function(x){
      
      donnees <-
        population  |>
        (\(x) {
          cbind(st_drop_geometry(x),
                data.frame(st_coordinates(st_centroid(x))))
        })() |>
        filter(Y > x)
      
      part = sprintf("%.4f",(sum(donnees$IPONDI, na.rm = T) / 67162154))
      
      pop = sum(donnees$IPONDI, na.rm = T) |> 
        scales::comma(big.mark = ".", decimal.mark = ",")
      
      if(str_sub(part,1,1) == "1"){
        part = "100.0%"
      } else {
        part = paste0(str_sub(part,3,4),".",str_sub(part,5,6),"%")
      }
      
      
      p <-
        ggplot() +
        # France et contours maritimes
        geom_sf(data = france,
                linewidth = .2,
                fill = "#F2F2F2",
                color = "#F2F2F2") +
        geom_sf(data = france_10,
                linewidth = .2,
                fill = NA,
                color = alpha("#868686",.5)) +
        geom_sf(data = france_5,
                linewidth = .2,
                fill = NA,
                color = alpha("#868686",.8)) +
        # Population
        geom_point(data = donnees,
                   aes(X,Y,
                       color = couleur,
                       fill = couleur,
                       size = IPONDI)) +
        # Habillage
        geom_sf(data = lignes,
                color = "#868686",
                linewidth = .4) +
        geom_sf(data = departements,
                fill = NA,
                color = "#262626",
                linewidth = .1) +
        geom_sf(data = regions,
                fill = NA,
                color = "#262626",
                linewidth = .2) +
        # Titre population
        annotate("text",
                 -Inf, Inf,
                 label = part,
                 hjust = 0, vjust = 2,
                 family = "atkinson", size = 40*.36,
                 fontface = "bold",
                 color = "#F3575A") +
        annotate("text",
                 -Inf, Inf,
                 label = pop,
                 hjust = 0, vjust = 5.75,
                 family = "atkinson", size = 20*.36,
                 fontface = "bold",
                 color = "black") +
        # Source
        annotate("richtext",
                 -Inf, -Inf,
                 label = "<span style='font-family:montserratbold'>Source : </span><span style='font-family:montserratmedium'>Insee - Recensement de la population 2019</span><br><span style='font-family:montserratbold'>Traitements : </span><span style='font-family:montserratmedium'>Jean Dupin - @JeanDup1n</span>",
                 hjust = 0, vjust = -.25,
                 label.color = NA,
                 fill = NA,
                 size = 10*.36,
                 color = "black") +
        # Villes
        geom_point(data = villes,
                   aes(X,Y),
                   shape = 21,
                   color = "#F3575A",
                   fill = "#F3575A",
                   size = 1) +
        shadowtext::geom_shadowtext(data = villes,
                                    aes(X,nudge_Y,label = LIB,
                                        hjust = hjust),
                                    bg.color = "white",
                                    color = "black",
                                    bg.r = .15,
                                    family = "atkinson",
                                    size = 10*.36,
                                    lineheight = .75) +
        #Scales
        scale_size_area(max_size = .8) +
        scale_color_identity() +
        scale_fill_identity() +
        # Theme
        theme_void() +
        theme(legend.position = "none",
              plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#F2F2F2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#F2F2F2")) +
        coord_sf(xlim = bbox[c(1,3)],
                 ylim = bbox[c(2,4)])
      
      
      ggsave(paste0("Population Density/Figures/",which(latitudes == x),".png"),
             plot = p,
             height = 21, width = 21, units = "cm",
             dpi = 600)
      
      cat(crayon::green(paste0("\n \nPlot N°",
                               which(latitudes == x),"/",
                               length(latitudes),
                               " réalisé\n \n ")))
      
    },
    .progress = T)


