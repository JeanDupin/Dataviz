# Packages ----

library(tidyverse)
library(scales)
library(sf)
library(patchwork)


# Polices ----

# extrafont::font_import(read.table("fontsff.txt") |> pull())
extrafont::loadfonts(device="win",
                     quiet = T)

# Données ----

load(file = "donnees_pour_plot.RData")

`%notin%` <- Negate(`%in%`)



layout <- c(
  area(1,1,1,2),
  area(1,4,1,5),
  area(2,1,2,5)
)


liste_etapes <- liste_etapes |> 
  select(3) |> 
  separate(1,
           into = c("Start","End"),
           sep = "–") |> 
  mutate(etape = levels(distance_totale$name),
         .before = 1) |> 
  mutate(End = str_remove(End, "^  "),
         End = str_remove(End, "^ "))


# Fontion pour plot ----

create_etape <- function(stage) {
  
  ville_depart <- liste_etapes |> 
    filter(etape == stage) |> 
    mutate(across(2:3,~str_replace_all(.," "," "))) |> 
    pull(Start)
  
  ville_arrive <- liste_etapes |> 
    filter(etape == stage) |> 
    mutate(across(2:3,~str_replace_all(.," "," "))) |> 
    pull(End)
  
  ggplot(distance_totale |> 
           filter(name == stage),
         aes(dist, ele)) +
    geom_area(fill = "#f5db2e") +
    # Coords
    scale_x_continuous(expand = expansion()) +
    {if(stage %in% c("Stage 8","Stage 15")){
      scale_y_continuous(expand = expansion(),
                         labels = NULL,
                         limits = c(0,2600),
                         breaks = seq(0,2500,500))
    } else {
      scale_y_continuous(expand = expansion(),
                         labels = comma_format(big.mark = " "),
                         limits = c(0,2600),
                         breaks = seq(0,2500,500))
    }} +
    # Villes
    annotate("text",
             x = 0, y = 2250,
             family = "Smudger LET", size = 3, color = "white", hjust = 0,
             label = ville_depart) +
    annotate("text",
             x = 0, y = 1750,
             family = "Smudger LET", size = 3, color = "white", hjust = 0,
             label = ville_arrive) +
    # Thème
    theme(strip.text = element_blank(),
          plot.margin = margin(b = 30, l = 5, r = 5),
          plot.background = element_rect(fill = "#121212",
                                         color = NA),
          panel.background = element_rect(fill = "#121212",
                                          color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "#BFBFBF",
                                            size = 0.25),
          text = element_text(family = "sans", size = 6, color = "#BFBFBF"),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 6, color = "white",
                                     family = "Roboto"),
          axis.title = element_blank()) +
    if(stage %notin% c("Stage 1", "Stage 8", "Stage 15")){
      theme(axis.text.y = element_blank())
    }
}

create_etape("Stage 8")


# Plot ----




sous_titre <- glue::glue("La 109e édition du Tour, c'est 3400 km de course, 50 200 m de dénivelé positif... le tout en 80 h d'effort.<br>
                         Dans sa lutte avec <span style='color:#107bc7;'>**Tadej Pogačar**</span>, le gagnant <span style='color:#f5db2e;'>**Jonas Vingegaard**</span> aura pu compter sur l'infatigable <span style='color:#3cb250;'>**Wout van Aert**</span>.<br>
                         Du Danemark à Paris, c'est bien le maillot vert qui a marqué cette édition, remportant au passage le titre de <span style='color:#f00033;'>Super-Combatif</span>.")


ndl <- glue::glue("**Source :** letour.fr | **Traitements et graphique :** Jean Dupin - @JeanDup1n")


plots <- map(levels(distance_totale$name), create_etape)

wrap_plots(plots,
           nrow = 3,
           ncol = 7,
           byrow = T) +
  plot_annotation(title = "Tour de France 2022",
                  subtitle = sous_titre,
                  caption = ndl,
                  theme = list(
                    plot.title = element_text(size = 40, color = "#f5db2e",
                                              family = "Smudger LET"),
                    plot.subtitle = ggtext::element_markdown(size = 14, color = "white",
                                                             family = "Roboto",
                                                             margin = margin(b = 15),lineheight = 1.2),
                    plot.caption = ggtext::element_markdown(family = "Roboto",face = "bold",
                                                color = "white", size = 8),
                    plot.background = element_rect(fill = "#121212",
                                                   color = NA),
                    panel.background = element_rect(fill = "#121212",
                                                    color = NA)
                  ))




ggsave("letour.png", height = 15, width = 30, units = "cm")


