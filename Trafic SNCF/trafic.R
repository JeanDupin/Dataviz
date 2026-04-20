# Packages ----

library(dplyr)
library(sf)
library(ggplot2)
library(showtext)
library(gifski)
library(ggtext)

# Données SNCF en pseudo temps réel ----
# Données du 14/04/2026 fournies par Nicolas Wurtz
# Plus de détails ici : https://carto.tchoo.net/
# Les données ne reflètent pas les déplacements réels des trains,
# mais sont une bonne approximation des trajets effectués.

data <-
  jsonlite::read_json("Donnees/20260414_geoloc.json")$data

journee <-
  vector("list", length(data))

for(i in seq_along(data)){
  sub_data <-
    data[[i]]
  minute <-
    sub_data[["minute"]]
  
  if(is.null(sub_data$json$features)){next}
  
  trains <-
    lapply(
      sub_data$json$features,
      function(.x){
        data.frame(
          id = .x[[2]][["numero"]],
          type = switch(.x[[2]][["origine"]],
                        "transilien" = "Transilien",
                        "ter" = "TER",
                        "tgv" = "TGV",
                        "intercites" = "Intercités",
                        "zou" = "TER",
                        "ouigo" = "TGV",
                        "lyria" = "TGV",
                        "ice" = "TGV"),
          angle = .x[[2]][["angle"]],
          x = .x[[1]][[1]][[1]],
          y = .x[[1]][[1]][[2]]
        )
      }
    )|> 
    do.call(bind_rows, args = _) |> 
    st_as_sf(
      crs = 4326,
      coords = c("x","y")
    ) |> 
    mutate(minute = minute,
           couleur = case_when(
             type == "Transilien" ~ "#641874",
             type == "TGV" ~ "#ea0f58",
             type == "Intercités" ~ "#5C9E00",
             type == "TER" ~ "#0591d1"
           ))
  
  journee[[minute]] <- trains
  
}

journee <-
  bind_rows(journee)

# Données géographiques ----

pays <-
  st_read("Donnees/pays.gpkg",
          quiet = T)
fm <-
  st_read("Donnees/francem.gpkg",
          quiet = T)
francecont <-
  st_intersection(
    fm,
    filter(st_read("Donnees/reg.gpkg"), code != "94")
  ) |> 
  st_union() |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 10)
mer <-
  st_read("Donnees/mer.gpkg",
          quiet = T)
reg <-
  st_read("Donnees/reg.gpkg",
          quiet = T) |> 
  filter(code != "94") |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)
dep <-
  st_read("Donnees/dep.gpkg",
          quiet = T) |> 
  filter(!(code %in% c("2A","2B"))) |> 
  smoothr::smooth(method = "ksmooth",
                  smoothness = 5)
corse <-
  st_read("Donnees/reg.gpkg") |> 
  filter(code == "94")

# Statistiques ----

# Nombre de trains
st_drop_geometry(journee) |> 
  distinct(id, type) |> 
  nrow()
# 12.457 - Sûrement plus puisqu'un numéro peut être partagé le même jour.

# Nombre d'observations
nrow(journee)
# 838.713 - Pour chaque minute, position des trains en circulation

# Graphique ----

# Conversion pour le temps
minutes_vers_hm <- function(m) {
  sprintf("%02d:%02d", m %/% 60, m %% 60)
}

# Polices
font_add_google("Atkinson Hyperlegible", "atkinson")
font_paths("Donnees")
font_add("montserratbold","Montserrat-Bold.ttf")
font_add("montserratmedium","Montserrat-Medium.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# Pour le texte de légende manuel
legende = -50000
legende.size = 9*.36

save_gif(
  {
    # On commence à 03:45 - Jusqu'à ~1h, on a les trains de la veille
    # Ensuite aucun train ne circule avant ~4h
    for(i in seq_len(length(data))[-seq_len(225)]){
      n.trains <-
        filter(journee, minute == i) |>
        nrow() |> 
        scales::comma(big.mark = ".",
                      decimal.mark = ",")
      
      p <-
        ggplot() +
        # Fonds
        geom_sf(data = mer,
                color = "#aed6eb",
                fill = "#aed6eb",
                lwd = .1) +
        geom_sf(data = pays,
                fill = "#F2F2F2",
                color = "#F2F2F2",
                lwd = .001) +
        geom_sf(data = dep,
                fill = "#F2F2F2",
                color = "black",
                lwd = .01) +
        geom_sf(data = reg,
                fill = NA,
                color = "black",
                lwd = .02) +
        geom_sf(data = francecont,
                fill = NA,
                color = "black",
                lwd = .05) +
        geom_sf(data = corse,
                fill = "#F2F2F2",
                color = "#F2F2F2",
                lwd = .001) +
        # Trains
        geom_sf(aes(color = I(couleur)),
                data = filter(journee, minute == i),
                size = .15) +
        # Horaire
        annotate("text", label = minutes_vers_hm(i),
                 x = 1242370 - 200000, y = 7110480 - 50000,
                 family = "atkinson", size = 8, fontface = "bold",
                 color = "#F3575A") +
        # Nombre de trains en direct
        annotate("text", label = paste0(n.trains," trains"),
                 x = 1242370 - 65000, y = 7110480 - 120000,
                 family = "atkinson", size = 2.5, fontface = "bold",
                 hjust = 1,
                 color = "black") +
        # Types de trains
        annotate("text", label = "TGV",
                 x = 110000, y = 6580064,
                 family = "atkinson", size = legende.size,
                 hjust = 0,
                 color = "#ea0f58") +
        annotate("text", label = "Transilien",
                 x = 110000, y = 6580064 + legende*1,
                 family = "atkinson", size = legende.size,
                 hjust = 0,
                 color = "#641874") +
        annotate("text", label = "Intercités",
                 x = 110000, y = 6580064 + legende*2,
                 family = "atkinson", size = legende.size,
                 hjust = 0,
                 color = "#5C9E00") +
        annotate("text", label = "TER",
                 x = 110000, y = 6580064 + legende*3,
                 family = "atkinson", size = legende.size,
                 hjust = 0,
                 color = "#0591d1") +
        # Source et crédits
        annotate("richtext",
                 -Inf, -Inf,
                 label = "<span style='font-family:montserratbold'>Source : </span><span style='font-family:montserratmedium'>Nicolas Wurtz - Carto Tchoo</span><br><span style='font-family:montserratbold'>Traitements : </span><span style='font-family:montserratmedium'>Jean Dupin</span>",
                 hjust = 0, vjust = -.15,
                 label.color = NA,
                 fill = NA,
                 size = 5*.36,
                 color = "black") +
        # Thème
        theme_void() +
        coord_sf(
          xlim = st_bbox(fm)[c(1,3)] + c(-1,1)*25000,
          ylim = st_bbox(fm)[c(2,4)] + c(-1,1)*(25000+41155.5),
          expand = F
        )
      
      print(p)
      
    }
  },
  gif_file = "trains.gif",
  width = 1080, height = 1080, delay = 0.05,
  res = 300
)






