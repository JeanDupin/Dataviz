# Packages ----

library(tidyverse)
library(sf)
library(sfdep)


# Données ----


france <-
  st_read("...",
          options = "ENCODING=WINDOWS-1252")


# Disponible sur Statistiques Locales
pop <-
  data.table::fread("Population Density\\data.csv",
                    encoding = "UTF-8",
                    skip = 2) |> 
  (\(x){`colnames<-`(x,
                     c("code","LIB","IPONDI"))})() |> 
  mutate(IPONDI = as.numeric(IPONDI))


# Mise en forme ----

population <-
  pop |> 
  left_join(select(france,code),
            by = "code") |> 
  st_as_sf() 



# Lissage simple des voisins par la moyenne pour les couleurs ----


nb <-
  st_nb_lag_cumul(st_contiguity(population),2)


population_sans_sf <-
  st_drop_geometry(population)


moyennes_pop <-
  map(1:nrow(population_sans_sf),
      function(x) {
        population_sans_sf |>
          slice(x, nb[[x]]) |>
          pull(IPONDI) |>
          mean()
      },
      .progress = T) |>
  unlist()
                          
population <-
  population |>
  mutate(IPONDIL = moyennes_pop,
         alpha = cut_number(IPONDIL,10,labels = F)/10 - .1,
         couleur = ifelse(!is.na(IPONDI),
                          alpha("#F37358",alpha),
                          "grey")); rm(nb, moyennes_pop, population_sans_sf)

