# Packages ----

library(sf)
library(tidyverse)
library(stringdist)
library(sfdep)


# Données ----

communes <- st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2022/franceentiere/commune_franceentiere_2022.gpkg")


# Méthode ----

# L'idée est dans un premier temps de déterminer les communes à comparer.
# Inutile de comparer des communes de Bretagne avec des communes des Alpes.
# Pour faire la liste des communes à comparer, on va calculer la matrice de
# contigüité d'ordre p, p étant à déterminer (1 pour le moment).


df.test <-
  communes |> 
  select(code, libelle) |> 
  arrange(code) |> 
  mutate(nb = st_contiguity(geometry))


df.nb <- st_contiguity(df.test)



MaF <- function(id) {
  
  
  ls.communes <-
    df.test |>
    st_drop_geometry() |>
    slice(id, unlist(df.nb[id])) |>
    pull(libelle)
  
  
  ls.codes <-
    df.test |>
    st_drop_geometry() |>
    slice(id, unlist(df.nb[id])) |>
    pull(code)
  
  
  
  if (length(ls.communes) <= 1) {
    sortie <-
      data.frame(
        org = "Z",
        code.org = "99999",
        dst = "Z",
        code.dst = "99999",
        dist = 999
      )
    
    
    
  } else {
    sortie <-
      data.frame(
        org = ls.communes[1],
        code.org = ls.codes[1],
        dst = ls.communes[-1],
        code.dst = ls.codes[-1],
        dist = stringdist(ls.communes[1],
                          ls.communes[-1])
      )
    
  }
  
  
  
  sortie |> 
    filter(dist == 1)
  
  
  
}



data.table::fwrite(df.hp |> 
                     select(-dist),
                   "communes_proches.csv",
                   encoding = "UTF-8")


test = data.table::fread("communes_proches.csv",
                         colClasses = "character",
                         encoding = "UTF-8")
