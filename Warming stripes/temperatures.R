
library(rvest)

temperatures <-
  purrr::map_dfr(2022,
             function(z){
               
               meteo1 <-
                 paste0("https://www.historique-meteo.net/france/bourgogne/dijon/",
                        z,
                        "/")
               
               purrr::map2_dfr(meteo1,
                               1:12,
                              function(zb,y) {
                                if (y < 10) {
                                  meteo <-
                                    paste0(zb,
                                           "0",
                                           y,
                                           "/")
                                } else {
                                  meteo <-
                                    paste0(zb,
                                           y,
                                           "/")
                                }
                                
                                
                                
                                dijon <- read_html(meteo) |>
                                  html_nodes(xpath = "/html/body/div[1]/main/section[3]/div/div/div[1]/table[2]/tbody") |>
                                  html_children()
                                
                                purrr::map_dfr(dijon,
                                               function(x) {
                                                 a <-
                                                   x |>
                                                   html_text() |>
                                                   stringr::str_sub(1, 10)
                                                 
                                                 b <-
                                                   x |>
                                                   html_text() |>
                                                   stringr::str_extract(pattern = "Températures : .*C") |>
                                                   stringr::str_remove("Températures : ") |>
                                                   stringr::str_remove_all("°C") |>
                                                   stringr::str_split_fixed("/", 2) |>
                                                   as.numeric() |>
                                                   mean()
                                                 
                                                 
                                                 data.frame(date2 = a,
                                                            temp = b)
                                               })
                                
                                
                              }) 
               
               
               
             }) |> 
  dplyr::mutate(date2 = lubridate::dmy(date2))



library(ggplot2)

mes_couleurs <-
  RColorBrewer::brewer.pal(11, "RdBu")


ggplot() +
  geom_col(data = dplyr::mutate(temperatures,
                         indic = 1) |> 
             dplyr::slice_tail(n = 365),
           aes(x = date2,
               y = indic,
               color = temp,
               fill = temp)) +
  scale_color_gradientn(colors = rev(mes_couleurs)) +
  scale_fill_gradientn(colors = rev(mes_couleurs)) +
  theme_void() +
  theme(legend.position = "none")




