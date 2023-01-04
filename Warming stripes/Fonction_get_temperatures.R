get_temperatures <- function(mon_url,
                             annees) {
  purrr::map2_dfr(mon_url,
                  annees,
                  function(base_url,z){
                    
                    meteo1 <-
                      paste0(base_url,
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
}