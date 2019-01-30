```{r}
quakes <- quakes %>%
  dplyr::mutate(mag.level = cut(mag,c(3,4,5,6),
                                labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))


rd_parties <- split(rd_centroids, rd_centroids$winning_party)

leaflet(rd_leaf) %>%
  addPolygons(label = ~stringr::str_c(NM_CEP, ", ",
                                      winning_party, ", ",
                                      vote_share, ", "),
              weight = 1, 
              fillOpacity = 0.5,
              color = ~partycol(winning_party)) %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite)  %>%
  addMarkers(data = rd_parties$CAQ, group = "CAQ") 
addAwesomeMarkers(data = rd_parties$LIB, group = "LIB", 
                  icon = awesomeIcons( 
                    iconColor = "black", 
                    markerColor = "black")) %>%
  addMarkers(data = rd_parties$PQ, group = "PQ") %>%
  addMarkers(data = rd_parties$QS, group = "QS") %>%
  addLayersControl(overlayGroups = c("CAQ", "LIB", "PQ", "QS"),
                   options = layersControlOptions(collapsed = FALSE))

```


## Add markers 

```{r}
rd_centroids$winning_party <- as.character(rd_centroids$winning_party)
party_icons <- awesomeIconList(
  CAQ = makeAwesomeIcon(icon = 'checkbox', library = 'ion', markerColor = "deepskyblue1"),
  LIB = makeAwesomeIcon(icon = 'checkbox', library = 'ion', markerColor = "pink"),
  PQ = makeAwesomeIcon(icon = 'checkbox', library = 'ion', markerColor = "royalblue4"),
  QS = makeAwesomeIcon(icon = 'checkbox', library = 'ion', markerColor = "pink")
)
```

```{r}
leaflet(rd_leaf) %>%
  addPolygons(label = ~stringr::str_c(NM_CEP, ", ",
                                      winning_party, ", ",
                                      vote_share, ", "),
              weight = 1, 
              fillOpacity = 0.5,
              color = ~partycol(winning_party)) %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite)  %>%
  addLegend(pal = partycol, values = ~winning_party, opacity = 1) %>%
  addAwesomeMarkers(data = rd_centroids, ~long, ~lat, icon = ~party_icons[winning_party])

```



rd_parties <- split(rd_centroids, rd_centroids$winning_party)

leaflet(rd_leaf) %>%
  addPolygons(label = ~stringr::str_c(NM_CEP, ", ",
                                      winning_party, ", ",
                                      vote_share, ", "),
              weight = 1, 
              fillOpacity = 0.5,
              color = ~partycol(winning_party)) %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite)  %>%
  addMarkers(data = rd_parties$CAQ, group = "CAQ") 
addAwesomeMarkers(data = rd_parties$LIB, group = "LIB", 
                  icon = awesomeIcons( 
                    iconColor = "black", 
                    markerColor = "black")) %>%
  addMarkers(data = rd_parties$PQ, group = "PQ") %>%
  addMarkers(data = rd_parties$QS, group = "QS") %>%
  addLayersControl(overlayGroups = c("CAQ", "LIB", "PQ", "QS"),
                   options = layersControlOptions(collapsed = FALSE))

```


```{r}
leaflet(rd_leaf) %>%
  addPolygons(label = ~stringr::str_c(NM_CEP, ", ",
                                      winning_party, ", ",
                                      vote_share, ", "),
              weight = 1, 
              color = ~partycol(winning_party)) %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addMarkers(data = rd_centroids, label = ~as.character(unemployment_rate))
```