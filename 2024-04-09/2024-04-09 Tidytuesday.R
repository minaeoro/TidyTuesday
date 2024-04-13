library(tidytuesdayR)
library(tidyverse)
library(maps)
library(showtext)
library(hms)
library(ggbeeswarm)
library(patchwork)
library(sessioninfo)


### 1. Loading the data ----------
tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

eclipse_annular_2023 <- tuesdata$eclipse_annular_2023
eclipse_total_2024 <- tuesdata$eclipse_total_2024
eclipse_partial_2023 <- tuesdata$eclipse_partial_2023
eclipse_partial_2024 <- tuesdata$eclipse_partial_2024


### 2. Data Wranggling & Exploration ----------
## eclipse_data : a tibble binding the tibbles "eclipse_annular_2023" and "eclipse_total_2024"
eclipse_data <- eclipse_annular_2023 %>%
                  mutate(year = "2023") %>%
                    bind_rows(eclipse_total_2024 %>%
                                mutate(year = "2024")) %>%
                      select(state, name, lat, lon, eclipse_3, eclipse_4, year) %>% 
                          mutate(duration = difftime(eclipse_4, eclipse_3) %>% as.numeric() %>% `/`(60)) # duration : dration of annular / total eclipses in terms of minutes

## Finding the min, max values of starting time of annular/total eclipse
# 2023 annular eclipse - result : min 58555, max 61110
eclipse_data %>% filter(year == "2023") %>% `$`(eclipse_3) %>% as.vector() %>% summary
# 2024 total eclipse - result : min 66450, max 70440
eclipse_data %>% filter(year == "2024") %>% `$`(eclipse_3) %>% as.vector() %>% summary


### 3. Sub-plot ----------
## Font Setting
font_add_google("Alumni Sans", "almni") # For Title and Axes
font_add_google("Teko", "teko") # For Subtitle
showtext_auto()

## A map with 2023 annular eclipse beginning time for each location. 
plot_annular_eclipse_2023 <- ggplot() +
  geom_polygon(data = map_data("state"),
               mapping = aes(x = long, y = lat, group = group),
               fill = "ivory1", color = "black") + 
  geom_point(data = eclipse_data %>% filter(year == "2024"), 
             mapping = aes(x = lon, y = lat), color = "grey50", alpha = 0.4, size = 0.5) +
  geom_point(data = eclipse_data %>% filter(year == "2023"),
             mapping = aes(x = lon, y = lat, color = eclipse_3), size = 0.5) +
  scale_color_gradientn(name = "Time", colors = hcl.colors(n = 10, palette = "Berlin"),
                        breaks = seq(from = 58555, to = 61110, by = (61110-58555)/4) %>% round() %>% as_hms()) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  labs(
    title = "Time at which Annular Solar Eclipse Begins in 2023",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.box = "vertical",
    legend.key.height = unit(0.65, "cm"),
    legend.text = element_text(size = 11),
    legend.position = c(1, 0.5),
    title = element_text(size = 16, family = "teko"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(vjust = -2.5),
    axis.title.y = element_text(vjust = 2.5),
    plot.margin = margin(l = 10, r = 20, b = 30)
  ) 

## A map with 2024 total eclipse beginning time for each location. 
plot_total_eclipse_2024 <- ggplot() +
    geom_polygon(data = map_data("state"),
                 mapping = aes(x = long, y = lat, group = group),
                 fill = "ivory1", color = "black") + 
    geom_point(data = eclipse_data %>% filter(year == "2023"), 
               mapping = aes(x = lon, y = lat), color = "grey50", alpha = 0.4, size = 0.5) +
    geom_point(data = eclipse_data %>% filter(year == "2024"),
               mapping = aes(x = lon, y = lat, color = eclipse_3), size = 0.5) +
    scale_color_gradientn(name = "Time", colors = hcl.colors(n = 10, palette = "Berlin"),
                          breaks = seq(from = 66450, to = 70440, by = (70400-66450)/4) %>% round() %>% as_hms()) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme_minimal() +
    labs(
      title = "Time at which Total Solar Eclipse Begins in 2024",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme(
      legend.box = "vertical",
      legend.key.height = unit(0.65, "cm"),
      legend.text = element_text(size = 11),
      legend.position = c(1, 0.5),
      title = element_text(size = 16, family = "teko"),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(vjust = -2.5),
      axis.title.y = element_text(vjust = 2.5),
      plot.margin = margin(l = 10, r = 20, b = 30)
    ) 

## Beeswarm plot & boxplot for annular / total eclipse duration 
plot_bee <- ggplot(data = eclipse_data, mapping = aes(x = year, y = duration)) +
    geom_boxplot(width = 0.3, mapping = aes(fill = year), alpha = 0.5) +
    geom_quasirandom(method = "pseudorandom", size = 0.1, alpha = 0.8) +
    theme_minimal() +
    scale_y_continuous(labels = function(x) {paste0(x, " min")}) + 
    labs(
      title = "How Long Did the Annular/Total Eclipses Last?"
    ) +
    theme(
      axis.title = element_blank(),
      legend.position = "none",
      title = element_text(size = 16, family = "teko", vjust = 10),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 14, face = "bold"),
      plot.margin = margin(l = 40, t = 10, b = 30)
    )


### 3. Final Plot ----------
((plot_annular_eclipse_2023 / plot_total_eclipse_2024) | plot_bee) +
  plot_annotation(title = "2023 & 2024 Solar Eclipses in the United States",
                  subtitle = paste0("The united States saw an annular solar eclipse in 2023, and a total solar eclipse in 2024.", "\n",
                                    "Left charts illustrate the time at which annulity / totality took place in each location in the United States.", "\n",
                                    "Right chart shows the distribution of times for which the annulity / totality lasted, with each dot representing a location.", "\n"),
                  theme = theme(plot.title = element_text(size = 40, family = "teko"),
                                plot.subtitle = element_text(size = 14, family = "almini", vjust = -2),
                                plot.margin = margin(t = 10, l = 10, r = 10),
                                panel.background = element_rect(fill = "azure"),
                                plot.background = element_rect(fill = "azure")
                                )
                  )

### 4. Session Info ----------
session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  Korean_Korea.utf8
# ctype    Korean_Korea.utf8
# tz       Asia/Seoul
# date     2024-04-14
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA

# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.3.2   2023-10-31 [?] local
# beeswarm       0.4.0   2021-06-01 [1] CRAN (R 4.3.1)
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.2)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.2)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.2)
# cli            3.6.2   2023-12-11 [1] CRAN (R 4.3.2)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.2)
# P compiler       4.3.2   2023-10-31 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.2)
# curl           5.2.0   2023-12-08 [1] CRAN (R 4.3.2)
# P datasets     * 4.3.2   2023-10-31 [2] local
# dplyr        * 1.1.4   2023-11-17 [1] CRAN (R 4.3.2)
# fansi          1.0.6   2023-12-08 [1] CRAN (R 4.3.2)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.2)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.2)
# fs             1.6.3   2023-07-20 [1] CRAN (R 4.3.2)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.2)
# ggbeeswarm   * 0.7.2   2023-04-29 [1] CRAN (R 4.3.3)
# ggplot2      * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.2   2023-10-31 [2] local
# P grDevices    * 4.3.2   2023-10-31 [2] local
# P grid           4.3.2   2023-10-31 [2] local
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# hms          * 1.1.3   2023-03-21 [1] CRAN (R 4.3.2)
# httr           1.4.7   2023-08-15 [1] CRAN (R 4.3.2)
# jsonlite       1.8.8   2023-12-04 [1] CRAN (R 4.3.2)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.2)
# mapproj        1.2.11  2023-01-12 [1] CRAN (R 4.3.3)
# maps         * 3.4.2   2023-12-15 [1] CRAN (R 4.3.3)
# P methods      * 4.3.2   2023-10-31 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.2)
# P parallel       4.3.2   2023-10-31 [2] local
# patchwork    * 1.2.0   2024-01-08 [1] CRAN (R 4.3.3)
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.2)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.2)
# purrr        * 1.0.2   2023-08-10 [1] CRAN (R 4.3.2)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.2)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# rlang          1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0  2023-07-07 [1] CRAN (R 4.3.2)
# rvest          1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
# scales         1.3.0   2023-11-28 [1] CRAN (R 4.3.3)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.2)
# sessioninfo  * 1.2.2   2021-12-06 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7   2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.3)
# P stats        * 4.3.2   2023-10-31 [2] local
# stringi        1.8.3   2023-12-11 [1] CRAN (R 4.3.2)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.3.2)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.3.3)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.3.2)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# tidyselect     1.2.0   2022-10-10 [1] CRAN (R 4.3.2)
# tidytuesdayR * 1.0.3   2023-12-13 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.2)
# timechange     0.3.0   2024-01-18 [1] CRAN (R 4.3.2)
# P tools          4.3.2   2023-10-31 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.2)
# usethis        2.2.3   2024-02-19 [1] CRAN (R 4.3.3)
# utf8           1.2.4   2023-10-22 [1] CRAN (R 4.3.2)
# P utils        * 4.3.2   2023-10-31 [2] local
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.3.2)
# vipor          0.4.7   2023-12-18 [1] CRAN (R 4.3.3)
# vroom          1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# withr          3.0.0   2024-01-16 [1] CRAN (R 4.3.2)
# xml2           1.3.6   2023-12-04 [1] CRAN (R 4.3.2)

# [1] C:/Users/GIHUN/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.2/library

# P ── Loaded and on-disk path mismatch.


