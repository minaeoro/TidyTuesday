library(tidytuesdayR)
library(showtext)
library(tidyverse)
library(scales)
library(sessioninfo)

### Loading the data ----------
tuesdata <- tidytuesdayR::tt_load('2024-04-02')
dubois_week10 <- tuesdata$dubois_week10

## Color_palette inspired by w.E.B Du Bois,
## source : https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf
dubois_palette <- c(blalck ='#000000', 
                    brown = '#654321', 
                    tan = '#d2b48c', 
                    gold = '#ffd700',
                    pink = '#ffc0cb', 
                    crimson = '#dc143c', 
                    green = '#00aa00', 
                    blue = '#4682b4',
                    purple = '#7e6583',
                    gray = '#888888',
                    paper = '#FAF0E6')

## Font : Couldn't find exact 'Du Bois' font family, so I selected similar font.
font_add_google("Chakra Petch", "chakra")
showtext_auto()


### Plotting ----------
## Du Bois's original chart :
## https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/2024/challenge10/original-plate-37.jpg
dubois_week10 %>% 
  mutate(Occupation = factor(Occupation, levels = rev(Occupation))) %>%
    ggplot(mapping = aes(x = Occupation, y = Percentage, fill = Occupation)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    geom_text(aes(label = percent(Percentage, scale=1)), hjust = -0.5, family = "chakra", size = 5) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_manual(values = c("Teachers" = dubois_palette[["crimson"]],
                                 "Ministers" = dubois_palette[["blue"]],
                                 "Government Service" = dubois_palette[["pink"]],
                                 "Business" = dubois_palette[["tan"]],
                                 "Other Professions" = dubois_palette[["gray"]],
                                 "House Wives" = dubois_palette[["gold"]])) +
    theme_minimal() +
    ylim(c(0,75)) +
    labs( title = "GRADUATES OF ATLANTA UNIVERSITY",
          subtitle = paste0("IN THE 1900 PARIS EXPOSITION, W.E.B. DU BOIS INTRODUCED A SERIES OF CHARTS,", "\n", 
                            "SHEDDING LIGHTS ON BLACK LIVES IN THE UNITED STATES AFTER EMANCIPATION.","\n\n",
                            "THE CHARTS WERE PREPARED AND EXECUTED BY BLACK STUDENTS,","\n",
                            "UNDER THE DIRECTION OF ATLANTA UNIVERSITY.","\n\n",
                            "IT HAD GRADUATED 330 BLACK STUDENTS, AMONG WHOM WERE")
          ) +
    geom_text(label = paste0("ATLANTA UNIVERSITY WAS FOUNDED IN 1867.","\n",
                             "BY THE TIME DU BOIS ISSUED THIS CHART,","\n",
                             "IT HAD INSTRUCTED 6,000 BLACK STUDENTS."),
                      x = 4, y = 40, family = "chakra", size = 5, colour = dubois_palette[["purple"]]) +
    theme(
          legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(family = "chakra", size = 15, hjust = 0.05, vjust = -0.5),
          axis.text.y = element_text(family = "chakra", size = 15, hjust = 1),
          axis.text.x = element_text(family = "chakra", size = 15, vjust = 2),
          plot.title = element_text(family = "chakra", face = "bold", size = 28, vjust = 0.5, margin = margin(t = 0, r = 0, b = 10, l = -20)),
          plot.margin = margin(t = 20, r = 20, b = 40, l = 20),
          plot.subtitle = element_text(family = "chakra", size = 15, vjust = -0.5, margin = margin(t = 0, r = 0, b = 30, l = -20)),
          plot.background = element_rect(fill = dubois_palette[["paper"]])
          )
        
### Session Info ----------
session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 11 x64 (build 22621)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  Korean_Korea.utf8
# ctype    Korean_Korea.utf8
# tz       Asia/Seoul
# date     2024-04-07
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA

# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.2    2023-10-31 [?] local
# bit            4.0.5    2022-11-15 [1] CRAN (R 4.3.2)
# bit64          4.0.5    2020-08-30 [1] CRAN (R 4.3.2)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.2)
# cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.2)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.2)
# P compiler       4.3.2    2023-10-31 [2] local
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.2)
# curl           5.2.0    2023-12-08 [1] CRAN (R 4.3.2)
# P datasets     * 4.3.2    2023-10-31 [2] local
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.3.2)
# fansi          1.0.6    2023-12-08 [1] CRAN (R 4.3.2)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.2)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.2)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.2)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.2)
# gganimate      1.0.9    2024-02-27 [1] CRAN (R 4.3.3)
# ggplot2      * 3.5.0    2024-02-23 [1] CRAN (R 4.3.2)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.3)
# glue           1.7.0    2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.2    2023-10-31 [2] local
# P grDevices    * 4.3.2    2023-10-31 [2] local
# P grid           4.3.2    2023-10-31 [2] local
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.2)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.2)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.2)
# jsonlite       1.8.8    2023-12-04 [1] CRAN (R 4.3.2)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3    2023-09-27 [1] CRAN (R 4.3.2)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.2)
# P methods      * 4.3.2    2023-10-31 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.2)
# P parallel       4.3.2    2023-10-31 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.2)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.2)
# prettyunits    1.2.0    2023-09-24 [1] CRAN (R 4.3.2)
# progress       1.2.3    2023-12-06 [1] CRAN (R 4.3.2)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.2)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.2)
# readr        * 2.1.5    2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.2)
# rlang          1.1.3    2024-01-10 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.2)
# rvest          1.0.4    2024-02-12 [1] CRAN (R 4.3.2)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.3.3)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.2)
# sessioninfo  * 1.2.2    2021-12-06 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# P stats        * 4.3.2    2023-10-31 [2] local
# stringi        1.8.3    2023-12-11 [1] CRAN (R 4.3.2)
# stringr      * 1.5.1    2023-11-14 [1] CRAN (R 4.3.2)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.2)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.3.2)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.2)
# tidytuesdayR * 1.0.3    2023-12-13 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.2)
# timechange     0.3.0    2024-01-18 [1] CRAN (R 4.3.2)
# P tools          4.3.2    2023-10-31 [2] local
# tweenr         2.0.3    2024-02-26 [1] CRAN (R 4.3.3)
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.2)
# usethis        2.2.3    2024-02-19 [1] CRAN (R 4.3.3)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# P utils        * 4.3.2    2023-10-31 [2] local
# vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# vroom          1.6.5    2023-12-05 [1] CRAN (R 4.3.2)
# withr          3.0.0    2024-01-16 [1] CRAN (R 4.3.2)
# xml2           1.3.6    2023-12-04 [1] CRAN (R 4.3.2)

# [1] C:/Users/GIHUN/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.2/library

# P ── Loaded and on-disk path mismatch.
