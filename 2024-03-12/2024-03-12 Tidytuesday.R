library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(showtext)
library(ggupset)
library(arules)
library(ggrepel)
library(patchwork)
library(sessioninfo)


### Loading the data ----------
tuesdata <- tidytuesdayR::tt_load('2024-03-12')
fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')


### Data Exploration ----------
## Step 1 : tokenization and removing stop words for "project_types."

# "eligibility_criteria" column is merged with "project_types" column.
# It turns out that some of "eligibility_criteria" contents have information that should have been in "project_types" column.
# By merging this column with "project_types" column, we will have more comprehensive ideas of project types.
fiscal_sponsor_directory <- fiscal_sponsor_directory %>%
                              mutate(project_types = paste(project_types," ", eligibility_criteria))

project_words <- fiscal_sponsor_directory %>% 
                  group_by(name) %>%
                    unnest_tokens(project_keywords, project_types, token = "regex", pattern = "[|/]") %>%
                      anti_join(stop_words, by = c("project_keywords" = "word")) %>%
                        ungroup()


## Step 2 : Sorting and seeing the key words in "project_types" to make classification
project_words %>% count(project_keywords, sort = TRUE) %>% view


## Step 3 : Making proper categories for project types
project_regex_list <- list()
project_regex_list[["Art and Culture"]] <- regex("(art|culture|cultural|festival|event)", ignore_case = TRUE)
project_regex_list[["Media"]] <- regex("(media|documentar(y|ies)|film|movie)", ignore_case = TRUE) 
project_regex_list[["Education"]] <- regex("(education|training|technology)", ignore_case = TRUE)
project_regex_list[["Youth"]] <- regex("(children|youth|childhood)", ignore_case = TRUE)
project_regex_list[["Family"]] <- regex("(famil(y|ies))", ignore_case = TRUE)
project_regex_list[["Gender"]] <- regex("(lgbt|wom[a|e]n)", ignore_case = TRUE)
project_regex_list[["Race"]] <- regex("(race|of color|racial|latin)", ignore_case = TRUE)
project_regex_list[["Environment"]] <- regex("(environment|sustainable)", ignore_case = TRUE)
project_regex_list[["Health"]] <- regex("(health|nutrition|drug|disease|clinic)", ignore_case = TRUE)
project_regex_list[["Religious"]] <- regex("(faith|religio(us|n))", ignore_case = TRUE)
project_regex_list[["Economic Development"]] <- regex("(economic)", ignore_case = TRUE)
project_regex_list[["Relief"]] <- regex("(disaster|relief|food|convicted)", ignore_case = TRUE)

# realizing the categorization on the column "project_type."
project_types_column <- fiscal_sponsor_directory %>% 
                          select(project_types) %>% deframe %>%
                            map( function(x) {
                              
                                        category <- c()
                                      
                                        for(name in names(project_regex_list)) {
                                          if(is.na(x) == FALSE & str_detect(x, eval(expr(`$`(project_regex_list, !!name))))) {
                                            category <- c(category, name)
                                          }}
                                        return(category)
                                      } 
                                      ) %>% tibble() %>% rename("project_types" = 1)

## Step 4 : realizing the categorization on the column "fiscal_sponsorship_model"
# Some sponsporship model information was recorded in the "services" column, with "fsical_sponsorship_model" left blank.
# following script fixes such an issue.
model_column <- fiscal_sponsor_directory %>% 
                  select(services, fiscal_sponsorship_model) %>%
                    mutate(fiscal_sponsorship_model = if_else(is.na(fiscal_sponsorship_model), services, fiscal_sponsorship_model)) %>%
                      select(fiscal_sponsorship_model) %>%
                        mutate(fiscal_sponsorship_model = str_extract_all(fiscal_sponsorship_model, "Model [A-FL]"))

## Step 5 : Geeting tibbles with (1) project type and (2) fiscal sponsorship model classification
# project_types_column and model_column have list as componenet
fiscal_sponsor_list_format <- fiscal_sponsor_directory %>% 
                                select(name, year_501c3, year_fiscal_sponsor, n_sponsored) %>%
                                  bind_cols(project_types_column, model_column)

# project_types_column and model_column reshaped into long form
fiscal_sponsor_long_format <- fiscal_sponsor_list_format %>%
                                unnest(cols = project_types) %>%
                                  unnest(fiscal_sponsorship_model)


### Plotting ----------
## Font Setting
font_add_google("Neuton", "newton")
font_add_google("Arvo", "arvo")
showtext_auto()

### UpSet plots
## Plot 1 : UpSet Plot for Sponsorship Model
p_UpSet_sponsorship_model <- fiscal_sponsor_list_format %>%
                                distinct(name, fiscal_sponsorship_model, .keep_all = TRUE) %>%
                                  filter(fiscal_sponsorship_model %>% is.na == FALSE) %>%
                                    ggplot(mapping = aes(x = fiscal_sponsorship_model)) +
                                      geom_bar() +
                                      scale_x_upset(n_intersections = 15) +
                                      theme_minimal() +
                                      labs(title = "Frequncies of Fiscal Model Combinations of Each Sponsor",
                                           x = "Fiscal Sponsor Model Combinations",
                                           y = "Count") +
                                      theme(legend.position = "none",
                                            plot.title = element_text(size=13, family = "arvo"),
                                            axis.title.x = element_text(size=11, family = "arvo"),
                                            axis.title.y = element_text(size=11, family = "arvo"),
                                            axis.text = element_text(size= 11),
                                            plot.margin = margin(10, 10, 10, 60))

## Plot 2 : UpSet Plot for Project Types
p_UpSet_project_model <- fiscal_sponsor_list_format %>%
                          distinct(name, project_types, .keep_all = TRUE) %>%
                            filter(project_types %>% is.na == FALSE) %>%
                              ggplot(mapping = aes(x = project_types)) +
                                geom_bar() +
                                scale_x_upset(n_intersections = 15) +
                                theme_minimal() +
                                labs(title = "Frequncies of Project Combinations of Each Sponsor",
                                     x = "Project Combinations",
                                     y = "Count") +
                                theme(legend.position = "none",
                                      plot.title = element_text(size=13, family = "arvo"),
                                      axis.title.x = element_text(size=11, family = "arvo"),
                                      axis.title.y = element_text(size=11, family = "arvo"),
                                      axis.text = element_text(size= 11),
                                      plot.margin = margin(10, 10, 10, 60))



## Plot 3 : Fiscal Sponsorship Model and Number of Sponsored Organization

p_violin_sponsorship_model <-  fiscal_sponsor_long_format %>%
                                  distinct(name, fiscal_sponsorship_model, .keep_all = TRUE) %>%
                                    filter(fiscal_sponsorship_model %>% is.na == FALSE) %>%
                                      ggplot(mapping = aes(x = fiscal_sponsorship_model, y = n_sponsored)) +
                                      geom_violin(mapping = aes(fill = fiscal_sponsorship_model,
                                                                color = fiscal_sponsorship_model)) + 
                                      geom_jitter(width = 0.05, size = 1, alpha = 0.5) + 
                                      geom_text_repel(data = fiscal_sponsor_long_format %>% filter(n_sponsored >= 600) %>% distinct(name, .keep_all = TRUE), 
                                                      mapping = aes(label = paste(name,"\n","Project Area : ", project_types,", etc")),
                                                      size = 3, color = "tomato", fontface = "bold") +
                                      labs(x = "Fiscal Sponsorship Model",
                                           y = "The Number of Organization\nSponsored by a Fiscal Sponsor",
                                           title = "Fiscal Sponsors and Their Choices in Sponsorship Models") +
                                      theme_minimal() +
                                      theme(legend.position = "none",
                                            plot.title = element_text(size=15, family = "arvo"),
                                            axis.title.x = element_text(size=12, family = "arvo"),
                                            axis.title.y = element_text(size=12, family = "arvo"),
                                            axis.text = element_text(size= 11),
                                            axis.text.x = element_text(vjust =8),
                                            axis.text.y = element_text(hjust =1.5)) +
                                      annotate(geom = "text", x = 4, y = 600,
                                               label = paste("Each dot indicates the number of sponsored organizations\n",
                                                             "by a fiscal sponsor that adopts certain fiscal sponsorship\n",
                                                             "model.\n\n",
                                                             "A fiscal sponsor may adopt multiple models, which menas\n",
                                                             "a fiscal sponsor may appear in the multiple model catogories"),
                                               size = 5, hjust = 0, family = "newton")


## Plot 4 : Association Rule 
# Association Rule analysis

rules <- apriori(fiscal_sponsor_list_format %>% select(project_types) %>% deframe() %>% transactions(), 
                        parameter = list(supp = 0.4, confidence = 0.9, target = "rules", maxlen = 3))

(inspect(rules, by = "lift"))

# Plotting
p_association <-  rules %>% 
                    DATAFRAME() %>% 
                      filter(lift >= 1.5) %>%
                        mutate(association = paste(LHS, "->", RHS)) %>% 
                        ggplot(aes(x = reorder(association, lift), y = lift)) + 
                          geom_segment(aes(x=reorder(association, lift), xend=reorder(association, lift), y=1.5, yend=lift), color="orange", size = 1.5) + 
                          geom_point( color="tomato", size = 4) + 
                          coord_flip() + 
                          theme_bw() + 
                          labs( title = "Association Rules on Project Types",
                                x = "Association Rules", 
                                y = "Lift", 
                                caption = "Rules were chosen such that (1) Support >= 0.4, (2) Confidence >= 0.90") +
                          theme(plot.caption = element_text(size=10, family = "arvo"),
                                plot.title = element_text(size=16, family = "arvo"),
                                axis.title.x = element_text(size=13, family = "arvo"),
                                axis.title.y = element_text(size=13, family = "arvo"))

## Final Plot : Plot Combination & Show
(p_UpSet <- p_UpSet_sponsorship_model + p_UpSet_project_model)
(p_violin_sponsorship_model)
(p_association)


 
### Session Info ----------
session_info(include_base = TRUE) 


# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 11 x64 (build 22621)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  Korean_Korea.utf8
# ctype    Korean_Korea.utf8
# tz       Asia/Seoul
# date     2024-03-24
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA

# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#! package      * version date (UTC) lib source
# arules       * 1.7-7   2023-11-29 [1] CRAN (R 4.3.3)
# base         * 4.3.2   2023-10-31 [?] local
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
# ggplot2      * 3.5.0   2024-02-23 [1] CRAN (R 4.3.2)
# ggrepel      * 0.9.5   2024-01-10 [1] CRAN (R 4.3.3)
# ggupset      * 0.3.0   2020-05-05 [1] CRAN (R 4.3.3)
# glue           1.7.0   2024-01-09 [1] CRAN (R 4.3.2)
# P graphics     * 4.3.2   2023-10-31 [2] local
# P grDevices    * 4.3.2   2023-10-31 [2] local
# P grid           4.3.2   2023-10-31 [2] local
# gtable         0.3.4   2023-08-21 [1] CRAN (R 4.3.2)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.2)
# httr           1.4.7   2023-08-15 [1] CRAN (R 4.3.2)
# janeaustenr    1.0.0   2022-08-26 [1] CRAN (R 4.3.3)
# jsonlite       1.8.8   2023-12-04 [1] CRAN (R 4.3.2)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.3.1)
# lattice        0.21-9  2023-10-01 [2] CRAN (R 4.3.2)
# lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3   2023-09-27 [1] CRAN (R 4.3.2)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.2)
# Matrix       * 1.6-1.1 2023-09-18 [2] CRAN (R 4.3.2)
# P methods      * 4.3.2   2023-10-31 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.2)
# P parallel       4.3.2   2023-10-31 [2] local
# patchwork    * 1.2.0   2024-01-08 [1] CRAN (R 4.3.3)
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.2)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.2)
# purrr        * 1.0.2   2023-08-10 [1] CRAN (R 4.3.2)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.2)
# Rcpp           1.0.12  2024-01-09 [1] CRAN (R 4.3.2)
# readr        * 2.1.5   2024-01-10 [1] CRAN (R 4.3.2)
# readxl         1.4.3   2023-07-06 [1] CRAN (R 4.3.2)
# rlang          1.1.3   2024-01-10 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0  2023-07-07 [1] CRAN (R 4.3.2)
# rvest          1.0.4   2024-02-12 [1] CRAN (R 4.3.2)
# scales         1.3.0   2023-11-28 [1] CRAN (R 4.3.2)
# selectr        0.4-2   2019-11-20 [1] CRAN (R 4.3.2)
# sessioninfo  * 1.2.2   2021-12-06 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7   2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.3)
# SnowballC      0.7.1   2023-04-25 [1] CRAN (R 4.3.1)
# P stats        * 4.3.2   2023-10-31 [2] local
# stringi        1.8.3   2023-12-11 [1] CRAN (R 4.3.2)
# stringr      * 1.5.1   2023-11-14 [1] CRAN (R 4.3.2)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.3.3)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.3.2)
# tidyr        * 1.3.1   2024-01-24 [1] CRAN (R 4.3.2)
# tidyselect     1.2.0   2022-10-10 [1] CRAN (R 4.3.2)
# tidytext     * 0.4.1   2023-01-07 [1] CRAN (R 4.3.3)
# tidytuesdayR * 1.0.3   2023-12-13 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.2)
# timechange     0.3.0   2024-01-18 [1] CRAN (R 4.3.2)
# tokenizers     0.3.0   2022-12-22 [1] CRAN (R 4.3.3)
# P tools          4.3.2   2023-10-31 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.2)
# usethis        2.2.3   2024-02-19 [1] CRAN (R 4.3.3)
# utf8           1.2.4   2023-10-22 [1] CRAN (R 4.3.2)
# P utils        * 4.3.2   2023-10-31 [2] local
# vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.3.2)
# vroom          1.6.5   2023-12-05 [1] CRAN (R 4.3.2)
# withr          3.0.0   2024-01-16 [1] CRAN (R 4.3.2)
# xml2           1.3.6   2023-12-04 [1] CRAN (R 4.3.2)

# [1] C:/Users/GIHUN/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.2/library

# P ── Loaded and on-disk path mismatch.

# ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────