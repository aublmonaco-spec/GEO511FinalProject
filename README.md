---
title: "Phenological Shifts in the Ruby-Throated Hummingbird and the Cardinal Flower"
author: Aubrey Monaco
subtitle: "GEO511 Final Project"
date: today
date-format: long
theme: morph
---

## Introduction

With warming temperatures due to anthropogenic climate change, bird migration times are shifting, and they are tending to migrate back to their breeding grounds earlier each year. Conversely, flowers that certain migrating birds, like ruby-throated hummingbirds, may feed on may not be changing the time of their first bloom. Therefore, when birds that rely on flowers as food sources migrate back earlier, they may not have enough food to support them when they arrive. I want to investigate how a pollinating bird species, the Ruby-throated Hummingbird (Archilochus colubris), has been shifting its migration patterns and how the appearances of a species it prefers to feed on, the cardinal flower (Lobelia cardinalis), has changed (or not changed) over the past 15 years (\~80 more words needed).

## Materials and Methods

\[\~ 200 words\]

Narrative: Clear narrative description of the data sources and methods. Includes data from at least two sources that were integrated / merged in R.

Code: The code associated with the project is well organized and easy to follow. Demonstrates mastery of R graphics and functions.

Data: The underlying data are publicly accessible via the web and downloaded/accessed within the Rmd script. If you want to use your own data, you must make it available on a website (e.g. Figshare) so that others are able to re-run your code.

You can do bullets like this:

-   The first most important thing
-   The second most important thing
-   The third most important thing

You can do numbers like this:

1.  The first most important thing
2.  The second most important thing
3.  The third most important thing

See <http://rmarkdown.rstudio.com/> for all the amazing things you can do.

Here's my first code chunk.

```{r}
#install.packages("leaflet")
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(spData)
library(lubridate)
library(viridis)
library(RColorBrewer)
library(tidyr)
getwd()
cardinal_flower <- read.csv("cardinal_flower_nat.csv")

world <- world

us <- world[world$name_long == "United States", ]

cardinal_flower <- cardinal_flower %>%
  mutate(observed_on = parse_date_time(observed_on, orders = c("mdy", "ydm", "dmy"))) %>%
  mutate(year = year(observed_on)) %>%
  mutate(year_day = yday(observed_on))

cf2010 <- cardinal_flower %>%
  filter(year == 2010) %>%
  mutate(cfmean2010 = mean(year_day, na.rm = TRUE)) 

mean_card_fl <- full_join(cardinal_flower, cf2010)

mean_card_fl <- mean_card_fl %>%
  mutate(cfmean2010 = ifelse(is.na(cfmean2010), 178.3846, cfmean2010))

cfmean2011 <- mean_card_fl %>%
  filter(year == 2011) %>%
  mutate(cfmean2011 = mean(year_day, na.rm = TRUE))

cfmean2012 <- mean_card_fl %>%
  filter(year == 2012) %>%
  mutate(cfmean2012 = mean(year_day, na.rm = TRUE))

cfmean2013 <- mean_card_fl %>%
  filter(year == 2013) %>%
  mutate(cfmean2013 = mean(year_day, na.rm = TRUE))

cfmean2014 <- mean_card_fl %>%
  filter(year == 2014) %>%
  mutate(cfmean2014 = mean(year_day, na.rm = TRUE))

cfmean2015 <- mean_card_fl %>%
  filter(year == 2015) %>%
  mutate(cfmean2015 = mean(year_day, na.rm = TRUE))

cfmean2016 <- mean_card_fl %>%
  filter(year == 2016) %>%
  mutate(cfmean2016 = mean(year_day, na.rm = TRUE))

cfmean2017 <- mean_card_fl %>%
  filter(year == 2017) %>%
  mutate(cfmean2017 = mean(year_day, na.rm = TRUE))

cfmean2018 <- mean_card_fl %>%
  filter(year == 2018) %>%
  mutate(cfmean2018 = mean(year_day, na.rm = TRUE))

cfmean2019 <- mean_card_fl %>%
  filter(year == 2019) %>%
  mutate(cfmean2019 = mean(year_day, na.rm = TRUE))

cfmean2020 <- mean_card_fl %>%
  filter(year == 2020) %>%
  mutate(cfmean2020 = mean(year_day, na.rm = TRUE))

cfmean2021 <- mean_card_fl %>%
  filter(year == 2021) %>%
  mutate(cfmean2021 = mean(year_day, na.rm = TRUE))

cfmean2022 <- mean_card_fl %>%
  filter(year == 2022) %>%
  mutate(cfmean2022 = mean(year_day, na.rm = TRUE))

cfmean2023 <- mean_card_fl %>%
  filter(year == 2023) %>%
  mutate(cfmean2023 = mean(year_day, na.rm = TRUE))

cfmean2024 <- mean_card_fl %>%
  filter(year == 2024) %>%
  mutate(cfmean2024 = mean(year_day, na.rm = TRUE))

cfmean2025 <- mean_card_fl %>%
  filter(year == 2025) %>%
  mutate(cfmean2025 = mean(year_day, na.rm = TRUE))

mean_card_fl <- mean_card_fl %>%
  mutate(cfmean = case_when(
    year == 2011 ~ "193.1176",
    year == 2012 ~ "186.4643",
    year == 2013 ~ "166.1282",
    year == 2014 ~ "162.5152",
    year == 2015 ~ "198.9559",
    year == 2016 ~ "171.7606",
    year == 2017 ~ "168.3232",
    year == 2018 ~ "165.468",
    year == 2019 ~ "175.7049",
    year == 2020 ~ "175.8336",
    year == 2021 ~ "176.8913",
    year == 2022 ~ "166.3364",
    year == 2023 ~ "163.6606",
    year == 2024 ~ "178.0036",
    year == 2025 ~ "167.9547"
  )) %>%
  mutate(cfmean = as.numeric(cfmean)) %>%
  mutate(difference = cfmean - cfmean2010)

rth_1995_2021 <- read.csv("RTH_1995_2021.csv")
rth_2022_2025 <- read.csv("RTH_2022_2025.csv")

rth_all <- rth_1995_2021 %>% full_join(rth_2022_2025)

ruby_throat <- rth_all %>%
  mutate(Date = parse_date_time(Date, orders = c("mdy", "ydm", "dmy"))) %>%
  mutate(year = year(Date)) %>%
  mutate(year_day = yday(Date))

rth2010 <- ruby_throat %>%
  filter(Year == 2010) %>%
  mutate(year_day = yday(Date))

rth2010 <- rth2010 %>%
  mutate(rthmean2010 = mean(year_day, na.rm = TRUE)) 

mean_ruby_throat <- full_join(ruby_throat, rth2010) 

mean_ruby_throat <- mean_ruby_throat %>%
  mutate(rthmean2010 = ifelse(is.na(rthmean2010), 200.4848, rthmean2010)) %>%
  mutate(difference = year_day - rthmean2010)

ggplot() + geom_sf(data = us) + geom_point(data = mean_card_fl, aes(x = longitude, y = latitude, color = difference))

ggplot() + geom_sf(data = us) + geom_point(data = mean_ruby_throat, aes(x = Longitude, y = Latitude, color = difference)) 

cpal <- colorNumeric(palette = "mako", domain = mean_card_fl$difference, na.color = NA, reverse = TRUE)

rpal <- colorNumeric(palette = "inferno", domain = mean_ruby_throat$difference, na.color = NA)

leaflet(mean_card_fl) %>%
  addProviderTiles(provider = "Esri") %>%
  addPolylines(~longitude, ~ latitude, color = ~cpal(difference)) %>%
  addLegend(pal = cpal, values = ~difference)

leaflet(mean_ruby_throat) %>%
  addProviderTiles(provider = "Esri") %>%
  addCircleMarkers(~Longitude, ~Latitude, radius = 1, color = ~rpal(difference)) %>%
  addLegend(pal = rpal, values = ~difference)
```

Load any required packages in a code chunk (you may need to install some packages):

## Download and clean all required data

Add any additional processing steps here.

## Results

\[\~200 words\]

Tables and figures (maps and other graphics) are carefully planned to convey the results of your analysis. Intense exploration and evidence of many trials and failures. The author looked at the data in many different ways before coming to the final presentation of the data.

Show tables, plots, etc. and describe them.r

### Dygraphs Example

## Conclusions

\[\~200 words\]

Clear summary adequately describing the results and putting them in context. Discussion of further questions and ways to continue investigation.

## References

All sources are cited in a consistent manner







# Template Repository for Quarto Website

This repository serves as a template for building a quarto website that is automatically rendered using GitHub Actions.

## .gitignore

Add any files here that you do not want to commit to your repository. The default `.gitignore` file is set up to ignore files that are commonly not committed to repositories.

### \_quarto.yml

This file contains the configuration for your quarto website. You can modify this file to change the title, author, and other settings for your website.

### index.qmd

This is the main page of your website. You can modify this file to change the content of your homepage.

### .github/workflows/quarto-render.yml

This file contains the GitHub Actions workflow that automatically renders your quarto website whenever you push changes to the repository. You can modify this file to change the settings for the workflow.

### \_site

This folder contains the rendered HTML files for your website. You do not need to modify anything in this folder, as it is automatically generated by GitHub Actions.

### Additional Pages

You can add additional pages to your website by creating new `.qmd` files in the root directory of the repository. For example, you could create a `about.qmd` file for an "About" page or a `contact.qmd` file for a "Contact" page.

### Customizing Your Website

You can customize the appearance of your website by modifying the `_quarto.yml` file and adding custom CSS stylesheets. Refer to the [Quarto documentation](https://quarto.org/docs/websites/) for more information on customizing your quarto website.

### Getting Started

To get started with this template, simply click the "Use this template" button at the top of the repository page to create a new repository based on this template. Then, clone your new repository to your local machine and start modifying the files to create your own quarto website.
