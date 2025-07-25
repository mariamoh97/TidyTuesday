---
title: "#TidyTuesday"
subtitle: "Week 5"
author: "Maria Mohyuddin"
css: custom_report.css
output: 
  bookdown::html_document2:
    toc: true
    toc_float: true
    toc_depth: 1
    code_folding: show
    theme: default
    highlight: tango
    fig_captions: no
    number_sections: false
---

<!-- This is a comment in markdown. 

The top section is the YAML metadata. 

Below are code chunks interspersed with markdown prose.

-->


```{r setup, include=FALSE, warning = FALSE, message = FALSE}
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
  options(scipen=999) # turns off scientific notation
  
  install.packages("tidytuesdayR")

# load packages
  library(tidyverse)
  library(tidytuesdayR)   # install.packages("tidytuesdayR") to get new data

# load data
tuesdata <- tidytuesdayR::tt_load('2024-02-13')

gifts_gender <- tuesdata$gifts_gender

  
# clean data (optional: tidy tuesday provides clean data)
  
```


# Exploration
```{r}
#Loading library

library("ggplot2")

# Data Wrangling 
data <- gifts_gender %>% select(-c("SpendingCelebrating"))%>%
  pivot_longer(!Gender) %>% rename(Category=name,
         Count = value)

#creating separate tibbles for a dumbbell plot

Men <- data %>%
  filter(Gender == "Men")
Women <- data %>%
  filter(Gender == "Women")


#Took help from github/stack overflow to make a geom that uses hearts instead of points

geom_heart <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
  ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, 
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", 
        call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
    params = list(parse = parse, check_overlap = check_overlap, 
      na.rm = na.rm, label = sprintf('\u2665'), ...))
}


# Creating a dumbbell plot

 p <-  ggplot(data) +
  geom_segment(data = Men,
              aes(x = Category, y = Count,
                  yend = Women$Count, xend = Women$Category),
              color = "lightpink",
              size = 4,
              alpha = .5) +
  
  geom_heart(aes(x = Category, y = Count, color = Gender), size = 6, show.legend = TRUE) + labs(y= "",x="") +

  ggtitle("Percentage of men and women who buy different categories of gifts on Valentines day")+ theme(plot.title = element_text(size = 10))
 
p + coord_flip()
  



```

