###### Understanding Membership Invites
library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(readr)
library(dplyr)
library("readxl")
library("RColorBrewer")
library(rsconnect)
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(readr)
library(readxl)
library(RColorBrewer)
library(stats)
library(utils)
library(sf)
library(writexl)
library(base64enc)
library(phsopendata)
library(plotly)
library(crosstalk)
library(lubridate)
library(kableExtra)
library(tidyverse)




main_data <- read.csv("data work pd internal audit.csv")

invite_list <- main_data %>% 
  filter(Type == "Invite")

uninvite_list <- main_data %>% 
  filter(Type == "Uninvite")

invite_list$date_cleaned <- substr(invite_list$Date, start = 1, stop = 10) 


invite_list$date_cleaned_proper <- as.Date(invite_list$date_cleaned, format = "%d.%m.%Y")

invite_list_cleaned <- invite_list %>%
group_by(date_cleaned_proper) %>%
  summarise(count=n(), .groups = 'drop')


bar_theme <- theme(axis.title = element_text(colour="#06402b", family = "sans"),
                            axis.text.x = element_text(size=8, colour = "black"),
                            axis.text.y = element_text(size=8, colour = "black"),
                            axis.title.x = element_text(size=14),
                            axis.title.y = element_text(size=14),
                            panel.background = element_blank(),
                            panel.grid.major.x = element_line(colour = "grey"),
                            panel.grid.major.y = element_blank(),
                            axis.line.x = element_line(colour="black"),
                            axis.line.y = element_line(colour="black"),
                            plot.margin = margin(1,1,1,1, "cm"))

uninvite_output <- invite_list_cleaned %>%
  ggplot(aes(x = date_cleaned_proper, y = count)) +
  geom_bar(stat = "identity")+
  bar_theme

