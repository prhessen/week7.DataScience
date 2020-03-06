# R Studio API Code 
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(ggplot2)
library(tidyverse)
library(GGally)
library(lubridate)
# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
    mutate(timeStart = ymd_hms(timeStart)) %>%
    mutate(gender = factor(gender, levels = c("F", "M"), labels = c("Female", "Male"))) %>%
    mutate(condition = factor(condition, levels = c("A", "B", "C"), labels = c("Block A", "Block B", "Block C"))) %>%
    filter(q6 == 1) %>%
    select(-q6)
    
    
# Visualization
ggpairs(week7_tbl[,5:13])
ggplot(week7_tbl, aes(x = timeStart, y = q1)) +
    geom_point() +
    labs(x = "Date of Experiment", y = "Q1 Score")

ggplot(week7_tbl, aes(x = q1, y = q2, col = gender)) +
    geom_jitter() 
ggplot(week7_tbl, aes(x = q1, y = q2)) +
    geom_jitter() +
    facet_grid(. ~ gender) +
    labs(x = "Score on Q1", y = "Score on Q2")

ggplot(week7_tbl, aes(x = gender, y = ((timeEnd - timeStart) * 60))) +
    geom_boxplot() + 
    labs(x = "Gender", y = "Time Elapsed (secs)")

ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) +
    geom_jitter(width = 0.07) +
    labs(x = "Score on Q5", y = "Score on Q7", col = "Experimental Condition") +
    geom_smooth(method = "lm", se = F) +
    theme(legend.position = "bottom", panel.background = element_blank(), legend.background = element_rect(fill = "#E0E0E0"))
