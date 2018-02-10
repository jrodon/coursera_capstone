# global.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny script for loading data into global environment
# Javier A. Rodón
# 10.02.2018

# Libraries ####
library(dplyr)
library(quanteda)
library(data.table)
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)

# Functions used in the processing ####
source("./funs_shiny.R")

# Define the js method that resets the page ####
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Load data ####
ngramsCount <- readRDS(file = "./counts.Rds")
