# description -------------------------------------------------------------

# R script to process uploaded raw data into a tidy dataframe

# R packages --------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(tmap)
library(spData)
library(janitor)
library(devtools)
library(openwashdata)


# read data ---------------------------------------------------------------

## code to read raw dataset(s) goes here

DATASET <- DATATYPE_read(RAW_DATA_LOCATION)

# tidy data ---------------------------------------------------------------

## code to tidy dataset(s) goes here
## generates `DATASET` dataset

# explore data ------------------------------------------------------------

## code to explore `DATASET` dataset goes here
## loops with "tidy data" section

# save data ------------------------------------------------------------

## code to save `DATASET` dataset goes here

# create DATASET.rda in data directory
usethis::use_data(DATASET, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
# create export files for website
write_csv(DATASET, here::here("inst", "extdata", "DATASET.csv"))
openxlsx::write.xlsx(DATASET, here::here("inst", "extdata", "DATASET.xlsx"))

# prepare dictionaries legacy -----------------------------------------------

## will be replaced by use_dictionary_skeleton function from openwashdata pkg
get_variable_info <- function(data, directory = "", file_name = "") {
  total_variables <- sum(sapply(data, function(df) length(names(df))))

  variable_info <- tibble(
    directory = character(total_variables),
    file_name = character(total_variables),
    variable_name = character(total_variables),
    variable_type = character(total_variables),
    description = character(total_variables)
  )

  index <- 1

  for (i in seq_along(data)) {
    dataframe <- data[[i]]
    variable_names <- names(dataframe)
    variable_types <- sapply(dataframe, typeof)

    num_variables <- length(variable_names)
    variable_info$variable_name[index:(index + num_variables - 1)] <- variable_names
    variable_info$variable_type[index:(index + num_variables - 1)] <- variable_types
    variable_info$file_name[index:(index + num_variables - 1)] <- rep(file_name[i], num_variables)
    variable_info$directory[index:(index + num_variables - 1)] <- rep(directory[i], num_variables)

    index <- index + num_variables
  }

  return(variable_info)
}

# Specify values for directory and file_name
directories <- c("data/", "data/")
file_names <- c("DATASET1.rda", "DATASET2.rda")

dictionary <- get_variable_info(data = list(DATASET1, DATASET2),
                                directory = directories,
                                file_name = file_names)

# export files to fill in dictionary
dictionary |>
  write_csv("data-raw/dictionary.csv")

dictionary |>
  openxlsx::write.xlsx("data-raw/dictionary.xlsx")

# prepare dictionary ------------------------------------------------------

## create a skeleton files for the dictionary

# TODO after tidy data
# output: dictionary.csv file
use_dictionary_skeleton(data_location = NULL,
                        skeleton_dest = "data-raw/dictionary.csv",
                        data_file_pattern = ".rda",
                        ignore_pattern = "codebook.Rda",
                        recursive = TRUE)
# output: dictionary.csv file
openxlsx::write.xlsx(read_csv("data-raw/dictionary.csv"), "data-raw/dictionary.xlsx")

# update dictionary -------------------------------------------------------

# TODO
# Create and update dictionary
# function that conversts dictionary as xlsx to csv for later use in roxygen
update_dictionary <- function(dictionary_path) {
  dictionary_excel <-
    readxl::read_excel(dictionary_path)

  dictionary_excel |>
    readr::write_csv("data-raw/dictionary.csv")
}

# TODO
update_dictionary("data-raw/dictionary.xlsx")

