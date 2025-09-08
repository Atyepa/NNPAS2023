library(tidyverse)  
library(writexl)
library(highcharter)  
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readxl)

options(warn=-1)

`%!in%` <- negate(`%in%`)

#---Data directory:
setwd("C:/Users/atyeo/OneDrive/R data/NNPAS2023")

# Data cube path:
dcpath <- "https://www.abs.gov.au/statistics/health/food-and-nutrition/national-nutrition-and-physical-activity-survey/2023"

#---------------------
# Cleaning functions:
#---------------------
# Function to remove footnotes such as (a) from labels:
strip_brace <- function(df, column_name) {
  column_sym <- sym(column_name)
  df %>%
    mutate(
      !!column_sym := str_remove_all(
        !!column_sym,
        "\\s*\\([A-Za-z]\\)|\\s*\\[[A-Za-z]\\]"
      ) %>%
        str_trim()
    )
}

# Function to clean text from numeric columns:
force_numeric <- function(df, target_cols) {
  df %>%
    mutate(across(all_of(target_cols), ~ {
      suppressWarnings(as.numeric(ifelse(. %in% c("n.p", "-", "—", "np"), NA, .)))
    }))
}

# Template to organise AUSNUT tables
AUSNUT_class <- read_excel("./AUSNUT23_class.xlsx", sheet = 1) 

# Template to organise Nutrient tables
Nut_class <- read_excel("./NUT23_class.xlsx", sheet = 1) %>% 
  strip_brace("Nutrient")

# Template to organise Macronutrient tables
Macro_class <- read_excel("./Macro23_class.xlsx", sheet = 1) %>% 
  strip_brace("Macro")

# Define age cols
age_cols <- c("02-04", "05-11", "12-17", "18-29", "30-49", 
              "50-64", "65-74", "75+", "02-17", "18+", "Total")

#-------------------------------------------------
#---- Read in Excel datacube and do pre-processing 
#-------------------------------------------------

# Function for reading in from dcpath:
read_dc_excel <- function(dcpath, filename, sheet, range) {
  url <- paste0(dcpath, "/", filename)
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = temp_file, mode = "wb")
  readxl::read_excel(temp_file, sheet = sheet, range = range)
}

#-------------------------
#  Table 1 Mean nutrients:
#-------------------------
# Table 1 function
process_nutrient_table <- function(dcpath, filename, sheet_nums, val_names, rse_names, nut_class, age_cols) {
  sexes <- c("Persons", "Males", "Females")
  
# Estimates (val)
val_data <- purrr::map2(sheet_nums[c(1, 2, 3)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A7:M56") %>%
                            setNames(val_names) %>%
                            mutate(Sex = .y) %>%
                            strip_brace("Nutrient") %>%
                            left_join(nut_class, by = "Nutrient") %>%
                            mutate(Type_unit = "Mean") %>% 
                            select(Sex, Type, Type_unit, Order, everything()) %>%
                            force_numeric(age_cols) %>%
                            drop_na(Type)
  ) %>%
    bind_rows() %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "val") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons")),
      Nutrient = gsub("\\([a-zA-Z]\\)", "", Nutrient)
      )
  
# RSE 
  rse_data <- purrr::map2(sheet_nums[c(4, 5, 6)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L56") %>%
                            setNames(rse_names) %>%
                            mutate(Sex = .y) %>%
                            force_numeric(age_cols) %>%
                            strip_brace("Nutrient") %>%
                            left_join(nut_class, by = "Nutrient") %>%
                            select(Sex, Type, Order, everything()) %>%
                            drop_na(Type)
  ) %>%
    bind_rows() %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "RSE") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons"))
    )
  
  # Join and calculate confidence intervals
  final_table <- val_data %>%
    left_join(select(rse_data, Sex, Nutrient, `Age group`, RSE),
              by = c("Sex", "Nutrient", "Age group")) %>%
    mutate(
      lowerCI = round(val - (1.96 * RSE / 100), 1),
      upperCI = round(val + (1.96 * RSE / 100), 1)
    ) %>%
    select(-RSE)
  
  return(final_table)
}

# Define Table1 colnames:
T1_val_names <- c("Nutrient", "Unit", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")
T1_rse_names <- c("Nutrient", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")

# Process Table1 
Table1_2023 <- process_nutrient_table(
  dcpath = dcpath,
  filename = "NNPASDC01.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  val_names = T1_val_names,
  rse_names = T1_rse_names,
  nut_class = Nut_class,
  age_cols = age_cols
) %>% 
  mutate(Year = "2023")

Table1_2011 <- process_nutrient_table(
  dcpath = dcpath,
  filename = "NNPASTSS01.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  val_names = T1_val_names,
  rse_names = T1_rse_names,
  nut_class = Nut_class,
  age_cols = age_cols
) %>% 
  mutate(Year = "2011-12")

Table1 <- Table1_2023 %>% 
  bind_rows(Table1_2011)

#---------------------------------
#  Table 2 Mean % kJ from macros:
#---------------------------------
# Define colnames
T2_names <- c("Macronutrient", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")

# Table2 function:
process_macronutrient_table <- function(dcpath, filename, sheet_nums, col_names, age_cols) {
  sexes <- c("Persons", "Males", "Females")
  
  # Read and process value tables
  val_data <- purrr::map2(sheet_nums[c(1, 2, 3)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L25") %>%
                            setNames(col_names) %>%
                            strip_brace("Macronutrient") %>%
                            force_numeric(age_cols) %>%
                            mutate(
                              Sex = .y,
                              Unit = case_when(Macronutrient == "Mean energy (kJ)" ~ "kJ", TRUE ~ "Percent"),
                              Type = "Energy_percent",
                              Order = row_number()
                            )
  ) %>%
    bind_rows() %>%
    select(Sex, Type, Order, Macronutrient, Unit, everything()) %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "val") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons"))
    )
  
  # Read and process RSE tables
  rse_data <- purrr::map2(sheet_nums[c(4, 5, 6)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L25") %>%
                            setNames(col_names) %>%
                            strip_brace("Macronutrient") %>%
                            force_numeric(age_cols) %>%
                            mutate(
                              Sex = .y,
                              Macronutrient = case_when(
                                Macronutrient == "Mean energy (Relative Standard Error of mean – %)" ~ "Mean energy (kJ)",
                                TRUE ~ Macronutrient
                              )
                            )
  ) %>%
    bind_rows() %>%
    select(Sex, Macronutrient, everything()) %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "RSE") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons"))
    )
  
  # Join and calculate confidence intervals
  final_table <- val_data %>%
    left_join(rse_data, by = c("Sex", "Macronutrient", "Age group")) %>%
    mutate(
      lowerCI = round(val - (1.96 * RSE / 100), 1),
      upperCI = round(val + (1.96 * RSE / 100), 1)
    ) %>%
    select(-RSE)
  
  return(final_table)
}

# Pre-process Table 2
Table2_2023 <- process_macronutrient_table(
  dcpath = dcpath,
  filename = "NNPASDC02.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  col_names = T2_names,
  age_cols = age_cols
) %>% 
  mutate(Year = "2023")

Table2_2011 <- process_macronutrient_table(
  dcpath = dcpath,
  filename = "NNPASTSS02.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  col_names = T2_names,
  age_cols = age_cols
) %>% 
  mutate(Year = "2011-12")

Table2 <- Table2_2023 %>% 
  bind_rows(Table2_2011)

#------------------------------
# Table 3 Nutrients per 1000 kJ 
#------------------------------
T3_est_names <- c("Nutrient", "Unit", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")
T3_rse_names <- c("Nutrient", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")


process_nutrient_table3 <- function(dcpath, filename, sheet_nums, est_names, rse_names, nut_class, age_cols) {
  sexes <- c("Persons", "Males", "Females")
  
  # Estimate tables
  est_data <- purrr::map2(sheet_nums[c(1, 2, 3)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A8:M55") %>%
                            set_names(est_names) %>%
                            strip_brace("Nutrient") %>%
                            force_numeric(age_cols) %>%
                            drop_na(Unit) %>%
                            mutate(Sex = .y) %>%
                            left_join(nut_class, by = "Nutrient") %>%
                            mutate(Type_unit = "per 1,000 kJ") %>% 
                            select(Sex, Type, Type_unit, Order, Nutrient, Unit, everything())
  ) %>%
    bind_rows() %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "val") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons"))
    )
  
  # RSE tables
  rse_data <- purrr::map2(sheet_nums[c(4, 5, 6)], sexes, ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A8:L55") %>%
                            set_names(rse_names) %>%
                            strip_brace("Nutrient") %>%
                            force_numeric(age_cols) %>%
                            drop_na(Total) %>%
                            mutate(Sex = .y) %>%
                            select(Sex, Nutrient, everything())
  ) %>%
    bind_rows() %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "RSE") %>%
    mutate(
      `Age group` = factor(`Age group`, levels = age_cols),
      Sex = factor(Sex, levels = c("Males", "Females", "Persons"))
    )
  
  # Join and calculate confidence intervals
  final_table <- est_data %>%
    left_join(rse_data, by = c("Sex", "Nutrient", "Age group")) %>%
    mutate(
      lowerCI = round(val - (1.96 * RSE / 100), 1),
      upperCI = round(val + (1.96 * RSE / 100), 1)
    ) %>%
    select(-RSE)
  
  return(final_table)
}

# Process Table 3:
Table3_2023 <- process_nutrient_table3(
  dcpath = dcpath,
  filename = "NNPASDC03.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  est_names = T3_est_names,
  rse_names = T3_rse_names,
  nut_class = Nut_class,
  age_cols = age_cols
) %>% 
  mutate(Year = "2023")

# Process 2011-12 Table 3:
Table3_2011 <- process_nutrient_table3(
  dcpath = dcpath,
  filename = "NNPASTSS03.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  est_names = T3_est_names,
  rse_names = T3_rse_names,
  nut_class = Nut_class,
  age_cols = age_cols
) %>% 
  mutate(Year = "2011-12")

# Create the dummy data frame (add dummy rows for Energy per 1,000 kJ )
# Define ordered categories
sex_order <- c("Persons", "Males", "Females")
year_order <- c("2011-12", "2023")
age_groups <- c("02-04", "05-11", "12-17", "18-29", "30-49", 
                "50-64", "65-74", "75+", "02-17", "18+", "Total")

dummy_df <- expand.grid(
  Sex = sex_order,
  Year = year_order,
  `Age group` = age_groups
) %>%
  mutate(
    Type = "Energy",
    Type_unit = "per 1,000 kJ",
    Order = 1,
    Nutrient = "Energy",
    Unit = "kJ",
    val = 1000,
    lowerCI = 1000,
    upperCI = 1000
  ) %>%
  mutate(
    Sex = factor(Sex, levels = sex_order),
    Year = factor(Year, levels = year_order),
    `Age group` = factor(`Age group`, levels = age_groups)
  ) %>%
  arrange(Year, Sex, `Age group`)

#  Final Table 3 (add dummy rows for Energy per 1,000 kJ )
Table3 <- Table3_2023 %>% 
  bind_rows(Table3_2011, dummy_df)


#---------------------------
# AUSNUT tables  (4 to 8)
#---------------------------
AUSNUT_names <- c("Label", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")

process_AUSNUT_tables <- function(filename, sheet_nums, names_vec, ausnut_df, type_label, unit_label) {

  # Read and process the main data
  main_data <- purrr::map2(sheet_nums[c(1, 2, 3)], c("Persons", "Males", "Females"), ~
                             read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L155") %>%
                             setNames(names_vec) %>%
                             left_join(ausnut_df, by = "Label") %>%
                             mutate(Sex = .y)
  ) %>%
    bind_rows() %>%
    mutate(Type = type_label, Unit = unit_label) %>%
    select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, everything()) %>%
    pivot_longer(9:19, names_to = "Age group", values_to = "val") %>%
    mutate(val = case_when(val %in% c("—", "np") ~ NA_real_, TRUE ~ as.numeric(val)))
  
  # Read and process the RSE data
  rse_data <- purrr::map2(sheet_nums[c(4, 5, 6)], c("Persons", "Males", "Females"), ~
                            read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L155") %>%
                            setNames(names_vec) %>%
                            left_join(ausnut_df, by = "Label") %>%
                            mutate(Sex = .y)
  ) %>%
    bind_rows() %>%
    mutate(Type = paste(type_label, "RSE"), Unit = unit_label) %>%
    select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, everything()) %>%
    pivot_longer(9:19, names_to = "Age group", values_to = "RSE") %>%
    mutate(RSE = case_when(RSE %in% c("—", "np") ~ NA_real_, TRUE ~ as.numeric(RSE))) %>%
    select(Sex, full_code, `Age group`, RSE)
  
  # Combine and calculate confidence intervals
  final_table <- main_data %>%
    left_join(rse_data, by = c("Sex", "full_code", "Age group")) %>%
    mutate(lowerCI = round(val - (1.96 * RSE / 100), 1),
           upperCI = round(val + (1.96 * RSE / 100), 1)) %>%
    select(-RSE)
  
  return(final_table)
}

# Table 4 (AUSNUT day 1 consumption prevalence)
Table4 <- process_AUSNUT_tables(
  filename = "NNPASDC04.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Percent consumed",
  unit_label = "Percent"
)

# Table 5 (AUSNUT day 1 mean grams)
Table5 <- process_AUSNUT_tables(
  filename = "./NNPASDC05.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Mean grams",
  unit_label = "g"
)

# Table 6 (AUSNUT day 1 median g) 
Table6 <- process_AUSNUT_tables(
  filename = "./NNPASDC06.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Median grams",
  unit_label = "g"
)

# Table 7 (AUSNUT day 1 mean kJ) 
Table7 <- process_AUSNUT_tables(
  filename = "./NNPASDC07.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Mean kJ",
  unit_label = "kJ"
)


# Table 8 (AUSNUT day 1 mean disc kJ) 
Table8 <- process_AUSNUT_tables(
  filename = "./NNPASDC08.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Disc energy",
  unit_label = "Percent"
)

#-------------------------------------
#----Bind common tables ----:
#-------------------------------------

# Nutrient tables (1, 3 TS1,TS3)
Nutrients_tab <- Table1 %>% 
  bind_rows(Table3)

Macro_kJ_tab <- Table2 

# AUSNUT tables (4:8)
AUSNUT_tab01 <- Table4 %>% 
  bind_rows(Table5, Table6, Table7, Table8) %>% 
  mutate(cLabel = case_when(Class_level == "Major" ~ paste0(maj_code, ", ", Label),
                            Class_level == "Sub-major" ~ paste0(full_code, ", ", Label),
                            TRUE ~ NA_character_)) %>% 
mutate(Sex = factor(Sex,
                      levels = c("Males", "Females", "Persons")))
  
# Add 'majmin' as a class level:
majmin <- AUSNUT_tab01 %>% 
  filter(Class_level == "Sub-major") %>% 
  mutate(Class_level = "MajMin") %>% 
  mutate(cLabel = NA)

AUSNUT_tab02 <- AUSNUT_tab01 %>% 
  bind_rows(majmin)

# Add Two-dig parent for three-dig:
two_dig_label <- AUSNUT_tab02 %>% 
  select(maj_code, Label) %>% 
  mutate(submajCode = paste0(maj_code, ", ", Label)) %>% 
  select(-Label) %>% 
  rename(Codec = maj_code) %>% 
  distinct() %>% 
  filter(Codec %!in% c("2400", "2500")) %>% 
  drop_na()

AUSNUT_tab <- AUSNUT_tab02 %>% 
  left_join(two_dig_label, by = c("maj_code" = "Codec")) %>% 
  mutate(submajCode = case_when(!is.na(cLabel) ~ NA_character_, TRUE ~ submajCode)) %>% 
  distinct()

#-----------------------------
# Generate label lists for UI
#-----------------------------
Two_dig <- AUSNUT_tab %>%
  filter(Class_level %in% c("Major")) %>% 
  select(maj_code, Label, cLabel) %>%
  filter(cLabel != "25, Total persons ('000)") %>% 
  distinct() %>% 
  group_by(cLabel) %>%
  tally()

Two_dig <- as.list(as.character(Two_dig$cLabel))

Thr_dig <- AUSNUT_tab %>%
  filter(Class_level == "Sub-major") %>% 
  select(sub_code, Label, cLabel) %>%
  distinct() %>% 
  group_by(cLabel) %>%
  tally()

Thr_dig <- as.list(as.character(Thr_dig$cLabel))

Nut_unit <- Nutrients_tab %>% 
  select(Nutrient, Unit)%>% 
  distinct() %>% 
  mutate(order = row_number())

Nutrient <- as.list(as.character(Nut_unit$Nutrient))

Macro <- Macro_kJ_tab %>%
  select(Macronutrient) %>%
  distinct() 

Macro <- as.list(as.character(Macro$Macronutrient))

# `Age group` list 
`Age group` <- AUSNUT_tab %>% 
  select(`Age group`) %>% 
  distinct() 

`Age group` <- as.list(`Age group`$`Age group`)

#--- Colours---
abscol <- c("#4FADE7", 	"#1A4472", 	"#F29000", 	"#993366", 	"#669966", 	"#99CC66",
            "#CC9966", 	"#666666", 	"#8DD3C7", 	"#BEBADA", 	"#FB8072", 	"#80B1D3",
            "#FDB462", 	"#B3DE69", 	"#FCCDE5", 	"#D9D9D9", 	"#BC80BD", 	"#CCEBC5", 	"#ffcc99")

# Source the custom styles function
source("./custom_styles.R")

#---Today's date 
now <- format(today(),"%d %B %Y")

#------------
#----UI----
#------------
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  custom_styles(),  # Use the custom styles function
  
  headerPanel("NNPAS 2023 Data cube visualisation"),
  
  sidebarPanel(
    radioButtons("choosetable", "Select table:",
                 choices = c("AUSNUT foodgroups" = "AUSNUT",
                             "Nutrients" = "Nutrients",
                             "Macronutrient kJ" = "Macro"),
                 selected = "AUSNUT", inline = FALSE),
    
    conditionalPanel(
      condition = "input.choosetable == 'AUSNUT'",
      
      radioButtons("Class1", "Classification level:", 
                   choices = c("Major", "Sub-major", "Sub-major within Major" = "MajMin"), 
                   selected = "Major", inline = TRUE),
      
      conditionalPanel(
        condition = "input.Class1 == 'Major'",
        pickerInput("Majgrp1", "AUSNUT major food groups:", choices = Two_dig, 
                    selected = "01, Non-alcoholic beverages", multiple = TRUE, 
                    options = list(`actions-box` = TRUE))
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'Sub-major'",
        pickerInput("Mingrp1", "AUSNUT sub-major food groups:", choices = Thr_dig,
                    selected = "0101, Tea", multiple = TRUE, 
                    options = list(`actions-box` = TRUE))
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'MajMin'", 
        pickerInput("MajMin", "Components of major food groups:", choices = Two_dig,
                    multiple = TRUE, options = list(`actions-box` = TRUE))
      ),
      
      radioButtons("A_Nutrient", "Estimate:", 
                   choices = c("% consumers" = "Percent consumed",
                               "Mean grams" = "Mean grams", 
                               "Median grams" = "Median grams", 
                               "Mean kJ" = "Mean kJ",
                               "% Discretionary kJ" = "Disc energy"), 
                   selected = "Mean grams", inline = TRUE)
    ),
    
    conditionalPanel(
      condition = "input.choosetable == 'Nutrients'",
      
      radioButtons("type_unit", "Mean or per 1,000 kJ:", 
                   choices = c("Mean", "per 1,000 kJ"), selected = "Mean", inline = TRUE),
      
      checkboxGroupInput("Year_nut", "Year:",
                         choices = c("2011-12", "2023" ), selected = "2023", inline = TRUE),
      
      pickerInput("Nutrient", "Select nutrients:",
                  choices = Nutrient,
                  selected = "Energy", multiple = FALSE, 
                  options = list(`actions-box` = TRUE))
    ),
    
    conditionalPanel(
      condition = "input.choosetable == 'Macro'",
      
      checkboxGroupInput("Year_macro", "Year:",
                         choices = c("2011-12", "2023" ), selected = "2011-12", inline = TRUE),
      
      pickerInput("MacrokJ", "Select macronutrients:",
                  choices = Macro,
                  selected = c("Protein", "Total fat", "Carbohydrate", "Dietary fibre", "Alcohol"),
                  multiple = TRUE, options = list(`actions-box` = TRUE))
    ),
    
    checkboxGroupInput("Sex", "Sex:", 
                       choices = c("Males", "Females", "Persons"), 
                       selected = "Persons", inline = TRUE),
    
    
    checkboxGroupButtons(
      inputId = "Agegroup",
      label = "Age group:",
      choices = c("02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total"),
      selected = c("02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+"),
      direction = "horizontal",
      status = "primary",
      justified = FALSE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    ),
    helpText("Click the button below to deselect all age groups except 'Total'."),
    actionButton("select_total", "Select only 'Total'", icon = icon("filter")),
    
    checkboxInput("showDataLabels", "Show Data Labels", value = TRUE),
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Graph", uiOutput("warning"), highchartOutput("hcontainer", height = "760px")),
                tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
    ),
    
    tags$div(class = "header", checked = NA,
             tags$p("Source: ", tags$a(href = "https://www.abs.gov.au/statistics/health/food-and-nutrition/national-nutrition-and-physical-activity-survey/2023",
                                       target = "_blank", "National Nutrition and Physical Activity Survey, 2023")),
             tags$p(paste0("Data retrieved from ABS, ", now))
    ),
    
    downloadButton("downloadTb", "Download graph/table selection:")
  )
)

#------------
# Server ---
#------------

# Define the custom function to apply font settings (bar / col)
server <- function(input, output, session) {  
  
  apply_font_styles <- function(hc, showDataLabels) {
    hc %>%
      hc_xAxis(
        title = list(style = list(fontSize = '18px')),
        labels = list(style = list(fontSize = '16px'))
      ) %>%
      hc_yAxis(
        title = list(style = list(fontSize = '18px')),
        labels = list(style = list(fontSize = '16px'))
      ) %>%
      hc_legend(
        itemStyle = list(fontSize = '16px')  # Set the font size for the legend
      ) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = showDataLabels,
            style = list(fontSize = '14px')
          ),
          marker = list(enabled = FALSE)
        ))
  }
  
  Sex <-  reactive({
    list(Sex =input$Sex) })

  Agegroup <-  reactive({
    list(Agegroup =input$Agegroup) })
    
  Maj1 <-  reactive({
  list(Majgrp1 =input$Majgrp1) })  
  
  Min1 <-  reactive({
  list(Mingrp1 =input$Mingrp1) })
                  
  Class1 <- reactive({
  list(Class1 =input$Class1) })
                  
  NutA <-  reactive({
  list(A_Nutrient =input$A_Nutrient) }) 
                  
  Nutrients <- reactive({
  list(Nutrient = input$Nutrient) })  
                  
  MacrokJ <-  reactive({
  list(MacrokJ =input$MacrokJ) }) 
  
  MajMin <-  reactive({
    list(MajMin =input$MajMin) })  
  
  Year_nut <-  reactive({
    list(Year_nut =input$Year_nut) })
  
  Year_macro <-  reactive({
    list(Year_macro =input$Year_macro) })  
  
    format_years <- function(years) {
    if (length(years) == 1) {
      return(years)
    } else {
      return(paste(years, collapse = " and "))
    }
  }
  
  # --- Observer function for 'Select only Total' button ---
  observeEvent(input$select_total, {
    updateCheckboxGroupButtons(
      session,
      inputId = "Agegroup",
      selected = "Total"
    )
  })
  
# Filter dataframes according to inputs 
Ausnut_tab_filtered <- reactive({
                      AUSNUT_tab %>%  
                      filter(Sex %in% Sex()$Sex) %>%
                      filter(`Age group` %in% Agegroup()$Agegroup) %>% 
                      filter(Type == input$A_Nutrient) %>% 
                      filter(Class_level %in% Class1()$Class1) %>%
                      filter(cLabel %in% input$Majgrp1 | cLabel %in% input$Mingrp1 | submajCode %in% input$MajMin)
                  })
                 
#  Units lookup - Ausnut table  
  U <- reactive({
    Ausnut_tab_filtered() %>%
      distinct(Unit) %>%
      slice(1) %>%
      rename(unit = Unit)
  })
  
    Nutrient_tab_filtered <- reactive({
    Nutrients_tab %>%
    mutate(Year = factor(Year, levels = c("2011-12", "2023"))) %>% 
      filter(Sex %in% Sex()$Sex) %>%
      filter(Type_unit == input$type_unit) %>%
      filter(Year %in% Year_nut()$Year_nut) %>%
      mutate(Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>%
      filter(`Age group` %in% Agegroup()$Agegroup) %>%
      filter(Nutrient %in% Nutrients()$Nutrient)
  })
  
#  Units lookup - Nutrient table
  Un <- reactive({
    Nutrient_tab_filtered() %>%
      distinct(Unit) %>%
      slice(1) %>%
      rename(unit = Unit)
  })
  
# Type_unit - Nutrient table
  Type_unit <- reactive({
    Nutrient_tab_filtered() %>%
      distinct(Type_unit) %>%
      slice(1) %>%
      rename(Type_unit = Type_unit)
  })
  
  Macro_tab_filtered <- reactive({
      Macro_kJ_tab %>% 
      mutate(Year = factor(Year, levels = c("2011-12", "2023"))) %>% 
      filter(Sex %in% Sex()$Sex) %>%
      mutate(Sex = factor(Sex,
                          levels = c("Males", "Females", "Persons"))) %>% 
      filter(`Age group` %in% Agegroup()$Agegroup) %>% 
      filter(Year %in% Year_macro()$Year_macro) %>% 
      mutate(Macronutrient = factor(Macronutrient,
                                    levels = c("Mean energy (kJ)",
                                               "Total energy",
                                               "Carbohydrate",
                                               "Total sugars",
                                               "Free sugars",
                                               "Added sugars",
                                               "Starch",
                                               "Total fat",
                                               "Saturated fat",
                                               "Trans fatty acids",   
                                               "Saturated fat + trans fatty acids",
                                               "Monounsaturated fat",
                                               "Polyunsaturated fat",
                                               "Alpha-linolenic acid",
                                               "Linoleic acid",
                                               "Protein",
                                               "Dietary fibre",
                                               "Alcohol"))) %>%
      filter(Macronutrient %in% MacrokJ()$MacrokJ) 
  })  
                
#  Units Macro table
Um <- reactive({
  Macro_kJ_tab %>%
    filter(Macronutrient %in% MacrokJ()$MacrokJ) %>%
    distinct(Unit) %>%
    slice(1) %>%
    rename(unit = Unit)
})

# Output tables
#--- Set up table objects for DT and Excel from df---  
dt_Ausn <- reactive({ Ausnut_tab_filtered() %>% 
    select(Class_level, Label, val, Unit, Sex, `Age group`) %>% 
    distinct() %>% 
    pivot_wider(1:4, names_from = `Age group`, values_from = val)})

dt_Nut <-  reactive({ Nutrient_tab_filtered() %>% 
    select(Nutrient, Unit, Sex, Year, `Age group`, val) %>% 
    distinct() %>% 
    pivot_wider(1:4, names_from = `Age group`, values_from = val)})

dt_Macro <- reactive({ Macro_tab_filtered() %>% 
    select(Macronutrient, Unit, Sex, Year, `Age group`, val) %>% 
    distinct() %>% 
    pivot_wider(1:4, names_from = `Age group`, values_from = val)})
  

# Table for DT display
output$table = DT::renderDataTable({
  
 if (input$choosetable == 'AUSNUT') {
    tab <- dt_Ausn() }
  
 if (input$choosetable == 'Nutrients') {
    tab <- dt_Nut() }
  
  if (input$choosetable == 'Macro') {
    tab <-dt_Macro()  }
 
  tab
})

# Table for download
dltab <- reactive({
  tab <- switch(input$choosetable,
                               'AUSNUT' = dt_Ausn(),
                               'Nutrients' = dt_Nut(),
                               'Macro' = dt_Macro()                )
tab
})

# Downloadable xlsx --
output$downloadTb <- downloadHandler(
  filename = function() { paste0("NNPAS 2023, Selected", input$choosetable," by age for ", input$Sex,".xlsx") },
  content = function(file) { write_xlsx(dltab(), path = file) }
) 

#--------------
# Output plots:          
#--------------
# For titles: 
Label <- reactive({
  unique(Ausnut_tab_filtered()$Label)
})

nut_year <- reactive({
  unique(Nutrient_tab_filtered()$Year)
})

macro_year <- reactive({
  unique(Macro_tab_filtered()$Year)
})


# Reactive object for groupby in plot. 

#1) Grouping variable for AUSNUT tables  
groupby <- reactive({
  sex_vals <- input$Sex
  age_vals <- input$Agegroup
  maj_vals <- input$Majgrp1
  min_vals <- input$Mingrp1
  majmin_vals <- input$MajMin
 
  food_group_complex <- (length(maj_vals) > 1 || length(min_vals) > 1 || length(majmin_vals) >0)
  
  if (length(sex_vals) == 1 &&
      length(age_vals) > 1 &&
      food_group_complex) {
    "Label"
  } else {
    "Sex"
  }
})

# 2) X-axis variable for AUSNUT tables    
x_axis <- reactive({
  sex_vals <- input$Sex
  age_vals <- input$Agegroup
  maj_vals <- input$Majgrp1
  min_vals <- input$Mingrp1
  majmin_vals <- input$MajMin
  
  food_group_complex <- (length(maj_vals) > 1 || length(min_vals) > 1)
  food_group_complex <- (length(maj_vals) > 1 || length(min_vals) > 1 || length(majmin_vals) >0)
  
  if ((length(sex_vals) == 1 && length(age_vals) == 1 && !food_group_complex) ||
      (length(sex_vals) == 1 && length(age_vals) == 1 && food_group_complex) ||
     (length(sex_vals) >= 1 && length(age_vals) == 1 && food_group_complex)) 
    {
    "Label"
  } else {
    "Age group"
  }
})

# Dynamic group & x-axis for nutrient table:
groupby_nut <- reactive({
  sex_vals <- input$Sex
  age_vals <- input$Agegroup
  nut_vals <- input$Nutrient
  year_vals <- input$Year_nut
  
  # Combination logic
  if (length(sex_vals) >= 1 && length(age_vals) > 1 && length(nut_vals) == 1 && length(year_vals) == 1) {
    return("Sex")  # A
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(nut_vals) >= 1 && length(year_vals) == 1) {
    return("Sex")  # B
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(nut_vals) == 1 && length(year_vals) > 1) {
    return("Year")  # C
  } else if (length(sex_vals) == 1 && length(age_vals) == 1 && length(nut_vals) >= 1 && length(year_vals) > 1) {
    return("Year")  # D
  } else if (length(sex_vals) == 1 && length(age_vals) > 1 && length(nut_vals) == 1 && length(year_vals) > 1) {
    return("Year")  # E
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(nut_vals) > 1 && length(year_vals) >= 1) {
    return("Sex")  # F
  } else if (length(sex_vals) > 1 && length(age_vals) >= 1 && length(nut_vals) > 1 && length(year_vals) > 1) {
    return("Sex")  # G
  } else if (length(sex_vals) >= 1 && length(age_vals) > 1 && length(nut_vals) > 1 && length(year_vals) > 1) {
    return("Year")  # H
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(nut_vals) >= 1 && length(year_vals) > 1) {
    return("Sex")  # I
  } else {
    return("Sex")  # Default fallback
  }
})

x_axis_nut <- reactive({
  sex_vals <- input$Sex
  age_vals <- input$Agegroup
  nut_vals <- input$Nutrient
  year_vals <- input$Year_nut
  
  # Combination logic
  if (length(sex_vals) >= 1 && length(age_vals) > 1 && length(nut_vals) == 1 && length(year_vals) == 1) {
    return("Age group")  # A
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(nut_vals) >= 1 && length(year_vals) == 1) {
    return("Nutrient")  # B
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(nut_vals) == 1 && length(year_vals) > 1) {
    return("Sex")  # C
  } else if (length(sex_vals) == 1 && length(age_vals) == 1 && length(nut_vals) >= 1 && length(year_vals) > 1) {
    return("Nutrient")  # D
  } else if (length(sex_vals) == 1 && length(age_vals) > 1 && length(nut_vals) == 1 && length(year_vals) > 1) {
    return("Age group")  # E
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(nut_vals) > 1 && length(year_vals) >= 1) {
    return("Age group")  # F
  } else if (length(sex_vals) > 1 && length(age_vals) >= 1 && length(nut_vals) > 1 && length(year_vals) > 1) {
    return("Nutrient")  # G
  } else if (length(sex_vals) >= 1 && length(age_vals) > 1 && length(nut_vals) > 1 && length(year_vals) > 1) {
    return("Age group")  # H
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(nut_vals) >= 1 && length(year_vals) > 1) {
    return("Age group")  # I
  } else {
    return("Age group")  # Default fallback
  }
})

# Dynamic group & x-axis for Macronutrient tables
groupby_macro <- reactive({
  sex_vals   <- input$Sex
  age_vals   <- input$Agegroup
  macro_vals <- input$MacrokJ
  year_vals  <- input$Year_macro
  
  if (length(sex_vals) == 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) > 1) {
    "Year"  # A
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(macro_vals) >= 1 && length(year_vals) == 1) {
    "Macronutrient"  # B
  } else if (length(sex_vals) == 1 && length(age_vals) >= 1 && length(macro_vals) >= 1 && length(year_vals) == 1) {
    "Macronutrient"  # D
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(macro_vals) == 1 && length(year_vals) >= 1) {
    "Year"  # E
  } else if (length(sex_vals) == 1 && length(age_vals) == 1 && length(macro_vals) >= 1 && length(year_vals) > 1) {
    "Macronutrient"  # D (again)
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) == 1) {
    "Sex"  # E (again)
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) > 1 && length(year_vals) >= 1) {
    "Year"  # F
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) > 1) {
    "Year"  # G
  } else if (length(sex_vals) > 1 && length(age_vals) == 1 && length(macro_vals) > 1 && length(year_vals) > 1) {
    "Macronutrient"  # H
  } else if (length(sex_vals) == 1 && length(age_vals) > 1 && length(macro_vals) > 1 && length(year_vals) > 1) {
    "Macronutrient"  # I
  } else {
    "Macronutrient"  # Default fallback
  }
})

x_axis_macro <- reactive({
  sex_vals   <- input$Sex
  age_vals   <- input$Agegroup
  macro_vals <- input$MacrokJ
  year_vals  <- input$Year_macro
  
  if (length(sex_vals) == 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) > 1) {
    "Age group"  # A
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(macro_vals) >= 1 && length(year_vals) == 1) {
    "Sex"  # B
  } else if (length(sex_vals) == 1 && length(age_vals) >= 1 && length(macro_vals) >= 1 && length(year_vals) == 1) {
    "Age group"  # D
  } else if (length(sex_vals) >= 1 && length(age_vals) == 1 && length(macro_vals) == 1 && length(year_vals) >= 1) {
    "Sex"  # E
  } else if (length(sex_vals) == 1 && length(age_vals) == 1 && length(macro_vals) >= 1 && length(year_vals) > 1) {
    "Year"  # D (again)
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) == 1) {
    "Age group"  # E (again)
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) > 1 && length(year_vals) >= 1) {
    "Age group"  # F
  } else if (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) == 1 && length(year_vals) > 1) {
    "Age group"  # G
  } else if (length(sex_vals) > 1 && length(age_vals) == 1 && length(macro_vals) > 1 && length(year_vals) > 1) {
    "Year"  # H
  } else if (length(sex_vals) == 1 && length(age_vals) > 1 && length(macro_vals) > 1 && length(year_vals) > 1) {
    "Age group"  # I
  } else {
    "Age group"  # Default fallback
  }
})

output$warning <- renderUI({
  sex_vals   <- input$Sex
  age_vals   <- input$Agegroup
  maj_vals <- input$Majgrp1
  min_vals <- input$Mingrp1
  majmin_vals <- input$MajMin
  nut_vals   <- input$Nutrient
  year_nut_vals  <- input$Year_nut
  year_macro_vals <- input$Year_macro
  macro_vals <- input$MacrokJ
  
  warnings <- tagList()
  
  # AUSNUT warning
  if (input$choosetable == 'AUSNUT' &&
      length(age_vals) > 1 &&
      length(sex_vals) > 1 &&
      (length(maj_vals) > 1 || length(min_vals) > 1)) {
    
    warnings <- tagAppendChild(warnings, tags$div(class = "alert alert-warning",
                                                  "Too many dimensions selected:  
                                                  If you are trying to compare consumption of Foodgroup by Age group and Sex, 
                                                  please ensure that one of these three variables is limited to a single selection. For example, limit the number of foods selected to 1, or limit Sex to 'Persons' or Age group to 'Total'."))
  }
  
  # Nutrients warning
  if (input$choosetable == "Nutrients" &&
      (
        (length(sex_vals) > 1 && length(age_vals) > 1 && length(nut_vals) > 1) ||  # F
        (length(sex_vals) > 1 && length(nut_vals) > 1 && length(year_nut_vals) > 1) ||  # G
        (length(age_vals) > 1 && length(nut_vals) > 1 && length(year_nut_vals) > 1) ||  # H
        (length(sex_vals) > 1 && length(age_vals) > 1 && length(year_nut_vals) > 1)     # I
      )) {
    warnings <- tagAppendChild(warnings, tags$div(class = "alert alert-warning",
                                                  "Too many dimensions selected:  
                                                  If you are trying to compare consumption of Nutrient(s) by Year Age group and Sex, 
                                                  please ensure that one of these four variables is limited to a single selection. For example, limit the number of Nutrients to one and limit Sex to 'Persons' or Age group to 'Total' or limit Year to either '2011' or '2023'."))
  }
  
  # Macro warning
  if (input$choosetable == "Macro" &&
      (
        (length(sex_vals) > 1 && length(age_vals) > 1 && length(macro_vals) > 1) ||  # F
        (length(sex_vals) > 1 && length(age_vals) > 1 && length(year_macro_vals) > 1) ||   # G
        (length(sex_vals) > 1 && length(macro_vals) > 1 && length(year_macro_vals) > 1) || # H
        (length(age_vals) > 1 && length(macro_vals) > 1 && length(year_macro_vals) > 1)    # I
      )) {
    warnings <- tagAppendChild(warnings, tags$div(class = "alert alert-warning",
                                                  "Too many dimensions selected:  
                                                  If you are trying to compare consumption of Macronutients by Year Age group and Sex, 
                                                  please ensure that one of these four variables is limited to a single selection. For example, limit Sex to just 'Persons' or Age group to 'Total' or limit Year to either '2011' or '2023'."))
  }
  
  return(warnings)
})

output$hcontainer <- renderHighchart({
  sex_vals <- input$Sex
  age_vals <- input$Agegroup
  maj_vals <- input$Majgrp1
  min_vals <- input$Mingrp1
  majmin_vals <- input$MajMin
  
  
  if (input$choosetable == 'AUSNUT' &&
      length(age_vals) == 1 &&
      (length(maj_vals) > 4 || length(min_vals) > 4) || length(majmin_vals) >0) {
    
    hc <- Ausnut_tab_filtered() %>%
      group_by(Sex) %>%
      arrange(desc(val)) %>%
      ungroup() %>%
      hchart(type = "bar", 
             hcaes(x = !!sym(x_axis()), 
                   y = val,
                   group = !!sym(groupby()))) %>%
      hc_xAxis(title = list(text = x_axis())) %>%
      hc_yAxis(title = list(text = paste0(U()$unit))) %>%
      hc_title(text = paste0(input$A_Nutrient, ", selected foods, ", "2023")) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
      apply_font_styles(input$showDataLabels)
    
  }
  
  else if (input$choosetable == 'AUSNUT' &&
           length(age_vals) > 1 &&
           length(sex_vals) < 2 &&
           ((length(maj_vals) >= 2 && length(maj_vals) <= 4) ||
            (length(min_vals) >= 2 && length(min_vals) <= 4))) {
    
    hc <- Ausnut_tab_filtered() %>%
      hchart(type = "column",
             hcaes(x = `Age group`, 
                   y = val,
                   group = Label)) %>%
      hc_xAxis(title = list(text = x_axis())) %>%
      hc_yAxis(title = list(text = paste0(U()$unit))) %>%
      hc_title(text = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023")) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
      apply_font_styles(input$showDataLabels)
  }
  
  else if (input$choosetable == 'AUSNUT' &&
             length(age_vals) >= 1 &&
             (length(maj_vals) < 4 || length(min_vals) < 4)) {
    
    hc <- Ausnut_tab_filtered() %>%
      hchart(type = "column",
             hcaes(x = !!sym(x_axis()), 
                   y = val,
                   group = !!sym(groupby()))) %>%
      hc_xAxis(title = list(text = x_axis())) %>%
      hc_yAxis(title = list(text = paste0(U()$unit))) %>%
      hc_title(text = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023")) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
      apply_font_styles(input$showDataLabels)
  }
  
  else if (input$choosetable == 'AUSNUT' &&
           length(age_vals) == 1 &&
           length(sex_vals) >1 &&
           (length(maj_vals) > 1 || length(min_vals) >1)) {
    
    hc <- Ausnut_tab_filtered() %>%
      hchart(type = "column",
             hcaes(x = !!sym(x_axis()), 
                   y = val,
                   group = !!sym(groupby()))) %>%
      hc_xAxis(title = list(text = x_axis())) %>%
      hc_title(text = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023")) %>% 
      hc_title(text = paste0(input$A_Nutrient)) %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
      apply_font_styles(input$showDataLabels)
  }  
  
 
  if (input$choosetable == 'Nutrients') {
    hc <- Nutrient_tab_filtered() %>%
      hchart(type = "column", hcaes(
        x = !!sym(x_axis_nut()),
        y = val,
        group = !!sym(groupby_nut())
      )) %>%
      hc_xAxis(title = list(text = x_axis_nut())) %>%
      hc_yAxis(title = list(text = paste0(Un()$unit, ", ", Type_unit()$Type_unit))) %>%
      hc_title(text = paste0("Daily mean ", input$Nutrient, ", ", Un()$unit, ", ", format_years(nut_year()))) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE,
                 valueSuffix = paste0(" ", Un()$unit)) %>%
      apply_font_styles(input$showDataLabels)
  }
  

  if (input$choosetable == 'Macro') {
    hc <- Macro_tab_filtered() %>%
      mutate(Year = factor(Year, levels = c("2011-12", "2023"))) %>% 
      arrange(Year) %>% 
      hchart(type = "column", hcaes(
        x = !!sym(x_axis_macro()),
        y = val,
        group = !!sym(groupby_macro())
      )) %>%
      hc_xAxis(title = list(text = x_axis_macro())) %>%
      hc_yAxis(title = list(text = paste0(Um()$unit))) %>%
      hc_title(text = paste0("Percent dietary energy from selected macronutrients, ", format_years(macro_year()))) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE,
                 valueSuffix = paste0(" ", Um()$unit)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      apply_font_styles(input$showDataLabels)
  }
  
                   hc
}) 

} 
                 
#-------------------
shinyApp(ui, server)
#-------------------
