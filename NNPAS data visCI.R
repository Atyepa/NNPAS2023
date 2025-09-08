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
# AUSNUT tables  (4 to 8  but NOT 7)
#---------------------------

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

AUSNUT_names <- c("Label", "02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+", "02-17", "18+", "Total")

# Process tables:
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

# Table 8 (AUSNUT day 1 mean disc kJ) 
Table8 <- process_AUSNUT_tables(
  filename = "./NNPASDC08.xlsx",
  sheet_nums = c(2, 4, 6, 3, 5, 7),
  names_vec = AUSNUT_names,
  ausnut_df = AUSNUT_class,
  type_label = "Disc energy",
  unit_label = "Percent"
)

#-----------------
# AUSNUT table 7 (it has Sex by: mean, rse, %, MoE)
#-----------------
# Example:
# AUSNUT_names <- c("Label","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")

# Expects:
# AUSNUT_names <- c("Label","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
# AUSNUT_class : lookup with columns Label, Class_level, maj_code, sub_code, full_code

process_AUSNUT_table7 <- function(
    filename,
    sheet_nums,                       # length-12; can be named or positional
    names_vec = AUSNUT_names,
    ausnut_df = AUSNUT_class,
    range = "A7:L155",
    type_labels = c(mean = "Mean kJ", pct = "Percent kJ"),
    unit_labels = c(mean = "kJ",      pct = "%")
) {
  need <- c(
    "mean_persons","rse_persons","pct_persons","moe_persons",
    "mean_males","rse_males","pct_males","moe_males",
    "mean_females","rse_females","pct_females","moe_females"
  )
  
  # Accept named or positional mapping
  if (!is.null(names(sheet_nums)) && all(need %in% names(sheet_nums))) {
    sheet_vec <- sheet_nums[need]
  } else {
    stopifnot(length(sheet_nums) == 12)
    names(sheet_nums) <- need   # assumes workbook is grouped by Sex, then Mean/RSE/Pct/MoE
    sheet_vec <- sheet_nums
  }
  
  # Use the exact order from AUSNUT_names, minus "Label"
  age_cols <- names_vec[names_vec != "Label"]
  
  read_block <- function(sheets3, sexes3, value_col_name = "val") {
    purrr::map2(
      sheets3, sexes3,
      ~ read_dc_excel(dcpath, filename, sheet = .x, range = range) %>%
        setNames(names_vec) %>%
        dplyr::left_join(ausnut_df, by = "Label") %>%
        dplyr::mutate(Sex = .y)
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(Sex, Class_level, maj_code, sub_code, full_code, Label, dplyr::everything()) %>%
      tidyr::pivot_longer(dplyr::all_of(age_cols), names_to = "Age group", values_to = value_col_name) %>%
      dplyr::mutate(
        {{value_col_name}} := dplyr::case_when(
          .data[[value_col_name]] %in% c("—", "np") ~ NA_real_,
          TRUE ~ suppressWarnings(as.numeric(.data[[value_col_name]]))
        )
      )
  }
  
  sexes <- c("Persons","Males","Females")
  
  # 1) Mean kJ
  mean_df <- read_block(
    sheets3 = unname(sheet_vec[c("mean_persons","mean_males","mean_females")]),
    sexes3  = sexes,
    value_col_name = "val"
  ) %>%
    dplyr::mutate(
      Type = type_labels["mean"],
      Unit = unit_labels["mean"]
    ) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val)
  
  # 2) Percent kJ
  pct_df <- read_block(
    sheets3 = unname(sheet_vec[c("pct_persons","pct_males","pct_females")]),
    sexes3  = sexes,
    value_col_name = "val"
  ) %>%
    dplyr::mutate(
      Type = type_labels["pct"],
      Unit = unit_labels["pct"]
    ) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val)
  
  # 3) RSE% for Mean kJ
  rse_df <- read_block(
    sheets3 = unname(sheet_vec[c("rse_persons","rse_males","rse_females")]),
    sexes3  = sexes,
    value_col_name = "RSE"
  ) %>%
    dplyr::select(Sex, full_code, `Age group`, RSE)
  
  # 4) MoE (±95%) for Percent kJ
  moe_df <- read_block(
    sheets3 = unname(sheet_vec[c("moe_persons","moe_males","moe_females")]),
    sexes3  = sexes,
    value_col_name = "MoE"
  ) %>%
    dplyr::select(Sex, full_code, `Age group`, MoE)
  
  # Join + compute CIs; keep only requested columns
  mean_out <- mean_df %>%
    dplyr::left_join(rse_df, by = c("Sex","full_code","Age group")) %>%
    dplyr::mutate(
      lowerCI = round(val - 1.96 * (val * RSE/100), 1),
      upperCI = round(val + 1.96 * (val * RSE/100), 1)
    ) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val, lowerCI, upperCI)
  
  pct_out <- pct_df %>%
    dplyr::left_join(moe_df, by = c("Sex","full_code","Age group")) %>%
    dplyr::mutate(
      lowerCI = round(val - MoE, 1),
      upperCI = round(val + MoE, 1)
    ) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val, lowerCI, upperCI)
  
  dplyr::bind_rows(mean_out, pct_out)
}


# Table 7 (AUSNUT day 1 mean kJ) 
sheets <- c(
  mean_persons = 2, rse_persons = 3, pct_persons = 4, moe_persons = 5,
  mean_males   = 6, rse_males   = 7, pct_males   = 8, moe_males   = 9,
  mean_females = 10, rse_females = 11, pct_females = 12, moe_females = 13
)

Table7 <- process_AUSNUT_table7(
  filename   = "NNPASDC07.xlsx",
  sheet_nums = sheets,
  names_vec  = AUSNUT_names,   # uses your defined vector
  ausnut_df  = AUSNUT_class    # uses your master lookup
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
      
      selectInput("A_Nutrient", "Estimate:", 
                   choices = c("% consumers" = "Percent consumed",
                               "Mean grams" = "Mean grams", 
                               "Median grams" = "Median grams", 
                               "Mean kJ" = "Mean kJ",
                               "% Total energy" = "Percent kJ",
                               "% Discretionary kJ" = "Disc energy"), 
                   selected = "Mean grams", multiple = FALSE), 
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
    # helpText("Click the button below to deselect all age groups except 'Total'."),
    actionButton("select_total", "Select only 'Total'", icon = icon("filter")),
    actionButton("select_age_groups", "Select standard age groups", icon = icon("filter")),
    
    
    checkboxInput("showDataLabels", "Show Data Labels", value = TRUE),
    checkboxInput("showErrorBars", "Show error bars", value = FALSE),
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
  
  # --- Observer function for 'Select only Total' button ---
  observeEvent(input$select_age_groups, {
    updateCheckboxGroupButtons(
      session,
      inputId = "Agegroup",
      selected = c("02-04", "05-11", "12-17", "18-29", "30-49", "50-64", "65-74", "75+") 
    )
  })
  
  
# Filter dataframes according to inputs 
Ausnut_tab_filtered <- reactive({
                      AUSNUT_tab %>%
                      mutate(Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>%
                      arrange(Sex, `Age group`) %>% 
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
        mutate(Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>%
        arrange(Year, Sex, `Age group`) %>% 
      filter(Sex %in% Sex()$Sex) %>%
      filter(Type_unit == input$type_unit) %>%
      filter(Year %in% Year_nut()$Year_nut) %>%
     
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
      mutate(Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>%
      arrange(Year, Sex, `Age group`) %>% 
      filter(Sex %in% Sex()$Sex) %>%
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
  
  food_group_complex <- (length(maj_vals) > 1 || length(min_vals) > 1 || length(majmin_vals) >0)
  
  if ((length(sex_vals) == 1 && length(age_vals) == 1 && !food_group_complex) ||
      (length(sex_vals) == 1 && length(age_vals) == 1 && food_group_complex) ||
     (length(sex_vals) >= 1 && length(age_vals) == 1 )) 
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
    "Sex"  # E
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
    "Year"  # E
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
  
  sex_vals    <- input$Sex
  age_vals    <- input$Agegroup
  maj_vals    <- input$Majgrp1
  min_vals    <- input$Mingrp1
  majmin_vals <- input$MajMin
  
  show_err <- isTruthy(input$showErrorBars)
  
  # ---------- helpers ----------
  build_base_chart <- function(series_type = c("column","bar"),
                               x_title, y_title, title_text, value_suffix) {
    series_type <- match.arg(series_type)
    highchart() %>%
      hc_chart(type = series_type) %>%
      hc_xAxis(title = list(text = x_title)) %>%
      hc_yAxis(title = list(text = y_title)) %>%
      hc_title(text = title_text) %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", value_suffix))
  }
  
  # Manual positioning: disables grouping and sets explicit x for bars + errorbars
  add_grouped_bars_with_errorbars <- function(
    hc, df, xvar, gvar,
    series_type = c("column","bar"),
    categories = NULL,
    inner_pad = 0.2,             # spacing inside each category
    show_errorbars = TRUE        # <— new toggle
  ) {
    series_type <- match.arg(series_type)
    
    if (is.null(categories)) categories <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull()
    hc <- hc %>% hc_xAxis(categories = categories)
    
    # 0-based category index for explicit x placement
    cat_idx <- setNames(seq_along(categories) - 1, as.character(categories))
    
    groups <- df %>% dplyr::distinct(!!gvar) %>% dplyr::pull()
    n <- length(groups)
    
    span <- 1 - 2 * inner_pad
    offsets <- if (n <= 1) 0 else seq(
      from = -span/2 + span/(2*n),
      to   =  span/2 - span/(2*n),
      length.out = n
    )
    
    for (i in seq_along(groups)) {
      g   <- groups[[i]]
      off <- offsets[[i]]
      dfg <- df %>% dplyr::filter(!!gvar == g)
      
      dfgp <- dfg %>%
        dplyr::mutate(
          .cat = as.character(!!xvar),
          .i   = unname(cat_idx[.cat]),
          x    = .i + off
        )
      
      bar_data <- dfgp %>%
        dplyr::transmute(x = x, y = val) %>%
        highcharter::list_parse2()
      
      hc <- hc %>%
        hc_add_series(
          name         = as.character(g),
          type         = series_type,
          data         = bar_data,
          grouping     = FALSE,                # manual layout
          pointRange   = span / max(n, 1),
          pointPadding = 0
        )
      
      if (isTRUE(show_errorbars)) {
        err_data <- dfgp %>%
          dplyr::filter(!is.na(lowerCI), !is.na(upperCI)) %>%
          dplyr::transmute(x = x, low = lowerCI, high = upperCI) %>%
          highcharter::list_parse2()
        
        hc <- hc %>%
          hc_add_series(
            type         = "errorbar",
            data         = err_data,
            linkedTo     = "previous",
            showInLegend = FALSE,
            zIndex       = 6,
            grouping     = FALSE,
            pointRange   = span / max(n, 1),
            tooltip      = list(pointFormat = "95% CI: {point.low}–{point.high}")
          )
      }
    }
    hc
  }
  
  # Special case of stacked column:
  add_stacked_columns_with_errorbars <- function(
    hc, df, xvar, gvar,
    categories = NULL,
    group_padding = 0.2,
    point_padding = 0.1,
    show_errorbars = TRUE
  ) {
    if (is.null(categories)) {
      categories <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull()
    }
    
    hc <- hc %>%
      hc_xAxis(categories = categories) %>%
      hc_plotOptions(
        column   = list(stacking = "normal",
                        groupPadding = group_padding,
                        pointPadding = point_padding),
        errorbar = list(pointRange   = 1 - 2 * group_padding,  # match inner band
                        pointPadding = point_padding)
      )
    
    groups <- df %>% dplyr::distinct(!!gvar) %>% dplyr::pull()
    
    for (g in groups) {
      dfg <- df %>% dplyr::filter(!!gvar == g)
      
      # values aligned to categories (NULL where missing)
      yvec <- lapply(categories, function(cat) {
        row <- dfg %>% dplyr::filter(!!xvar == cat)
        if (nrow(row) == 1) row$val[[1]] else NULL
      })
      
      hc <- hc %>%
        hc_add_series(
          name = as.character(g),
          type = "column",
          data = yvec
          # stacking comes from plotOptions
        )
      
      if (isTRUE(show_errorbars)) {
        err <- lapply(categories, function(cat) {
          row <- dfg %>% dplyr::filter(!!xvar == cat)
          if (nrow(row) == 1 &&
              !is.na(row$lowerCI[[1]]) && !is.na(row$upperCI[[1]])) {
            list(row$lowerCI[[1]], row$upperCI[[1]])
          } else {
            NULL
          }
        })
        
        hc <- hc %>%
          hc_add_series(
            type         = "errorbar",
            data         = err,
            linkedTo     = "previous",
            showInLegend = FALSE,
            zIndex       = 6,
            tooltip      = list(pointFormat = "95% CI: {point.low}–{point.high}")
          )
      }
    }
    hc
  }
  
  # ---------- /helpers ----------
  
  hc <- NULL
  
  # CASE 1: AUSNUT, single age, many categories -> horizontal bars
  if (input$choosetable == "AUSNUT" &&
      length(age_vals) == 1 &&
      ((length(maj_vals) > 4 || length(min_vals) > 4) || length(majmin_vals) > 0)) {
    
    df   <- Ausnut_tab_filtered() %>%
      group_by(!!sym(groupby())) %>%
      arrange(desc(val), .by_group = TRUE) %>%
      ungroup()
    
    xvar <- rlang::sym(x_axis())
    gvar <- rlang::sym(groupby())
    cats <- df %>% distinct(!!xvar) %>% pull()
    
    hc <- build_base_chart(
      series_type = "bar",
      x_title     = x_axis(),
      y_title     = paste0(U()$unit),
      title_text  = paste0(input$A_Nutrient, ", selected foods, 2023"),
      value_suffix = U()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "bar", 
                                      categories = cats, 
                                      show_errorbars  = show_err)
  }
  
  # CASE 2: AUSNUT, >1 age, 1 sex, small selection -> vertical columns (x = Age group, group = Label)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) > 1 &&
           length(sex_vals) < 2 &&
           ((length(maj_vals) >= 2 && length(maj_vals) <= 4) ||
            (length(min_vals) >= 2 && length(min_vals) <= 4))) {
    
    df <- Ausnut_tab_filtered()
    xvar <- rlang::sym("Age group")
    gvar <- rlang::sym("Label")
    cats <- df %>% distinct(!!xvar) %>% pull()
    
    hc <- build_base_chart(
      series_type = "column",
      x_title     = x_axis(),
      y_title     = paste0(U()$unit),
      title_text  = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "column",
                                      categories = cats, 
                                      show_errorbars  = show_err)
    print("Case 2")
  }
  
  # CASE 3: AUSNUT, general columns (dynamic x & group)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) > 1 &&
           ((length(maj_vals) >=1 && length(maj_vals) <= 4) || (length(min_vals)>=1 && length(min_vals) <= 4))) {
    
    df   <- Ausnut_tab_filtered()
    xvar <- rlang::sym(x_axis())
    gvar <- rlang::sym(groupby())
    
    cats <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull() %>% as.character()
    
    hc <- build_base_chart(
      series_type  = "column",
      x_title      = x_axis(),
      y_title      = paste0(U()$unit),
      title_text   = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "column",
                                      categories = cats, 
                                      show_errorbars  = show_err)
    print("Case 3")
    }
  
  # CASE 3.1: AUSNUT, general columns (dynamic x & group)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) <= 1 &&
           (length(maj_vals) <=1 || length(min_vals) <=1 )) {
    
    df   <- Ausnut_tab_filtered()
    xvar <- rlang::sym(x_axis())
    gvar <- rlang::sym(groupby())
    
    cats <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull() %>% as.character()
    
    hc <- build_base_chart(
      series_type  = "column",
      x_title      = paste0(Label()),
      y_title      = paste0(U()$unit),
      title_text   = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "column",
                                      categories = cats, 
                                      show_errorbars  = show_err)
    print("Case 3.1")
  }
  
  
  # CASE 4: AUSNUT, single age, multiple sexes -> vertical columns (dynamic)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) == 1 &&
           length(sex_vals) >= 1 &&
           (length(maj_vals) >= 1 || length(min_vals) >= 1)) {
    
    df   <- Ausnut_tab_filtered()
    xvar <- rlang::sym(x_axis())
    gvar <- rlang::sym(groupby())
    cats <- df %>% distinct(!!xvar) %>% pull()
    
    hc <- build_base_chart(
      series_type = "column",
      x_title     = x_axis(),
      y_title     = paste0(U()$unit),
      title_text  = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "column", 
                                      categories = cats, 
                                      show_errorbars  = show_err)
    print("Case 4")
  }
  
  # CASE 5: Nutrients table
  else if (input$choosetable == "Nutrients") {
    
    df   <- Nutrient_tab_filtered()
    xvar <- rlang::sym(x_axis_nut())
    gvar <- rlang::sym(groupby_nut())
    cats <- df %>% distinct(!!xvar) %>% pull()
    
    hc <- build_base_chart(
      series_type = "column",
      x_title     = x_axis_nut(),
      y_title     = paste0(Un()$unit, ", ", Type_unit()$Type_unit),
      title_text  = paste0("Daily mean ", input$Nutrient, ", ", Un()$unit, ", ", format_years(nut_year())),
      value_suffix = Un()$unit
    ) %>%
      add_grouped_bars_with_errorbars(df, xvar, gvar, series_type = "column",
                                      categories = cats, 
                                      show_errorbars  = show_err)
  }
  
  # CASE 6: Macro table (stacked columns)
  else if (input$choosetable == "Macro") {
    df <- Macro_tab_filtered() %>%
      dplyr::mutate(Year = factor(Year, levels = c("2011-12", "2023"))) %>%
      dplyr::arrange(Year)
    
    xvar <- rlang::sym(x_axis_macro())
    gvar <- rlang::sym(groupby_macro())
    cats <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull()
    show_err <- isTruthy(input$showErrorBars)
    
    hc <- build_base_chart(
      series_type  = "column",
      x_title      = x_axis_macro(),
      y_title      = paste0(Um()$unit),
      title_text   = paste0("Percent dietary energy from selected macronutrients, ",
                            format_years(macro_year())),
      value_suffix = Um()$unit
    ) %>%
      add_stacked_columns_with_errorbars(
        df, xvar, gvar,
        categories     = cats,
        group_padding  = 0.2,   # match your theme if different
        point_padding  = 0.1,
        show_errorbars = show_err # or FALSE
      )
  }
  
  # Style once at the end
  if (!is.null(hc)) {
    hc <- hc %>% apply_font_styles(input$showDataLabels)
  }
  hc
})

} 
                 
#-------------------
shinyApp(ui, server)
#-------------------
