# prep_data.R
# Run this script locally to generate app_data.rds for deployment.
# The Shiny app will load this file at startup instead of re-downloading
# all ABS Excel files on every launch.

message("Loading packages...")
library(tidyverse)
library(readxl)

message("Sourcing helpers...")
source("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/custom_styles.R")
source("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/cleaning_fun.R")

dcpath <- "https://www.abs.gov.au/statistics/health/food-and-nutrition/national-nutrition-and-physical-activity-survey/2023"

message("Loading classification templates...")
AUSNUT_class <- read_xlsx_from_url("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/AUSNUT23_class.xlsx")
Nut_class    <- read_xlsx_from_url("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/NUT23_class.xlsx") %>%
  strip_brace("Nutrient")
Macro_class  <- read_xlsx_from_url("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/Macro23_class.xlsx") %>%
  strip_brace("Macro")

age_cols <- c("02-04", "05-11", "12-17", "18-29", "30-49",
              "50-64", "65-74", "75+", "02-17", "18+", "Total")

# ---------- processing functions (copied from app) ----------

process_nutrient_table <- function(dcpath, filename, sheet_nums, val_names, rse_names, nut_class, age_cols) {
  sexes <- c("Persons", "Males", "Females")
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
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")),
           Nutrient = gsub("\\([a-zA-Z]\\)", "", Nutrient))
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
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")))
  val_data %>%
    left_join(select(rse_data, Sex, Nutrient, `Age group`, RSE), by = c("Sex", "Nutrient", "Age group")) %>%
    mutate(lowerCI = round(val - (1.96 * RSE / 100 * val), 1),
           upperCI = round(val + (1.96 * RSE / 100 * val), 1)) %>%
    select(-RSE)
}

process_macronutrient_table <- function(dcpath, filename, sheet_nums, col_names, age_cols) {
  sexes <- c("Persons", "Males", "Females")
  val_data <- purrr::map2(sheet_nums[c(1, 2, 3)], sexes, ~
    read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L25") %>%
    setNames(col_names) %>%
    strip_brace("Macronutrient") %>%
    force_numeric(age_cols) %>%
    mutate(Sex = .y,
           Unit = case_when(Macronutrient == "Mean energy (kJ)" ~ "kJ", TRUE ~ "Percent"),
           Type = "Energy_percent", Order = row_number())
  ) %>%
    bind_rows() %>%
    select(Sex, Type, Order, Macronutrient, Unit, everything()) %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "val") %>%
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")))
  rse_data <- purrr::map2(sheet_nums[c(4, 5, 6)], sexes, ~
    read_dc_excel(dcpath, filename, sheet = .x, range = "A7:L25") %>%
    setNames(col_names) %>%
    strip_brace("Macronutrient") %>%
    force_numeric(age_cols) %>%
    mutate(Sex = .y,
           Macronutrient = case_when(
             Macronutrient == "Mean energy (Relative Standard Error of mean – %)" ~ "Mean energy (kJ)",
             TRUE ~ Macronutrient))
  ) %>%
    bind_rows() %>%
    select(Sex, Macronutrient, everything()) %>%
    pivot_longer(cols = all_of(age_cols), names_to = "Age group", values_to = "RSE") %>%
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")))
  val_data %>%
    left_join(rse_data, by = c("Sex", "Macronutrient", "Age group")) %>%
    mutate(lowerCI = round(val - (1.96 * RSE / 100 * val), 1),
           upperCI = round(val + (1.96 * RSE / 100 * val), 1)) %>%
    select(-RSE)
}

process_nutrient_table3 <- function(dcpath, filename, sheet_nums, est_names, rse_names, nut_class, age_cols) {
  sexes <- c("Persons", "Males", "Females")
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
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")))
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
    mutate(`Age group` = factor(`Age group`, levels = age_cols),
           Sex = factor(Sex, levels = c("Males", "Females", "Persons")))
  est_data %>%
    left_join(rse_data, by = c("Sex", "Nutrient", "Age group")) %>%
    mutate(lowerCI = round(val - (1.96 * RSE * val / 100), 1),
           upperCI = round(val + (1.96 * RSE * val / 100), 1)) %>%
    select(-RSE)
}

process_AUSNUT_tables <- function(filename, sheet_nums, names_vec, ausnut_df, type_label, unit_label) {
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
  main_data %>%
    left_join(rse_data, by = c("Sex", "full_code", "Age group")) %>%
    mutate(lowerCI = round(val - (1.96 * RSE / 100 * val), 1),
           upperCI = round(val + (1.96 * RSE / 100 * val), 1)) %>%
    select(-RSE)
}

process_AUSNUT_table7 <- function(filename, sheet_nums, names_vec = AUSNUT_names, ausnut_df = AUSNUT_class,
                                   range = "A7:L155",
                                   type_labels = c(mean = "Mean kJ", pct = "Percent kJ"),
                                   unit_labels  = c(mean = "kJ", pct = "%")) {
  need <- c("mean_persons","rse_persons","pct_persons","moe_persons",
            "mean_males","rse_males","pct_males","moe_males",
            "mean_females","rse_females","pct_females","moe_females")
  if (!is.null(names(sheet_nums)) && all(need %in% names(sheet_nums))) {
    sheet_vec <- sheet_nums[need]
  } else {
    stopifnot(length(sheet_nums) == 12); names(sheet_nums) <- need; sheet_vec <- sheet_nums
  }
  age_cols_t7 <- names_vec[names_vec != "Label"]
  read_block <- function(sheets3, sexes3, value_col_name = "val") {
    purrr::map2(sheets3, sexes3, ~
      read_dc_excel(dcpath, filename, sheet = .x, range = range) %>%
      setNames(names_vec) %>%
      dplyr::left_join(ausnut_df, by = "Label") %>%
      dplyr::mutate(Sex = .y)
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(Sex, Class_level, maj_code, sub_code, full_code, Label, dplyr::everything()) %>%
      tidyr::pivot_longer(dplyr::all_of(age_cols_t7), names_to = "Age group", values_to = value_col_name) %>%
      dplyr::mutate({{value_col_name}} := dplyr::case_when(
        .data[[value_col_name]] %in% c("—", "np") ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(.data[[value_col_name]]))))
  }
  sexes <- c("Persons","Males","Females")
  mean_df <- read_block(unname(sheet_vec[c("mean_persons","mean_males","mean_females")]), sexes, "val") %>%
    dplyr::mutate(Type = type_labels["mean"], Unit = unit_labels["mean"]) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val)
  pct_df  <- read_block(unname(sheet_vec[c("pct_persons","pct_males","pct_females")]), sexes, "val") %>%
    dplyr::mutate(Type = type_labels["pct"],  Unit = unit_labels["pct"]) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val)
  rse_df  <- read_block(unname(sheet_vec[c("rse_persons","rse_males","rse_females")]), sexes, "RSE") %>%
    dplyr::select(Sex, full_code, `Age group`, RSE)
  moe_df  <- read_block(unname(sheet_vec[c("moe_persons","moe_males","moe_females")]), sexes, "MoE") %>%
    dplyr::select(Sex, full_code, `Age group`, MoE)
  mean_out <- mean_df %>%
    dplyr::left_join(rse_df, by = c("Sex","full_code","Age group")) %>%
    dplyr::mutate(lowerCI = round(val - 1.96*(val*RSE/100), 1),
                  upperCI = round(val + 1.96*(val*RSE/100), 1)) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val, lowerCI, upperCI)
  pct_out <- pct_df %>%
    dplyr::left_join(moe_df, by = c("Sex","full_code","Age group")) %>%
    dplyr::mutate(lowerCI = round(val - MoE, 1), upperCI = round(val + MoE, 1)) %>%
    dplyr::select(Sex, Type, Class_level, maj_code, sub_code, full_code, Label, Unit, `Age group`, val, lowerCI, upperCI)
  dplyr::bind_rows(mean_out, pct_out)
}

# ---------- column name vectors ----------
T1_val_names <- c("Nutrient","Unit","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
T1_rse_names <- c("Nutrient","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
T2_names     <- c("Macronutrient","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
T3_est_names <- c("Nutrient","Unit","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
T3_rse_names <- c("Nutrient","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")
AUSNUT_names <- c("Label","02-04","05-11","12-17","18-29","30-49","50-64","65-74","75+","02-17","18+","Total")

# ---------- download and process ----------
message("Processing Table 1 (2023)...")
Table1_2023 <- process_nutrient_table(dcpath, "NNPASDC01.xlsx",  c(2,4,6,3,5,7), T1_val_names, T1_rse_names, Nut_class, age_cols) %>% mutate(Year = "2023")
message("Processing Table 1 (2011-12)...")
Table1_2011 <- process_nutrient_table(dcpath, "NNPASTSS01.xlsx", c(2,4,6,3,5,7), T1_val_names, T1_rse_names, Nut_class, age_cols) %>% mutate(Year = "2011-12")
Table1 <- bind_rows(Table1_2023, Table1_2011)

message("Processing Table 2 (2023)...")
Table2_2023 <- process_macronutrient_table(dcpath, "NNPASDC02.xlsx",  c(2,4,6,3,5,7), T2_names, age_cols) %>% mutate(Year = "2023")
message("Processing Table 2 (2011-12)...")
Table2_2011 <- process_macronutrient_table(dcpath, "NNPASTSS02.xlsx", c(2,4,6,3,5,7), T2_names, age_cols) %>% mutate(Year = "2011-12")
Table2 <- bind_rows(Table2_2023, Table2_2011)

message("Processing Table 3 (2023)...")
Table3_2023 <- process_nutrient_table3(dcpath, "NNPASDC03.xlsx",  c(2,4,6,3,5,7), T3_est_names, T3_rse_names, Nut_class, age_cols) %>% mutate(Year = "2023")
message("Processing Table 3 (2011-12)...")
Table3_2011 <- process_nutrient_table3(dcpath, "NNPASTSS03.xlsx", c(2,4,6,3,5,7), T3_est_names, T3_rse_names, Nut_class, age_cols) %>% mutate(Year = "2011-12")
sex_order <- c("Persons","Males","Females"); year_order <- c("2011-12","2023")
dummy_df <- expand.grid(Sex = sex_order, Year = year_order, `Age group` = age_cols) %>%
  mutate(Type="Energy", Type_unit="per 1,000 kJ", Order=1, Nutrient="Energy", Unit="kJ",
         val=1000, lowerCI=1000, upperCI=1000,
         Sex = factor(Sex, levels=sex_order), Year = factor(Year, levels=year_order),
         `Age group` = factor(`Age group`, levels=age_cols)) %>%
  arrange(Year, Sex, `Age group`)
Table3 <- bind_rows(Table3_2023, Table3_2011, dummy_df)

message("Processing AUSNUT Table 4...")
Table4 <- process_AUSNUT_tables("NNPASDC04.xlsx", c(2,4,6,3,5,7), AUSNUT_names, AUSNUT_class, "Percent consumed", "Percent")
message("Processing AUSNUT Table 5...")
Table5 <- process_AUSNUT_tables("NNPASDC05.xlsx", c(2,4,6,3,5,7), AUSNUT_names, AUSNUT_class, "Mean grams", "g")
message("Processing AUSNUT Table 6...")
Table6 <- process_AUSNUT_tables("NNPASDC06.xlsx", c(2,4,6,3,5,7), AUSNUT_names, AUSNUT_class, "Median grams", "g")
message("Processing AUSNUT Table 7...")
Table7 <- process_AUSNUT_table7("NNPASDC07.xlsx",
  c(mean_persons=2,rse_persons=3,pct_persons=4,moe_persons=5,
    mean_males=6,rse_males=7,pct_males=8,moe_males=9,
    mean_females=10,rse_females=11,pct_females=12,moe_females=13),
  AUSNUT_names, AUSNUT_class)
message("Processing AUSNUT Table 8...")
Table8 <- process_AUSNUT_tables("NNPASDC08.xlsx", c(2,4,6,3,5,7), AUSNUT_names, AUSNUT_class, "Disc energy", "Percent")

message("Building final tables...")
Nutrients_tab <- bind_rows(Table1, Table3)
Macro_kJ_tab  <- Table2

AUSNUT_tab01 <- bind_rows(Table4, Table5, Table6, Table7, Table8) %>%
  mutate(cLabel = case_when(Class_level == "Major"     ~ paste0(maj_code,  ", ", Label),
                            Class_level == "Sub-major" ~ paste0(full_code, ", ", Label),
                            TRUE ~ NA_character_),
         Sex = factor(Sex, levels = c("Males","Females","Persons")))

majmin <- AUSNUT_tab01 %>%
  filter(Class_level == "Sub-major") %>%
  mutate(Class_level = "MajMin", cLabel = NA)

AUSNUT_tab02 <- bind_rows(AUSNUT_tab01, majmin)

two_dig_label <- AUSNUT_tab02 %>%
  select(maj_code, Label) %>%
  mutate(submajCode = paste0(maj_code, ", ", Label)) %>%
  select(-Label) %>%
  rename(Codec = maj_code) %>%
  distinct() %>%
  filter(!grepl("Total persons", submajCode))

AUSNUT_tab <- AUSNUT_tab02 %>%
  left_join(two_dig_label, by = c("maj_code" = "Codec")) %>%
  mutate(submajCode = case_when(!is.na(cLabel) ~ NA_character_, TRUE ~ submajCode)) %>%
  distinct()

Two_dig <- AUSNUT_tab %>%
  filter(Class_level == "Major") %>%
  select(maj_code, Label, cLabel) %>%
  filter(cLabel != "25, Total persons ('000)") %>%
  distinct() %>%
  group_by(cLabel) %>% tally()
Two_dig <- as.list(as.character(Two_dig$cLabel))

Thr_dig <- AUSNUT_tab %>%
  filter(Class_level == "Sub-major") %>%
  select(sub_code, Label, cLabel) %>%
  distinct() %>%
  group_by(cLabel) %>% tally()
Thr_dig <- as.list(as.character(Thr_dig$cLabel))

message("Saving app_data.rds...")
saveRDS(
  list(
    AUSNUT_tab    = AUSNUT_tab,
    Nutrients_tab = Nutrients_tab,
    Macro_kJ_tab  = Macro_kJ_tab,
    Two_dig       = Two_dig,
    Thr_dig       = Thr_dig
  ),
  "app_data.rds"
)
message("Done! app_data.rds written.")
