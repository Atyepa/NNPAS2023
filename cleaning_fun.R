# cleaning_fun.R
# This file defines functions commonly used to clean numeric data
# Also loads useful utilities such as read_xlsx_from_url 

#-----------------------------------------------
# Function for reading in from an Excel data cube
#-----------------------------------------------
read_dc_excel <- function(dcpath, filename, sheet, range) {
  url <- paste0(dcpath, "/", filename)
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, destfile = temp_file, mode = "wb")
  readxl::read_excel(temp_file, sheet = sheet, range = range)
}
# Usage:
# Example: Reading a specific range from a data‑cube Excel file:
    # dcpath = URL where the data cube files are hosted:  
#e.g.
   # dcpath= "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022"
   # filename = "NHSDC01.xlsx"
   # sheet = 2
   # range = c(A4:F95)   

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

# Usage:
# Remove footnote markers such as (a) or [b] from the 'region' column
# clean_df <- strip_brace(df, "region")

#---------------------------------------------
# Function to clean text from numeric columns:
#---------------------------------------------
# Convert selected columns to numeric, turning random symbols/chars into NA
force_numeric <- function(df, target_cols) {
  df %>%
    mutate(across(all_of(target_cols), ~ {
      suppressWarnings(as.numeric(ifelse(. %in% c("n.p", "-", "—", "np"), NA, .)))
    }))
}
# Usage e.g. 
# Clean out the non-numeric guff from columns "value" & "rate" 
# clean_df <- force_numeric(df, c("value", "rate"))

#--------------------------------------
#--- Function to round numeric columns:
#--------------------------------------
roundit <- function(df, places = 2) {
  df %>%
    mutate(across(where(is.numeric), ~ round(.x, places)))
}
# Usage e.g. 
# Round numeric cols to 1 place:
# rounded_dat <- unrounded %>%
# roundit(1)


#----------
# Not in
#----------
`%!in%` <- negate(`%in%`)

#------------------------------
# Read in xlsx from URL to a df 
#------------------------------
read_xlsx_from_url <- function(url, sheet = NULL, range = NULL, col_types = NULL, .quiet = TRUE) {
  stopifnot(is.character(url), length(url) == 1)
  tf <- tempfile(fileext = ".xlsx")
  on.exit(try(unlink(tf), silent = TRUE), add = TRUE)

  utils::download.file(url, destfile = tf, mode = "wb", quiet = .quiet)
  readxl::read_xlsx(path = tf, sheet = sheet, range = range, col_types = col_types)
}


