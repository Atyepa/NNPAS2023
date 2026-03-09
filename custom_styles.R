# custom_styles.R
# This file defines functions commonly used in custom Shiny app
# It primarily holds functions to customise the appearance of the app
# but it also defines a few utility functions

#----------
# Not in
#----------
`%!in%` <- negate(`%in%`)

#----------------
# --- UI style--
#----------------
custom_styles <- function() {
  tags$head(tags$style(HTML(
    "
    .dataTables_length label,
    .dataTables_filter label,
    .dataTables_info {
        color: white!important;
    }

    .paginate_button {
        background: white!important;
    }

    thead {
        color: white;
    }

    table.dataTable {
        background-color: white!important;
        color: black!important;
    }

    table.dataTable th,
    table.dataTable td {
        color: black!important;
    }

    table.dataTable thead th {
        background-color: white!important;
        color: black!important;
    }

    table.dataTable thead td {
        background-color: white!important;
        color: black!important;
    }

    .paginate_button,
    .paginate_button:hover,
    .paginate_button:active {
        color: black!important;
        background-color: white!important;
        border-color: black!important;
    }
    "
  )))
}

#----------------------
# --- Chart font style--
#----------------------

# Apply a consistent font & plot style to a highcharter object.
#' @param hc A highcharter object.
#' @param showDataLabels Logical; whether to enable data labels (default FALSE).
#' @return A modified highcharter object.
apply_font_styles <- function(hc, showDataLabels = FALSE) {
  hc %>%
    hc_xAxis(
      title  = list(style = list(fontSize = '18px')),
      labels = list(style = list(fontSize = '16px'))
    ) %>%
    hc_yAxis(
      title  = list(style = list(fontSize = '18px')),
      labels = list(style = list(fontSize = '16px'))
    ) %>%
    hc_legend(itemStyle = list(fontSize = '16px')) %>%
    hc_plotOptions(series = list(
      dataLabels = list(enabled = showDataLabels, style = list(fontSize = '14px')),
      marker     = list(enabled = FALSE)
    ))
}

#----------------------
# --- Colour palette -- 
#----------------------

abscol <- c("#4FADE7","#1A4472","#F29000","#993366","#669966","#99CC66",
            "#CC9966","#666666","#8DD3C7","#BEBADA","#FB8072","#80B1D3",
            "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#ffcc99")
<<<<<<< HEAD
=======


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


#---------------------------
# Update labels from a df
#---------------------------

# Requires: dplyr, stringr, rlang
apply_labels <- function(df, lookup, by, descr_col = NULL, keep_empty = FALSE) {
  stopifnot(is.data.frame(df), is.data.frame(lookup))
  
  # Validate `by`: named character vector of length 1
  if (!is.character(by) || length(by) != 1L || is.null(names(by))) {
    stop("`by` must be a named character vector of length 1, e.g. c('code' = 'AUSNUT_code').")
  }
  
  lookup_key <- names(by)[[1]]   # e.g. "code"    (in lookup)
  df_key     <- unname(by)[[1]]  # e.g. "AUSNUT_code" (in df)
  
  # Choose description column to overwrite
  if (is.null(descr_col)) {
    preferred <- c("AUSNUT_descr")
    regex_hits <- grep("(?i)(^descr$|_descr$|descr$)", names(df), value = TRUE, perl = TRUE)
    candidates <- unique(c(preferred[preferred %in% names(df)], regex_hits))
    if (length(candidates) == 0L) {
      stop("Could not infer description column. Please provide `descr_col = '...'`.")
    } else if (length(candidates) > 1L) {
      warning("Multiple possible description columns detected: ",
              paste(candidates, collapse = ", "),
              ". Using '", candidates[[1]], "'. Pass descr_col= to choose explicitly.")
    }
    descr_col <- candidates[[1]]
  }
  
  # Symbols for tidy-eval when we need them
  df_key_sym    <- rlang::sym(df_key)
  lookup_key_sym<- rlang::sym(lookup_key)
  descr_sym     <- rlang::sym(descr_col)
  
  # -- Clean lookup (trim, dedupe on key, keep only key + short_label)
  if (!all(c(lookup_key, "short_label") %in% names(lookup))) {
    stop("`lookup` must contain columns: '", lookup_key, "' (key) and 'short_label'.")
  }
  lookup_clean <- lookup %>%
    dplyr::mutate(!!lookup_key_sym := stringr::str_trim(as.character(!!lookup_key_sym))) %>%
    dplyr::distinct(!!lookup_key_sym, .keep_all = TRUE) %>%
    dplyr::select(!!lookup_key_sym, .data$short_label)
  
  # -- Prepare df (trim keys and ensure descr is character)
  df1 <- df %>%
    dplyr::mutate(
      !!df_key_sym := stringr::str_trim(as.character(!!df_key_sym)),
      !!descr_sym  := as.character(!!descr_sym)
    )
  if (!keep_empty) {
    df1 <- df1 %>%
      dplyr::mutate(!!df_key_sym := dplyr::na_if(!!df_key_sym, ""))
  }
  
  # -- Left join using a simple named mapping and overwrite via coalesce()
  out <- df1 %>%
    dplyr::left_join(lookup_clean, by = setNames(lookup_key, df_key)) %>%
    dplyr::mutate(
      !!descr_sym := dplyr::coalesce(.data$short_label, !!descr_sym)
    ) %>%
    dplyr::select(-.data$short_label)
  
  # Optionally restore "" for codes if requested
  if (keep_empty) {
    out[[df_key]][is.na(out[[df_key]])] <- ""
  }
  
  out
}
>>>>>>> b0b6154 (Add swap/stack controls, CI error bars, MajMin sub-picker, and table CI rows)
