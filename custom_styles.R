# custom_styles.R
# This file defines functions commonly used in custom Shiny app
# It primarily holds functions to customise the appearance of the app
# but it also defines a few utility functions

#----------
# Not in
#----------
`%!in%` <- Negate(`%in%`)

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
