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

#----------------------------------------------------------------------
# --- Slide / report styling + export ---
#----------------------------------------------------------------------

# style_plot(): make any highcharter chart read cleanly on a WHITE
# background (slides / Word / PDF) and add a one-click vector export.
# Apply it LAST in a renderHighchart() chain, e.g.
#   hc %>% apply_font_styles() %>% style_plot(filename = "my_chart")
#
#   - white background, dark axis / title / legend text, faint gridlines
#   - font STACK ending in a generic sans-serif, so exported SVGs never
#     fall back to serif (Highcharts names the font but does not embed it;
#     Arial is always present on Office, sans-serif is the final safety net)
#   - SVG / PNG / PDF export menu rendered CLIENT-SIDE (offline-exporting),
#     so data is never posted to Highsoft's public export server.

abs_ink   <- "#0D1117"   # near-black: titles, value labels
abs_label <- "#333333"   # dark grey: axis tick labels
abs_grid  <- "#E6E6E6"   # faint gridlines
abs_axis  <- "#888888"   # axis / tick lines

# Sanitise a chart title into a safe export filename.
clean_fname <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

#' Slide-ready styling + SVG/PNG/PDF export for a highcharter object.
#' @param hc A highcharter object.
#' @param filename Export filename (no extension).
#' @return A modified highcharter object.
style_plot <- function(hc, filename = "GBI_chart") {
  hc %>%
    hc_chart(backgroundColor = "#FFFFFF", plotBackgroundColor = "#FFFFFF",
             style = list(fontFamily = "Open Sans, Arial, Helvetica, sans-serif")) %>%
    hc_xAxis(lineColor = abs_axis, tickColor = abs_axis,
             title  = list(style = list(color = abs_ink)),
             labels = list(style = list(color = abs_label))) %>%
    hc_yAxis(gridLineColor = abs_grid, lineColor = abs_axis, tickColor = abs_axis,
             title  = list(style = list(color = abs_ink)),
             labels = list(style = list(color = abs_label))) %>%
    hc_legend(itemStyle = list(color = abs_ink)) %>%
    hc_title(style = list(color = abs_ink)) %>%
    hc_subtitle(style = list(color = abs_label)) %>%
    hc_add_dependency("modules/exporting.js") %>%
    hc_add_dependency("modules/offline-exporting.js") %>%
    hc_exporting(enabled = TRUE, fallbackToExportServer = FALSE,
                 filename = filename,
                 sourceWidth = 1000, sourceHeight = 560, scale = 3,
                 buttons = list(contextButton = list(
                   menuItems = c("downloadSVG", "downloadPNG", "downloadPDF",
                                 "separator", "viewFullscreen"))))
}
