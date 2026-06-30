
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
 
# style_plot(): theme a highcharter chart for comfortable on-screen WORK
# (dark, blends into the darkly app) while exports are forced to a clean
# WHITE background for slides / Word / PDF. Apply it LAST in a
# renderHighchart() chain, e.g.
#   hc %>% apply_font_styles() %>% style_plot(filename = "my_chart")
#
#   - dark = TRUE  (default): transparent bg + light text on screen.
#     dark = FALSE           : white bg + dark text on screen.
#   - The DOWNLOAD is ALWAYS white with dark text (exporting$chartOptions),
#     whatever the on-screen theme -- so published output never needs toggling.
#   - font STACK ending in a generic sans-serif, so exported SVGs never fall
#     back to serif (Highcharts names the font but does not embed it).
#   - SVG / PNG / PDF export menu rendered CLIENT-SIDE (offline-exporting),
#     so data is never posted to Highsoft's public export server.
#   - the in-chart title is only RE-coloured when one already exists, so
#     title-less charts don't get Highcharts' default "Chart title".
 
style_plot <- function(hc, filename = "GBI_chart", dark = TRUE) {
 
  pal <- if (dark)
    list(bg = "transparent", ink = "#E6EDF3", lab = "#C9D1D9",
         grid = "#30363D", axis = "#8B949E", err = "#C9D1D9")
  else
    list(bg = "#FFFFFF", ink = "#0D1117", lab = "#333333",
         grid = "#E6E6E6", axis = "#888888", err = "#333333")
 
  # Only style a title/subtitle if the chart actually set one (prevents the
  # default "Chart title" appearing on title-less charts).
  has_title    <- !is.null(hc$x$hc_opts$title$text)    && nzchar(hc$x$hc_opts$title$text)
  has_subtitle <- !is.null(hc$x$hc_opts$subtitle$text) && nzchar(hc$x$hc_opts$subtitle$text)
 
  # ---- on-screen (working) theme ----
  hc <- hc %>%
    hc_chart(backgroundColor = pal$bg, plotBackgroundColor = pal$bg,
             style = list(fontFamily = "Open Sans, Arial, Helvetica, sans-serif")) %>%
    hc_xAxis(lineColor = pal$axis, tickColor = pal$axis,
             title  = list(style = list(color = pal$ink)),
             labels = list(style = list(color = pal$lab))) %>%
    hc_yAxis(gridLineColor = pal$grid, lineColor = pal$axis, tickColor = pal$axis,
             title  = list(style = list(color = pal$ink)),
             labels = list(style = list(color = pal$lab))) %>%
    hc_legend(itemStyle = list(color = pal$ink)) %>%
    hc_plotOptions(
      series   = list(dataLabels = list(style = list(color = pal$ink, textOutline = "none"))),
      errorbar = list(color = pal$err, whiskerLength = "30%", stemWidth = 1.5))
  if (has_title) {
    hc <- hc %>% hc_title(style = list(color = pal$ink))
  } else {
    # No title set: blank it so Highcharts' default "Chart title" never shows
    # (in the app OR in the white export).
    hc <- hc %>% hc_title(text = "")
  }
  if (has_subtitle) hc <- hc %>% hc_subtitle(style = list(color = pal$lab))
 
  # ---- export overrides: ALWAYS publication-light, applied only on download ----
  exp <- list(
    chart  = list(backgroundColor = "#FFFFFF", plotBackgroundColor = "#FFFFFF"),
    xAxis  = list(lineColor = "#888888", tickColor = "#888888",
                  title  = list(style = list(color = "#0D1117")),
                  labels = list(style = list(color = "#333333"))),
    yAxis  = list(gridLineColor = "#E6E6E6", lineColor = "#888888", tickColor = "#888888",
                  title  = list(style = list(color = "#0D1117")),
                  labels = list(style = list(color = "#333333"))),
    legend = list(itemStyle = list(color = "#0D1117")),
    plotOptions = list(
      series   = list(dataLabels = list(style = list(color = "#0D1117", textOutline = "none"))),
      errorbar = list(color = "#333333"))
  )
  if (has_title)    exp$title    <- list(style = list(color = "#0D1117"))
  if (has_subtitle) exp$subtitle <- list(style = list(color = "#333333"))
 
  hc %>%
    hc_add_dependency("modules/exporting.js") %>%
    hc_add_dependency("modules/offline-exporting.js") %>%
    hc_exporting(enabled = TRUE, fallbackToExportServer = FALSE,
                 filename = filename, sourceWidth = 1000, sourceHeight = 560, scale = 3,
                 chartOptions = exp,
                 buttons = list(contextButton = list(
                   menuItems = c("downloadSVG", "downloadPNG", "downloadPDF",
                                 "separator", "viewFullscreen"))))
}
 
