

# custom_styles.R

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