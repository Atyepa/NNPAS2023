library(tidyverse)  
library(writexl)
library(highcharter)  
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readxl)
options(warn=-1)
options(shiny.launch.browser = FALSE)

# Source the custom styles function
source("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/custom_styles.R")

# Source the cleaning functions
source("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/cleaning_fun.R")


# ---- Data: load pre-baked RDS (fast) or abort with a clear message ----
# To regenerate app_data.rds after ABS publishes updated data:
#   1. Run prep_data.R locally
#   2. Redeploy to shinyapps.io
if (!file.exists("app_data.rds")) {
  stop("app_data.rds not found. Run prep_data.R locally to generate it, then redeploy.")
}
.d            <- readRDS("app_data.rds")
AUSNUT_tab    <- .d$AUSNUT_tab
Nutrients_tab <- .d$Nutrients_tab
Macro_kJ_tab  <- .d$Macro_kJ_tab
Two_dig       <- .d$Two_dig
Thr_dig       <- .d$Thr_dig
rm(.d)


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
    shinyWidgets::pickerInput("choosetable", "Select table:",
                 choices = c("AUSNUT foodgroups" = "AUSNUT",
                             "Nutrients" = "Nutrients",
                             "Macronutrient kJ" = "Macro"),
                 selected = "AUSNUT", 
                 multiple = FALSE, options = list(`actions-box` = TRUE), width = "300px"),

    conditionalPanel(
      condition = "input.choosetable == 'AUSNUT'",
      
      shinyWidgets::pickerInput("Class1", "Classification level:", 
                   choices = c("Major", "Sub-major", "Sub-major within Major" = "MajMin"), 
                   selected = "Major", 
                   multiple = FALSE,
          options  = list(`actions-box` = TRUE), width = "300px"
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'Major'",
        pickerInput("Majgrp1", "AUSNUT major food groups:", choices = Two_dig, 
                    selected = "01, Non-alcoholic beverages", multiple = TRUE, 
                    options = list(`actions-box` = TRUE), width = "300px")
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'Sub-major'",
        pickerInput("Mingrp1", "AUSNUT sub-major food groups:", choices = Thr_dig,
                    selected = "0101, Tea", multiple = TRUE, 
                    options = list(`actions-box` = TRUE), width = "300px")
      ),
      
      conditionalPanel(
        condition = "input.Class1 == 'MajMin'",
        pickerInput("MajMin", "Components of major food groups:", choices = Two_dig,
                    selected = "01, Non-alcoholic beverages",
                    multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"),
        pickerInput("MajMin_sub", "Sub-major foods within selection:", choices = NULL,
                    multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px")
      ),
      
      selectInput("A_Nutrient", "Estimate:", 
                   choices = c("% consumers" = "Percent consumed",
                               "Mean grams" = "Mean grams", 
                               "Median grams" = "Median grams", 
                               "Mean kJ" = "Mean kJ",
                               "% Total energy" = "Percent kJ",
                               "% Discretionary kJ" = "Disc energy"), 
                  selected = "Mean grams", multiple = FALSE,
                  width = "180px"),
      checkboxInput("AUSNstack", "Stack bars", value = FALSE),
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
                  options = list(`actions-box` = TRUE), width = "300px")
    ),
    
    conditionalPanel(
      condition = "input.choosetable == 'Macro'",
      
      checkboxGroupInput("Year_macro", "Year:",
                         choices = c("2011-12", "2023" ), selected = "2011-12", inline = TRUE),
      
      pickerInput("MacrokJ", "Select macronutrients:",
                  choices = Macro,
                  selected = c("Protein", "Total fat", "Carbohydrate", "Dietary fibre", "Alcohol"),
                  multiple = TRUE, options = list(`actions-box` = TRUE), width = "300px"),
      checkboxInput("Macrostack", "Stack bars", value = TRUE),
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

    tags$div(
      style = "margin-top: 8px;",
      tags$button(
        id = "swap_group",
        type = "button",
        class = "btn btn-default btn-sm",
        style = "width: 300px;",
        "Swap x-axis / series group"
      ),
      tags$script(HTML("
(function() {
  var swapped = false;
  var btn = document.getElementById('swap_group');
  function sendVal() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('swap_group', swapped ? 'swapped' : 'normal', { priority: 'event' });
    }
  }
  function updateAppearance() {
    if (!btn) return;
    if (swapped) {
      btn.classList.add('active');
      btn.setAttribute('aria-pressed', 'true');
      btn.style.fontWeight = 'bold';
    } else {
      btn.classList.remove('active');
      btn.setAttribute('aria-pressed', 'false');
      btn.style.fontWeight = '';
    }
  }
  document.addEventListener('shiny:connected', function() {
    sendVal();
    updateAppearance();
  });
  if (document.readyState !== 'loading' && window.Shiny && Shiny.shinyapp) {
    sendVal();
    updateAppearance();
  }
  if (btn) {
    btn.addEventListener('click', function() {
      swapped = !swapped;
      sendVal();
      updateAppearance();
    });
  }
})();
"))
    ),
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
        )) %>%
      hc_plotOptions(
        errorbar = list(
          dataLabels = list(enabled = FALSE)
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
  
  # --- Observer: populate sub-major picker when MajMin major group selection changes ---
  observeEvent(input$MajMin, {
    sub_choices <- AUSNUT_tab %>%
      dplyr::filter(Class_level == "MajMin", submajCode %in% input$MajMin) %>%
      dplyr::distinct(Label) %>%
      dplyr::arrange(Label) %>%
      dplyr::pull(Label)
    updatePickerInput(session, "MajMin_sub",
                      choices  = sub_choices,
                      selected = sub_choices)
  }, ignoreNULL = FALSE)

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
                      a_nutrient <- input$A_Nutrient
                      majgrp1    <- input$Majgrp1
                      mingrp1    <- input$Mingrp1
                      majmin     <- input$MajMin
                      majmin_sub <- input$MajMin_sub
                      AUSNUT_tab %>%
                      mutate(Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>%
                      arrange(Sex, `Age group`) %>%
                      filter(Sex %in% Sex()$Sex) %>%
                      filter(`Age group` %in% Agegroup()$Agegroup) %>%
                      filter(Type == a_nutrient) %>%
                      filter(Class_level %in% Class1()$Class1) %>%
                      filter(cLabel %in% majgrp1 | cLabel %in% mingrp1 |
                               (submajCode %in% majmin &
                                  (length(majmin_sub) == 0 | Label %in% majmin_sub)))
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
dt_Ausn <- reactive({
  show_ci <- isTruthy(input$showErrorBars)
  df <- Ausnut_tab_filtered() %>%
    select(Class_level, Label, val, lowerCI, upperCI, Unit, Sex, `Age group`) %>%
    distinct()
  wide_val <- df %>% select(-lowerCI, -upperCI) %>%
    pivot_wider(names_from = `Age group`, values_from = val) %>%
    mutate(.row_type = 1L)
  if (show_ci) {
    wide_ci <- df %>%
      mutate(val = round((upperCI - lowerCI) / 2, 1), Unit = "95% CI (+/-)") %>%
      select(-lowerCI, -upperCI) %>%
      pivot_wider(names_from = `Age group`, values_from = val) %>%
      mutate(.row_type = 2L)
    bind_rows(wide_val, wide_ci) %>%
      arrange(Class_level, Label, Sex, .row_type) %>%
      select(-.row_type)
  } else {
    select(wide_val, -.row_type)
  }
})

dt_Nut <- reactive({
  show_ci <- isTruthy(input$showErrorBars)
  df <- Nutrient_tab_filtered() %>%
    select(Nutrient, Unit, Sex, Year, `Age group`, val, lowerCI, upperCI) %>%
    distinct()
  wide_val <- df %>% select(-lowerCI, -upperCI) %>%
    pivot_wider(1:4, names_from = `Age group`, values_from = val) %>%
    mutate(.row_type = 1L)
  if (show_ci) {
    wide_ci <- df %>%
      mutate(val = round((upperCI - lowerCI) / 2, 1), Unit = "95% CI (+/-)") %>%
      select(-lowerCI, -upperCI) %>%
      pivot_wider(1:4, names_from = `Age group`, values_from = val) %>%
      mutate(.row_type = 2L)
    bind_rows(wide_val, wide_ci) %>%
      arrange(Nutrient, Sex, Year, .row_type) %>%
      select(-.row_type)
  } else {
    select(wide_val, -.row_type)
  }
})

dt_Macro <- reactive({
  show_ci <- isTruthy(input$showErrorBars)
  df <- Macro_tab_filtered() %>%
    select(Macronutrient, Unit, Sex, Year, `Age group`, val, lowerCI, upperCI) %>%
    distinct()
  wide_val <- df %>% select(-lowerCI, -upperCI) %>%
    pivot_wider(1:4, names_from = `Age group`, values_from = val) %>%
    mutate(.row_type = 1L)
  if (show_ci) {
    wide_ci <- df %>%
      mutate(val = round((upperCI - lowerCI) / 2, 1), Unit = "95% CI (+/-)") %>%
      select(-lowerCI, -upperCI) %>%
      pivot_wider(1:4, names_from = `Age group`, values_from = val) %>%
      mutate(.row_type = 2L)
    bind_rows(wide_val, wide_ci) %>%
      arrange(Macronutrient, Sex, Year, .row_type) %>%
      select(-.row_type)
  } else {
    select(wide_val, -.row_type)
  }
})
  

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
  
  show_err    <- isTruthy(input$showErrorBars)
  is_swapped  <- isTRUE(input$swap_group == "swapped")
  ausn_stack  <- isTRUE(input$AUSNstack)
  macro_stack <- isTRUE(input$Macrostack)
  ausn_show_err  <- show_err && !ausn_stack
  macro_show_err <- show_err && !macro_stack

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
        dplyr::transmute(x = x, y = val, name = .cat) %>%
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
    hc <- hc %>%
      hc_tooltip(headerFormat = '<span style="font-size: 10px">{point.name}</span><br/>')
    hc
  }
  
  # Special case of stacked column (or bar):
  add_stacked_columns_with_errorbars <- function(
    hc, df, xvar, gvar,
    categories = NULL,
    group_padding = 0.2,
    point_padding = 0.1,
    show_errorbars = TRUE,
    series_type = "column"
  ) {
    if (is.null(categories)) {
      categories <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull()
    }

    stack_opts <- list(stacking = "normal",
                       groupPadding = group_padding,
                       pointPadding = point_padding)
    err_opts   <- list(pointRange   = 1 - 2 * group_padding,
                       pointPadding = point_padding)

    hc <- hc %>%
      hc_xAxis(categories = categories) %>%
      hc_plotOptions(
        column   = stack_opts,
        bar      = stack_opts,
        errorbar = err_opts
      )

    groups <- df %>% dplyr::distinct(!!gvar) %>% dplyr::pull()
    # Cumulative y-base per category so error bars sit at the segment top
    cum_base <- setNames(rep(0, length(categories)), as.character(categories))

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
          type = series_type,
          data = yvec
          # stacking comes from plotOptions
        )

      if (isTRUE(show_errorbars)) {
        err <- lapply(categories, function(cat) {
          row  <- dfg %>% dplyr::filter(!!xvar == cat)
          base <- cum_base[[as.character(cat)]]
          if (nrow(row) == 1 &&
              !is.na(row$lowerCI[[1]]) && !is.na(row$upperCI[[1]])) {
            list(low  = row$lowerCI[[1]] + base,
                 high = row$upperCI[[1]] + base)
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

      # Advance cumulative base by this group's values
      for (cat in as.character(categories)) {
        row <- dfg %>% dplyr::filter(!!xvar == cat)
        if (nrow(row) == 1 && !is.na(row$val[[1]])) {
          cum_base[[cat]] <- cum_base[[cat]] + row$val[[1]]
        }
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

    x_col   <- if (is_swapped && !ausn_stack) groupby() else x_axis()
    grp_col <- if (is_swapped && !ausn_stack) x_axis() else groupby()
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% distinct(!!xvar) %>% pull()

    hc_base <- build_base_chart(
      series_type  = "bar",
      x_title      = x_col,
      y_title      = paste0(U()$unit),
      title_text   = paste0(input$A_Nutrient, ", selected foods, 2023"),
      value_suffix = U()$unit
    )
    hc <- if (ausn_stack)
      add_stacked_columns_with_errorbars(hc_base, df, xvar, gvar,
                                         categories = cats,
                                         show_errorbars = ausn_show_err, series_type = "bar")
    else
      add_grouped_bars_with_errorbars(hc_base, df, xvar, gvar, series_type = "bar",
                                      categories = cats, show_errorbars = ausn_show_err)
  }
  
  # CASE 2: AUSNUT, >1 age, 1 sex, small selection -> vertical columns (x = Age group, group = Label)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) > 1 &&
           length(sex_vals) < 2 &&
           ((length(maj_vals) >= 2 && length(maj_vals) <= 4) ||
            (length(min_vals) >= 2 && length(min_vals) <= 4))) {
    
    df <- Ausnut_tab_filtered()
    x_col   <- if (is_swapped && !ausn_stack) "Label" else "Age group"
    grp_col <- if (is_swapped && !ausn_stack) "Age group" else "Label"
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% distinct(!!xvar) %>% pull()

    hc_base <- build_base_chart(
      series_type  = "column",
      x_title      = x_col,
      y_title      = paste0(U()$unit),
      title_text   = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    )
    hc <- if (ausn_stack)
      add_stacked_columns_with_errorbars(hc_base, df, xvar, gvar,
                                         categories = cats, show_errorbars = ausn_show_err)
    else
      add_grouped_bars_with_errorbars(hc_base, df, xvar, gvar, series_type = "column",
                                      categories = cats, show_errorbars = ausn_show_err)
  }

  # CASE 3: AUSNUT, general columns (dynamic x & group)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) > 1 &&
           ((length(maj_vals) >=1 && length(maj_vals) <= 4) || (length(min_vals)>=1 && length(min_vals) <= 4))) {
    
    df   <- Ausnut_tab_filtered()
    x_col   <- if (is_swapped && !ausn_stack) groupby() else x_axis()
    grp_col <- if (is_swapped && !ausn_stack) x_axis() else groupby()
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull() %>% as.character()

    hc_base <- build_base_chart(
      series_type  = "column",
      x_title      = x_col,
      y_title      = paste0(U()$unit),
      title_text   = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    )
    hc <- if (ausn_stack)
      add_stacked_columns_with_errorbars(hc_base, df, xvar, gvar,
                                         categories = cats, show_errorbars = ausn_show_err)
    else
      add_grouped_bars_with_errorbars(hc_base, df, xvar, gvar, series_type = "column",
                                      categories = cats, show_errorbars = ausn_show_err)
    }


  # CASE 3.1: AUSNUT, general columns (dynamic x & group)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) <= 1 &&
           (length(maj_vals) <=1 || length(min_vals) <=1 )) {
    
    df <- Ausnut_tab_filtered()
    x_col   <- if (is_swapped && !ausn_stack) groupby() else x_axis()
    grp_col <- if (is_swapped && !ausn_stack) x_axis() else groupby()

    hc <- df %>%
      hchart(.,
             type = "column",
             hcaes(x = !!sym(x_col),
                   y = val,
                   group = !!sym(grp_col))) %>%
      hc_xAxis(title = list(text = x_col)) %>%
      hc_yAxis(title = list(text = paste0(U()$unit))) %>%
      hc_title(text = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023")) %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE, valueSuffix = paste0(" ", U()$unit)) %>%
      hc_plotOptions(column = list(stacking = if (ausn_stack) "normal" else NULL)) %>%
      apply_font_styles(input$showDataLabels)

    }
  
  
  # CASE 4: AUSNUT, single age, multiple sexes -> vertical columns (dynamic)
  else if (input$choosetable == "AUSNUT" &&
           length(age_vals) == 1 &&
           length(sex_vals) >= 1 &&
           (length(maj_vals) >= 1 || length(min_vals) >= 1)) {
    
    df   <- Ausnut_tab_filtered()
    x_col   <- if (is_swapped && !ausn_stack) groupby() else x_axis()
    grp_col <- if (is_swapped && !ausn_stack) x_axis() else groupby()
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% distinct(!!xvar) %>% pull()

    hc_base <- build_base_chart(
      series_type  = "column",
      x_title      = x_col,
      y_title      = paste0(U()$unit),
      title_text   = paste0(paste(Label(), collapse = ", "), ", ", input$A_Nutrient, ", 2023"),
      value_suffix = U()$unit
    )
    hc <- if (ausn_stack)
      add_stacked_columns_with_errorbars(hc_base, df, xvar, gvar,
                                         categories = cats, show_errorbars = ausn_show_err)
    else
      add_grouped_bars_with_errorbars(hc_base, df, xvar, gvar, series_type = "column",
                                      categories = cats, show_errorbars = ausn_show_err)
  }

  # CASE 5: Nutrients table
  else if (input$choosetable == "Nutrients") {
    
    df   <- Nutrient_tab_filtered()
    x_col   <- if (is_swapped) groupby_nut() else x_axis_nut()
    grp_col <- if (is_swapped) x_axis_nut() else groupby_nut()
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% distinct(!!xvar) %>% pull()

    hc <- build_base_chart(
      series_type = "column",
      x_title     = x_col,
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
    
    x_col   <- if (is_swapped && !macro_stack) groupby_macro() else x_axis_macro()
    grp_col <- if (is_swapped && !macro_stack) x_axis_macro() else groupby_macro()
    xvar <- rlang::sym(x_col)
    gvar <- rlang::sym(grp_col)
    cats <- df %>% dplyr::distinct(!!xvar) %>% dplyr::pull()
    hc_base <- build_base_chart(
      series_type  = "column",
      x_title      = x_col,
      y_title      = paste0(Um()$unit),
      title_text   = paste0("Percent dietary energy from selected macronutrients, ",
                            format_years(macro_year())),
      value_suffix = Um()$unit
    )
    hc <- if (macro_stack)
      add_stacked_columns_with_errorbars(hc_base, df, xvar, gvar,
                                         categories    = cats,
                                         group_padding = 0.2,
                                         point_padding = 0.1,
                                         show_errorbars = macro_show_err)
    else
      add_grouped_bars_with_errorbars(hc_base, df, xvar, gvar, series_type = "column",
                                      categories = cats, show_errorbars = macro_show_err)
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
