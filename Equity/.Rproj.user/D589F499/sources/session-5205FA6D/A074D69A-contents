#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
#devtools::install_github("rstudio/gridlayout")
library(gridlayout)
library(DT)
library(sp) # sp::spDists()
library(measurements)
library(lubridate)
library(readxl)
library(janitor)
library(purrr)
library(DataExplorer)
library(conflicted)
#devtools::install_github("UTCoAssessors/Valuation",dependencies = TRUE)
library(Valuation)
library(httr)
library(geojsonsf)
library(sf)
library(leaflet)
library(rvest)
library(openxlsx)
library(furrr)
library(future)
library(rjson)


source("./potential_comps.R")


ui <- grid_container(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  layout = c(
    "sidebar   query_table    ",
    "potential_comps potential_comps"
  ),
  row_sizes = c(
    "300px",
    "460px"
  ),
  col_sizes = c(
    "300px",
    "1fr"
  ),
  gap_size = "10px",
  grid_card(
    area = "sidebar",
    scrollable = T,
    collapsible = T,
    item_alignment = "top",
    title = "Query",
    textInput(
      inputId = "ID",
      label = "Parcel ID (Serial Number)",
      value = "",
      width = "100%",
      placeholder = "000000000"
    ), br(), 
    numericInput(
      inputId = "RADIUS",
      label = "Search radius (in miles)",
      value = 1L,
      step = 1L,
      width = "100%"
    ), br(),
    selectInput(
      inputId = 'grad',
      label = 'gradient',
      choices = c('YEAR_BUILT', 'QUALITY', 'adjusted_prediction_cap', 'psf', "tot_val_2022", 'pct_change'),
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = '100%',
      size = NULL
    )

  ),
  grid_card(scrollable = FALSE,has_border = TRUE,
    area = "query_table",
    title = "Query property info",
    item_gap = "10px",
    DT::dataTableOutput(
      outputId = "query",
      height = "auto"
    )
  ),
grid_card(scrollable =FALSE,has_border = TRUE,
          area = "potential_comps",
          title = "Select the parcels to use (ranked)",
          item_gap = "10px",
          navbarPage('Options',
                     tabPanel('Map Results',
                              leafletOutput(outputId = 'MapResults',
                                            width="800px",
                                            height="400px"))
          )
          
)
)

# Where we calculate/ define output
server <- function(input, output, session) {
 options(shiny.maxRequestSize=100*1024^2)



  df <- reactive({
    if(is.null(input$file1$datapath) ){
      df <- readRDS("./data/cleaned_data_w_lat-long.RDS")
      df <- df %>%
        dplyr::distinct(serno, .keep_all = T)

    }
    else{
    df <- read_clean(input$file1$datapath)
    df <- df %>%
      dplyr::distinct(serno, .keep_all = T)
    }
    return(df)
    })

  output$query <- DT::renderDataTable({
    df() %>%
      dplyr::filter(serno == as.character(input$ID)) %>%
      dplyr::select(serno,eff_age, style_descr, quality_descr, acreage,gla,bsmt,fin_bsmt,tot_baths,
             att_gar_area,det_gar_area)
  })

  

    output$MapResults <- renderLeaflet({
      latlong_mat <- as.matrix(df()[,c("long","lat")])
      
      # convert NA values to 0 ... for now ... back to NA after distance calculation
      latlong_mat[is.na(latlong_mat)] <- 0
      rownames(latlong_mat) <- df()$serno
      
      
      # function to find parcels in range, gather info on them, and calculate scores from:
      #     distance | sale date | similarity
      y <- as.matrix(df()[df()$serno == input$ID,c("long","lat")])
       
      
      dists <- spDistsN1(latlong_mat,y,longlat=TRUE)
      names(dists) <- rownames(latlong_mat)
      
      in_range_dists <- dists[dists <= (input$RADIUS * 1.609) & dists > 0] # find all parcels within the set radius (converted to km) and not itself (>0)
      comp_ids <- names(in_range_dists)
      
      
      subject <- df() %>%
        dplyr::filter(serno == as.character(input$ID)) 
      
      subject_pot_comp <- plyr::rbind.fill(df() %>% dplyr::filter(serno %in% comp_ids), subject)
      MapIt(subject_pot_comp, x, input$grad)
    })

  ID <- reactive({
    input$ID
  })


 
}

# Run the application
shinyApp(ui = ui, server = server)




