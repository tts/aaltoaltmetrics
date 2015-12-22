##################################################################
#
#  An R Shiny web application on Aalto University articles 
#  published since 2007 which have 
#  a) a record in the Web of Science by Thomson Reuters,
#  b) a DOI, and 
#  c) have accumulated some altmetrics data by Altmetric.com
#
#
#  Thanks to 
#  
#  Thomson Reuters
#  Altmetric.com 
#  RStudio for shinyapps.io, Shiny and ggvis
#  Ramnath Vaidyanathan for rCharts (NVD3)
#  jdharrison for http://stackoverflow.com/a/24557912/2613636
#  Winston Chang for https://github.com/wch/movies/blob/master/server.R
#  
#
#  See blog postings at 
#  https://blogs.aalto.fi/suoritin/tag/altmetrics/
#
#  Data gathering follows the code found in 
#  https://gist.github.com/tts/5df86b664b840c5927f9#file-getdata-r
#
#  You can run this gist in R with sample data:
#  runGist("0df0fd1deae932d6488b)
#
#  Tuija Sonkkila, 11.7.2014 
#
##################################################################

# See https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class="icon-question-sign")
    )
  )
}

shinyUI(fluidPage(
  
  includeCSS("styles.css"),
  
  titlePanel('Some (alt)metric data for Aalto University articles published since 2007'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dim", 
        label = "Dimension", 
        choices = c("Title", "Journal")
      ),
      uiOutput("items"),
      br(),
      selectInput("xc", "Horizontal axis", as.list(metrics), selected = "WoS"),
      selectInput("yc", "Vertical axis", as.list(metrics), selected = "Mendeley"),
      helpPopup("What are these?", help, "top", "hover"),
      br(),
      br(),
      br(),
      selectInput("sc", "Filter by school (with all items)", as.list(scs), selected = "All"),
      br(),
      htmlOutput("text")
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Compare", ggvisOutput("gv"),
                 br(),
                # ggvisControlOutput("slider"),
                 htmlOutput("gvschools"),
                 br(),
                 paste0("Without any selected items, shows all ", nrow(aalto_all), " publications, but no Data")
                 ),
        tabPanel("All metrics", showOutput("chart", "nvd3")),
        tabPanel("Data",
                 textOutput("rownr"),
                 br(),
                 DT::dataTableOutput("datatable")
                 ),
        tabPanel("Query",
                 textInput("doi", "Give any DOI (e.g. 10.1001/jama.2012.347)", ""),
                 br(),
                 actionButton("submit","Submit"),
                 br(),
                 showOutput("chart2", "nvd3")
                 )
      )
    )
  )
))