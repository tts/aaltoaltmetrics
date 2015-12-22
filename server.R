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
#  See blog postings at 
#  https://blogs.aalto.fi/suoritin/tag/altmetrics/
#
#  Data gathering follows the code found in 
#  https://gist.github.com/tts/5df86b664b840c5927f9#file-getdata-r

#  You can run this gist in R with sample data:
#  runGist("0df0fd1deae932d6488b)
#
#  Tuija Sonkkila, 11.7.2014, 2.1.2015
#
##################################################################

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  output$text <- renderText({
    
    paste0("Altmetric and Thomson Reuters (WoS) data as of ", datadate, ". Other data by Elsevier (Scopus), Lib4RI and Publication Forum. <br/><br/>
           For more details, see these <a href='https://blogs.aalto.fi/suoritin/tag/altmetrics/' target='_blank'>blog postings</a>, and the ",
           code, " of this version.")
    
  })
  
  
  output$ggplot2schools <- renderText({schools})
  output$nvd3schools <- renderText({schools})
  output$gvschools <- renderText({schools})
  
  
  # Render a selectize drop-down selection box 
  output$items <- renderUI({
    
    if ( input$dim == 'Title') {
      nr <- 10
    } else {
      nr <- 5
    }
    
    selectizeInput(
      inputId = 'items', 
      label = paste0('Select max ', nr, ' items. Click to delete'),
      multiple = TRUE,
      choices = aalto_all[ ,names(aalto_all) %in% input$dim],
      options = list(maxItems = nr, placeholder = 'Start typing'),
      width = '400px'
    )
    
  })
  
  
  
  
  items <- reactive({
    
    if(is.null(input$items)){
      return()
    }
  
    df <- aalto_all[aalto_all[[input$dim]] %in% input$items, ]
    df
    
  })
  
  
  
  show_title <- function(x=NULL) {
    if(is.null(x)) return(NULL)
    key <- x["keys"][[1]]
    paste0(selected()$Title20[key], " ", selected()$Journal[key])
  }  
  
  
  
  # ggvis
  selected <- reactive({
         
    if (is.null(input$items)) {
      switch(input$sc, 
             All = return(aalto_all),
             ARTS = { arts <- aalto_all[aalto_all$School == 'ARTS', ]; arts$keys <- seq_along(arts[,1]); return(arts) } ,
             BIZ = { biz <- aalto_all[aalto_all$School == 'BIZ', ]; biz$keys <- seq_along(biz[,1]); return(biz) },
             CHEM = { chem <- aalto_all[aalto_all$School == 'CHEM', ]; chem$keys <- seq_along(chem[,1]); return(chem) },
             ELEC = { elec <- aalto_all[aalto_all$School == 'ELEC', ]; elec$keys <- seq_along(elec[,1]); return(elec) },
             ENG = { eng <- aalto_all[aalto_all$School == 'ENG', ]; eng$keys <- seq_along(eng[,1]); return(eng) },
             SCI = { sci <- aalto_all[aalto_all$School == 'SCI', ]; sci$keys <- seq_along(sci[,1]); return(sci) })
    }   
    
    df <- aalto_all[aalto_all[[input$dim]] %in% input$items, ]
    
    # See http://stackoverflow.com/a/24557912/2613636
    if(nrow(df) == 0) {
      switch(input$sc,    
             All = return(aalto_all),
             ARTS = { arts <- aalto_all[aalto_all$School == 'ARTS', ]; arts$keys <- seq_along(arts[,1]); return(arts) } ,
             BIZ = { biz <- aalto_all[aalto_all$School == 'BIZ', ]; biz$keys <- seq_along(biz[,1]); return(biz) },
             CHEM = { chem <- aalto_all[aalto_all$School == 'CHEM', ]; chem$keys <- seq_along(chem[,1]); return(chem) },
             ELEC = { elec <- aalto_all[aalto_all$School == 'ELEC', ]; elec$keys <- seq_along(elec[,1]); return(elec) },
             ENG = { eng <- aalto_all[aalto_all$School == 'ENG', ]; eng$keys <- seq_along(eng[,1]); return(eng) },
             SCI = { sci <- aalto_all[aalto_all$School == 'SCI', ]; sci$keys <- seq_along(sci[,1]); return(sci) })
    }
       
    df$keys <-seq_along(df[,1])
    df
  })
  
  
  
  # See https://github.com/wch/movies/blob/master/server.R
  selectedItems <- reactive({
    
    # Added isolate() 21.10.2015
    xvar_name <- isolate(input$xc)
    yvar_name <- isolate(input$yc)
    
    xc <- prop("x", as.symbol(input$xc))
    yc <- prop("y", as.symbol(input$yc))
    
    selected %>%
      ggvis(x = xc, y = yc, 
            key := ~keys, 
            fill = ~School, 
            opacity := 0.80,
            stroke = ~OA, 
            stroke := "gold",
            strokeWidth := ~OA*3,
            size.hover := 200) %>%
      layer_points() %>%
      add_tooltip(show_title) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name, title_offset = 50) %>% 
      scale_numeric("y", domain = c(0, NA), override = FALSE) %>%
      scale_numeric("x", domain = c(0, NA), override = FALSE) %>%
      set_options(width = 500, height = 500)
  })
  
  selectedItems %>% bind_shiny("gv", "slider")
  
  
  
  # Output to Chart tab with rCharts/NVD3
  output$chart <- renderChart({
    
    validate(
      need(!is.null(items()), paste0("Please select some items from ", input$dim, "."))
    )
    
    if(nrow(items()) == 0){
      return()
    }
    
    dataC <- items()
    if ( input$dim == 'Title' ) {
      dataC$id <- dataC$Title10
    } else {
      dataC$id <- dataC$Journal10
    }
    
    dataCsub <- dataC[, c(names(dataC) %in% colsChart)]
    
    # Transform data for plotting
    dataM <- melt(dataCsub, id.vars="id")
    nplot <- nPlot(value ~ id, data = dataM, 
                   group = "variable", type = "multiBarChart", 
                   width = 900)
    nplot$chart(reduceXTicks = FALSE)
    nplot$xAxis(staggerLabels = TRUE)
    nplot$set(dom="chart")
    return(nplot)
    
  })
  
  
  
  # Output nr of items to Table tab
  output$rownr <- renderText({
    
    paste("Number of items: ", if (is.null(items())) { "0" } else { nrow(items()) })
    
  })
  
  
  
  output$datatable <- DT::renderDataTable({
    
    if (is.null(items()))
      return(invisible())
    
    if (nrow(items()) == 0)
      return(invisible())
    
    rows <- items()
    rows <- rows[, c(names(rows) %in% colsTable)]
    
    # Shorten Authors and make URL of the Altmetric landing page
    for (i in 1:nrow(rows)) {
      au <- paste0(substr(rows$Authors[i], 1, 30), "...")
      rows[i, c("Authors")] <- au
      url <- substr(rows$AltmetricURL[i], 47, nchar(rows$AltmetricURL[i]))
      doUrl <- paste0("<a href='", rows[i, c("AltmetricURL")], "'>", url, "</a>")
      rows[i, c("AltmetricURL")] <- doUrl
    } 
    rows
  }, escape =F)
  
  
  
  
  # Output to query tab
  observe ({
    
    if(input$submit > 0) {
      values$doi <- isolate(input$doi)
    }
    
  })
  
  
  
  
  df <- reactive({
    
    if ( is.null(values$doi) || length(values$doi) == 0 || values$doi == '' || 
           nchar(values$doi) <= 8 || nchar(values$doi) >= 60) {
      updateTextInput(session, "doi", label = "Give any DOI (e.g. 10.1001/jama.2012.347)", value = '')
      return(NULL)
    }
    
    raw_metrics <- altmetrics(doi = values$doi)
    
    if ( !length(raw_metrics) == 0 )  {
      
      data <- altmetric_data(raw_metrics)
      
      data <- data[ , c("doi", "journal", "url", "details_url", "score", "mendeley", 
                        "connotea", "citeulike", "readers_count", "cited_by_gplus_count", 
                        "cited_by_fbwalls_count", "cited_by_posts_count", "cited_by_tweeters_count",
                        "cited_by_accounts_count", "cited_by_feeds_count", 
                        "cited_by_delicious_count", "cited_by_rdts_count","cited_by_forum_count",
                        "cited_by_qs_count", "cited_by_rh_count", "cited_by_msm_count")]
      
      names(data) <- c("DOI", "Title", "URL", "Altmetric.com_URL", "Altmetric", "Mendeley",
                       "Connotea", "CiteULike", "Readers_count", "GooglePlus", 
                       "Facebook", "Any_type_of_posts", "Twitter", 
                       "Accounts", "Blog_posts",   
                       "Delicious_bookmarks", "Reddit", "Forums", 
                       "StackExchange", "rh", "NewsOutlets")
      
      data[is.na(data)] <- ""
      data$Altmetric <- as.integer(ceiling(data$Altmetric))
      data
      
    } else {
      
      updateTextInput(session, "doi", label = "Give any DOI (e.g. 10.1001/jama.2012.347)", value = '')
      return(NULL)
      
    }
    
  })
  
  
  
  
  output$chart2 <- renderChart({
    
    validate(
      need(!is.null(df()), "DOI was not valid or no data was found")
    )
    
    df <- df()
    
    # http://stackoverflow.com/a/12868266/2613636
    factorconvertChar <- function(x){as.character(x)}
    factorconvertNum <- function(x){as.numeric(as.character(x))}
    
    df[,1:4] <- lapply(df[,1:4], factorconvertChar)
    df[,5:21] <- lapply(df[,5:21], factorconvertNum)
    
    df$id <- df$DOI
    dfsub <- df[, c(names(df) %in% colsChart2)]
    
    dfsubM <- melt(dfsub, id.vars="id")
    nplot <- nPlot(value ~ id, data = dfsubM, group = "variable", type = "multiBarChart")
    nplot$set(dom="chart2")
    return(nplot)
    
  })
  
  
})