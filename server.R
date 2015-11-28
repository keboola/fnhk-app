# Framework application for keboola applications. This 
# code should be filled with whatever application logic you like.

# Load libraries
library(shiny)
library(networkD3)
library(plotly)
library(data.table)
library(lubridate)

# connection to keboola, use what you like.  
# https://github.com/keboola/shiny-lib
library(keboola.shiny.lib)

prepVykony <- function(data) {
    print(names(data))
    #data$Identifikace_pripadu <- as.factor(data$identifikace$pripadu)
    data$Datum_provedeni_vykonu <- as.Date(data$datum_provedeni_vykonu)
    data$Kod_polozky <- as.factor(data$kod_polozky)
    data$Typ_polozky <- as.factor(data$typ_polozky)
    data$Body <- as.numeric(data$body)
    data$Pocet <- as.numeric(data$pocet)
    data
}
prepZup <- function(data) {
    print(names(data))
    #data$identifikace_pripadu <- as.factor(data$identifikace_pripadu)
    data$datum_provedeni_vykonu <- as.Date(data$datum_provedeni_vykonu)
    data$kod_polozky <- as.factor(data$kod_polozky)
    data$typ_polozky <- as.factor(data$typ_polozky)
    data$cena <- as.numeric(data$cena)
    data$pocet <- as.numeric(data$pocet)
    data
}


shinyServer(function(input, output, session) {
    
    # Create instance of keboola/shiny-lib
    klib <- KeboolaShiny$new(requireRunId = FALSE)
    
    # keboola will hold 
    keboola <- reactive({
        
        tables <- list(fnhk_zup = list(name="fnhk_zup_cvrt_roku"),
                       fnhk_pripady = list(name="fnhk_pripady"),
                       fnhk_vykony = list(name="fnhk_vykony_cvrt_roku")
                       )
        
        # start it up
        ret <- klib$startup(
                             list(appTitle = "Application Title",
                                  tables = tables,
                                  dataToSave = NULL,
                                  configCallback = NULL,
                                  description = FALSE,
                                  customElements = NULL,
                                  cleanData = FALSE))
        ret$tables <- tables
        
        ret$sourceData <- klib$sourceData()()
        
        ret
    })
    
    # This observe method will run if anything inside the keboola() reactive changes.  
    # That will only happen when the library is fully loaded and the authentication complete.
    # For this reason, this is a good place to trigger pulling the data and updating input elements.
    #
    # We'll use the sapi-r-client (https://github.com/keboola/sapi-r-client) to grab a list of tables in our bucket
    # Note that we'll need to check the value of keboola()$ready to make sure we're logged in and the page is loaded
    # If we're not ready, we'll do nothing and return NULL
    observe({
        dataSet <- sourceData()
        if (length(dataSet) == 0) {
            print("sourcedata is empty")
            NULL
        } else {
            updateSelectInput(session,"table",choices=names(keboola()$tables))
            # update inputs
        }
    })
    
    # Every time the chosen table changes, we'll import it into our session from SAPI
    # By default the loadTables function will display a progress bar while downloading data
    # Once we have the data, we can update our rangeCols selector with the column names
    sourceData <- reactive({
        keboola()$sourceData
    })
    
    # we'll run data through some known type setting functions 
    tableData <- reactive({
        sd <- sourceData()
        if (is.null(sd)) {
            print("null table data")
            NULL
        } else {
            td <- sd[[input$table]]    
            if (input$table == "fnhk_vykony") {
                print("prepping vykony")
                td <- prepVykony(td)
            }else if (input$table == "fnhk_zup") {
                print("prepping zup")
                td <- prepZup(td)
                print("zup prepped")
            }
            td
        }
    })
    
    # We're going to also observe for when the table gets changed
    observe({
        td <- tableData()
        if (input$table != "" && !(is.null(td))) {
            
            # update filter inputs
            updateSelectInput(session,"rangeCols", choices=names(td[sapply(td, is.numeric)]))
            updateSelectInput(session,"dateCols", choices=names(td[sapply(td, function (x) is.Date(x) | is.POSIXt(x))]))
            updateSelectInput(session,"categoryVar", choices=names(td[sapply(td, is.factor)]))
            
            # update histogram input
            updateSelectInput(session,"histCol", choices=names(td[sapply(td, function (x) { !is.factor(x)})]))
            
            # update scatter plot inputs
            updateSelectInput(session,"xcol", choices=names(td))
            updateSelectInput(session,"ycol", choices=names(td))
            updateSelectInput(session,"groupCol", choices=c("",names(td[sapply(td, is.factor)])))
            updateSelectInput(session,"facetCol", choices=c("",names(td[sapply(td, is.factor)])))
        }
    })
    
    
    
    # any time filter inputs chnage, this will run and return reflected changes
    filteredData <- reactive({
        sd <- tableData()
        input$table
        input$go
        isolate({
            if (!is.null(sd) && length(input$rangeCols) > 0) {
                # loop over our chosen range inputs
                for (i in 1:length(input$rangeCols)) {
                    rangeElem <- input$rangeCols[i]
                    
                    # filter our dataset
                    sd <- sd[
                                which(
                                    (sd[,rangeElem] > input[[rangeElem]][1]) &
                                    (sd[,rangeElem] < input[[rangeElem]][2])
                                ), 
                                # leaving the second argument empty like this means all columns will be selected
                            ]
                }
            }
            if (length(input$dateCol) > 0) {
                for (i in 1:length(input$dateCol)) {
                    dateElem <- input$dateCol[i]
                    timeInterval <- new_interval(input[[dateElem]][1],input[[dateElem]][2])
                    outdat <- outdat[which(
                        outdat[,dateElem] %within% timeInterval),]    
                }
            }
            if (length(input$categoryVar) > 0) {
                for (i in 1:length(input$categoryVar)) {
                    catlhs <- input$categoryVar[i]
                    if (length(input[[catlhs]]) > 0) {
                        outdat <- outdat[which(outdat[,catlhs] %in% input[[catlhs]]),]    
                    }
                }
            }
            # return the dataset
            sd
        })
    })
    # when a category variable is chosen a multi-select is displayed allowing the user
    # to choose which values to include in the filtered data
    output$categoryUI <- renderUI({
        if (length(input$categoryVar) > 0) {
            sd <- tableData()
            lapply(seq_along(input$categoryVar), function(x) {
                selectInput(input$categoryVar[x], input$categoryVar[x],
                            choices = levels(as.factor(sd[,input$categoryVar[x]])),
                            multiple = TRUE)    
            })
        }
    })
    # dynamically create range selectors for the chosen columns
    # the data returned by filteredData is filtered by these inputs
    # filteredData() is then used by our plot and table output elements
    output$rangeElementsUI <- renderUI({ 
        if (length(input$rangeCols) > 0) {
            
            # get our chosen table (selected by the top select input)
            sd <- tableData()
            
            # this function creates a list of sliderInputs with name equal to the selected column
            # Note, we aren't capturing this in a variable 
            # and there are no further statements so this is our return value
            lapply(seq_along(input$rangeCols), function(x) {
                # here is the numeric assumption
                colData <- as.numeric(sd[,input$rangeCols[x]])
                
                minval <- min(colData)
                maxval <- max(colData)
                sliderInput(input$rangeCols[x], input$rangeCols[x],
                            min = minval,
                            max = maxval,
                            value = c(minval,maxval))
            })
        } 
    })
    
    # dynamically create daterange selectors for the chosen columns
    # the data returned by filteredData is filtered by these inputs
    # filteredData() is then used by our plot and table output elements
    output$daterangeElementsUI <- renderUI({ 
        if (length(input$dateCols) > 0) {
            
            # get our chosen table (selected by the top select input)
            sd <- tableData()
            
            # this function creates a list of sliderInputs with name equal to the selected column
            # Note, we aren't capturing this in a variable 
            # and there are no further statements so this is our return value
            lapply(seq_along(input$dateCols), function(x) {
                
                dateRangeInput(input$dateCols[[x]], input$dateCols[[x]],
                               min = min(sd[,input$dateCols[[x]]]),
                               max = max(sd[,input$dateCols][[x]]),
                               start = min(sd[,input$dateCols[[x]]]),
                               end = max(sd[,input$dateCols[[x]]])
                )
            })      
        } 
    })
    # Render a table, this methods shows an example of a reactive element
    output$sampleTable <- renderDataTable({
        
        # the table will only list our filtered data
        filteredData()
    })
    
    # [CHANGE] Plot histogram, this methods shows an example of a reactive element
    output$histPlot <- renderPlot({
        
        # Get our filtered data
        dataSet <- filteredData()
        if (is.null(dataSet) || is.null(input$histCol) || input$histCol == "") return(NULL)
        
        # get our selected column
        x    <- as.numeric(dataSet[[input$histCol]])
        
        bins <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, prob=TRUE, breaks = bins, col = 'lightblue', border = 'white', main=paste("Histogram of", input$histCol))
        
        # lets add a density line as it can help visualize peaks and valleys
        lines(density(x, na.rm=TRUE), col="darkblue", lwd=2) # add a density estimate with defaults
    })
    
    output$varmean <- renderText({
        if (!(is.null(filteredData())))
            mean(as.numeric(filteredData()[,input$histCol]))
    })
    output$varsd <- renderText({
        if (!(is.null(filteredData())))
            sd(as.numeric(filteredData()[,input$histCol]))
    })
    output$ggplot <- renderPlotly({
        print(paste("start scatter", input$xcol, input$ycol))
        td <- filteredData()
        if (input$xcol == "" || input$xcol == "None" || input$ycol == "" || input$ycol == "None") {
            return(NULL)
        }
        p <- ggplot(td, aes_string(x = input$xcol, y = input$ycol))
        
        p <- p + geom_point()
        
        
        if (input$groupCol != 'None' & input$groupCol != "") {
            p <- p + aes_string(color=input$groupCol)
        }
        facets <- paste('. ~', input$facetCol)
        if (facets != '. ~ .' & input$facetCol != "") {
            p <- p + facet_grid(facets)
        }
        if (class(td[[input$xcol]]) == 'date') {
            p <- p + scale_x_date(labels = scales::date_format("%d. %m. %Y"))
        }
        if (class(td[[input$ycol]]) == 'date') {
            p <- p + scale_y_date(labels = scales::date_format("%d. %m. %Y"))
        }
        if (input$smooth) {
            p <- p + geom_smooth()
        }
        print("plot end")
        ggplotly(p)
    })
    
    output$scatterPlot <- renderPlotly({        
        td <- filteredData()
        pd <- data.table(
                xcol = td[[input$xcol]],
                ycol = td[[input$ycol]],
                group = td[[input$groupCol]]
            )
    
        p <- plot_ly(pd, x=xcol, y=ycol, type="scatter", mode="markers", group=group)
        if (input$groupCol != "") subplot(p)
        else p
    })
    
    # This is the plot.ly histogram
    output$trendPlot <- renderPlotly({
        data(movies, package="ggplot2")
        minx <- min(movies$rating)
        maxx <- max(movies$rating)
        
        # size of the bins depend on the input 'bins'
        size <- (maxx - minx) / input$moviebins
        
        # a simple histogram of movie ratings
        p <- plot_ly(movies, x = rating, autobinx = F, type = "histogram",
                     xbins = list(start = minx, end = maxx, size = size))
        # style the xaxis
        layout(p, xaxis = list(title = "Ratings", range = c(minx, maxx), autorange = F,
                               autotick = F, tick0 = minx, dtick = size))
    })
    
    # This is a plotly box plot
    output$boxPlot <- renderPlotly({
        
        plot_ly(td, x = percollege, color = state, type = "box")
    })
    
    # Fancy 3D surface plot
    output$volcano <- renderPlotly({
        plot_ly(z = volcano, type = "surface")
    })
})
