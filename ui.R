# This is a sample application for Keboola App development
# It can be used as a template to create your own Keboola application
# For help creating shiny applications, please see the documentation at http://shiny.rstudio.com

# We need to load the shared library on the UI side 
# because it contains keboolaPage and keboolaModal methods
library("keboola.shiny.lib")
library("plotly")

shinyUI(
    # keboolaPage - displays a keboola application and takes care of authentication/login 
    #               and provides consistent header with optional toolbar.
    # @param - page - page element.  This is your main display.  
    #               Can be pageWithSidebar, bootsrtapPage, fluidPage, or anything that shiny supports
    # @param - appTitle - optional title for the app.  Is displayed in the top-right of the page header.
    keboolaPage(
        fluidPage(
            
            #make an empty column on the left as a margin (just for fun!) 
            column(1,div()), 
            
            # There are 12 columns to a row, so with this column at 10, it will be nicely centered.
            column(10,              
                            
                # Here we'll have a table selection element and that is the table we'll use throughout
                # We'll update the choices for this element on the server side, after the data loads
                fluidRow(
                    column(12, selectInput("table","Choose a table from the bucket to analyse",choices=c()))                            
                ),
                
                # In this tab we'll set up a sidebar/main style layout  
                fluidRow(
                    column(3, # sidebar
                        h5("Filters"),
                        wellPanel(
                            # all these 
                            selectInput("rangeCols","Numeric Ranges",choices=c(), multiple=TRUE),
                            
                            # this will hold a selection of range selectors depending on the chosen rangeCols.
                            uiOutput("rangeElementsUI"),
                            
                            selectInput("dateCols", "Date Ranges", choices=c(), multiple=TRUE),
                            uiOutput("daterangeElementsUI"),
                            
                            
                            selectInput("categoryVar", "Categories", choices=c(), multiple=TRUE),
                            uiOutput("categoryUI"),
                            
                            hr(),
                            actionButton("go", "Go")
                        ),
                        hr(),
                        h5("Some Statistics"),
                        fluidRow(
                            column(6,div(
                                "Mean:", textOutput("varmean")    
                            )),
                            column(6,div(
                                "Standard Deviation:", textOutput("varsd")    
                            ))
                        )
                    ),
                    column(8,
                        tabsetPanel(
                            tabPanel("Data Table",
                                     dataTableOutput("sampleTable")         
                            ),
                            tabPanel("Histograms",
                                plotOutput('histPlot'),
                                fluidRow(
                                    column(6,
                                        selectInput("histCol", "Column To Plot", choices=c())   
                                    ),
                                    column(6,
                                        sliderInput("bins",
                                                "Number of bins:",
                                                min = 1,
                                                max = 100,
                                                value = 30
                                        )
                                    )
                                )
                            ),
                            tabPanel("Scatter plots",
                                fluidRow(
                                    column(6,selectInput("xcol", "X Column", choices=c())),    
                                    column(6,selectInput("ycol", "Y Column", choices=c()))
                                ),
                                fluidRow(
                                    column(5,selectInput("groupCol", "Group by", choices = c())),
                                    column(5,selectInput("facetCol", "Facet by", choices = c())),           
                                    column(2,checkboxInput('smooth', 'Smooth Geom'))
                                ),
                                plotlyOutput('scatterPlot'),
                                plotlyOutput('ggplot')
                            )
                        ) 
                    )    
                ),
               
                # here we add our custom css and javascript to the HTML head
                tags$head(tags$script(src="example.js"),
                         tags$link(href="example.css", rel="stylesheet"))
            )
        ),
        
        # our application title
        appTitle = "FNHK -- Řezníci"
    )
)