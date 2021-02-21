library(agridat)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(rlang)

kreu <- tibble::as_tibble(agridat::kreusler.maize)
kreu %>% glimpse
kreu$gen %>% unique

kreu %>% 
    group_by(gen, year) %>%
    count()

kreu %<>% 
    dplyr::mutate(date = lubridate::dmy(as.character(date))) %>%
    dplyr::mutate(month = lubridate::month(date, label = T, abbr = T))

ui <- fluidPage(
    
    headerPanel(
        h3(
            "Agricultural experiments in Germany: 1875:1878.  Peak timeliness."
        )
    ),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput(
                "outcome",
                "Outcome to show:",
                c("Parent seed weight (g)" = "parentseed",
                  "Root weight (g)" = "roots",
                  "Leaves weight (g)" = "leaves",
                  "Stem weight (g)" = "stem",
                  "Tassel weight (g)" = "tassel",
                  "Grain weight (g)" = "grain",
                  "Whole plant weight (g)" = "plantweight",
                  "Whole plant height (cm)" = "plantheight",
                  "Number of leaves" = "leafcount",
                  "Total leaf area" = "leafarea")
            ),
            
            radioButtons(
                inputId = "month",
                label = "Month to show:",
                choices = c(
                    "May" = "May",
                    "June" = "Jun",
                    "July" = "Jul",
                    "August" = "Aug",
                    "September" = "Sep", 
                    "October" = "Oct"
                )
            ),
            
            checkboxGroupInput(
                inputId = "genotype",
                label = "Genotypes to include:",
                choices = c(
                    "Badischer" = "Badischer",
                    "Huhner" = "Huhner",
                    "Oberlander" = "Oberlander",
                    "Pferdezahn", # just testing that this works if name = value.
                    "Ungarischer" = "Ungarischer"
                ),
                selected = c("Badischer", "Huhner", "Pferdezahn") 
            )
        ),
        
        mainPanel = mainPanel(
            verbatimTextOutput("outcome"),
            verbatimTextOutput("month"),
            verbatimTextOutput("genotype"),
            plotOutput(outputId = "gen_plot")
        )
    )
)



server <- function(input, output) {
    filter_data <- reactive({
        kreu %>%
            filter(gen %in% input$genotype & month %in% input$month) %>%
            mutate(year = factor(year)) # should do this outside.
    })
    
    output$outcome <- renderText({
        input$outcome
    })
    
    output$month <- renderText({
        input$month
    })
    
    output$genotype <- renderText({
        input$genotype
    })
   
    output$gen_plot <- renderPlot({
        dat <- filter_data()
        
        ggplot(data = dat,
               mapping = aes(y = gen, 
                             x = !!as.symbol(input$outcome),
                             color = year)) + 
            geom_jitter(height = 0.2) +
            scale_color_viridis_d(begin = 0.2, end = 0.8, option = "magma") +
            theme_minimal()
        
    })
    
    
}

shinyApp(ui, server)
            
            
        
            
