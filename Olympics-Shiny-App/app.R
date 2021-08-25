
# Import Libraries --------------------------------------------------------


library(tidytuesdayR)
library(tidyverse)
library(png)
library(gridExtra)
library(grid)
library(shiny)
library(reactable)
library(magick)

# Read Tidy-Tuesday Data --------------------------------------------------


olympics <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv'
  )
img=readPNG("OL2.png")
img=rasterGrob(img,interpolate = TRUE)
# UI Part -----------------------------------------------------------------


ui <-  fluidPage(
  titlePanel(title="Olympics Data Viewer"),
  sidebarLayout(
  sidebarPanel(
    selectizeInput("cou", "Select Country", choices = unique(olympics$team)),
    selectizeInput("spo",
                   "Select Sport",
                   choices = NULL),
    selectizeInput("yea", "Select Year", choices = NULL),
  ),
   mainPanel(reactableOutput("tab"),
             plotOutput("plotting"))))


# Server Part -------------------------------------------------------------


server <- function(input, output, session) {
  observeEvent(input$cou, {
    updateSelectizeInput(
      session = session,
      inputId = "spo",
      "Select Sport",
      choices = unique(olympics$sport[olympics$team %in% input$cou &
                                        olympics$medal %in% c("Gold", "Silver", "Bronze")]))
    
  })
  
  observeEvent(input$spo, {
    updateSelectizeInput(
      session = session,
      inputId = "yea",
      "Select Year",
      choices = unique(olympics$year[olympics$team %in% input$cou &
                                       olympics$sport %in% input$spo &
                                       olympics$medal %in% c("Gold", "Silver", "Bronze")])
    )
  })
  
  
  
  rf <- reactiveValues(
    df = NULL,
    sport = NULL,
    country = NULL,
    medal = NULL,
    year = NULL
  )
  rf$df <- olympics
  rf$sport <- olympics$sport
  rf$cou <- olympics$team
  rf$medal <- olympics$medal
  rf$year <- olympics$year
  
  aa <- reactive({
    rf$df %>% filter(
      rf$sport %in% input$spo &
        rf$cou %in% input$cou &
        rf$year %in% input$yea &
        rf$medal %in% c("Gold", "Silver", "Bronze"))
  })
    
  output$tab <- renderReactable({
    
    aa() %>% select(Name=name, Sex=sex, Age=age, Season=season, Event=event, Medal=medal) %>% reactable(paginationType = "simple", compact = TRUE)
  })
  
  output$plotting <- renderPlot({

aa() %>%
      count(medal) %>% ggplot(aes(x = medal,
                                  y = n,
                                  fill = medal)) + geom_col() + geom_text(
                                    aes(label = n),
                                    vjust = 0.5,
                                    color = "black",
                                    position = position_stack(0.5),
                                    size = 4.0
                                  ) + scale_fill_manual(name="Medal Type",breaks = c("Bronze", "Gold", "Silver"),
                                                        values = c("#E69F00", "gold", "#999999")) +
      labs(title =paste0(input$cou,"-",input$spo,"-",input$yea),x="Medal Type",y="Medal Count",caption = "By Hari Krishna")+  scale_y_continuous(limits = c(0, 50))+theme_minimal()+coord_flip()+theme(
        plot.background = element_rect(fill = NA, color = "cyan", size = 4),
        legend.background = element_rect(linetype = "dashed"),
        legend.text = element_text(size = 15),
        legend.title.align = 0.5,
        legend.box="vertical",
        plot.title = element_text(hjust = 0.5,size = 20),
        plot.caption =  
      )+annotation_custom(img,ymin = 45,ymax = 50)

  })

}
shinyApp(ui, server)
