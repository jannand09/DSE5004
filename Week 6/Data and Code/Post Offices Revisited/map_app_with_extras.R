# Module 6 - Map Application Example with additional tables and formatting

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)


#--------------------
# Pre-Processing
#--------------------
# Runs on loading of the app (not on changes to inputs)
#--------------------
new_england <- sort(c("MA","ME","VT","NH","RI","CT"))
post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')
post_offices <- post_offices %>% 
  mutate(current_with_loc = ifelse(is.na(discontinued) & !is.na(latitude) & !is.na(longitude),TRUE,FALSE)) %>% 
  filter(current_with_loc & state %in% new_england)

ne_post_office_by_county = post_offices %>% 
  group_by(state,county1) %>% 
  summarize(num_po = n()) %>% 
  mutate(county1 = tolower(county1))

ma_counties <- map_data("county", "massachusetts") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "MA")
vt_counties <- map_data("county", "vermont") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "VT")
me_counties <- map_data("county", "maine") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "ME")
ri_counties <- map_data("county", "rhode island") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "RI")
nh_counties <- map_data("county", "new hampshire") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "NH")
ct_counties <- map_data("county", "connecticut") %>% 
  select(lon = long, lat, group, id = subregion) %>% mutate(state = "CT")

counties <- bind_rows(ma_counties,vt_counties,me_counties,ri_counties,nh_counties,ct_counties)
ne_counties <- counties %>% inner_join(ne_post_office_by_county, by = c("id"="county1","state"="state"))

#-------------------------------------


# Define UI for application that draws maps of a selected state
ui <- fluidPage(

    # Application title
    titlePanel("New England Post Offices"),

    # Sidebar with a selector
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state",
                        label = "State",
                        choices = new_england,
                        selected = "MA"),
            radioButtons(inputId = "sorting",
                         label = "Sort by",
                         choices = c("County", "Count"),
                         selected = "County"),
            width = 2
        ),

        mainPanel(
          splitLayout(
            verticalLayout(
              htmlOutput(outputId = "title"),
              plotOutput(outputId = "map"),
            ),
            tableOutput(outputId = "summary_table"),
            cellWidths = c("75%", "25%")
          ),
          width = 10
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Map title object
    output$title <- renderText({paste("Number of Post Offices per",input$state,"County")})
    
    # Map object
    output$map <- renderPlot({
      g<- ggplot(ne_counties %>% filter(state == input$state), aes(lon, lat, group = group)) +
        geom_polygon(aes(fill = num_po), colour = "grey50") + 
        coord_quickmap()+
        theme_map()
      print(g)
    })
    
    # Table object
    output$summary_table <- renderTable({
      if(input$sorting == "County"){
        ne_post_office_by_county %>% filter(state == input$state) %>% arrange(county1)
      } else {
        ne_post_office_by_county %>% filter(state == input$state) %>% arrange(desc(num_po))
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
