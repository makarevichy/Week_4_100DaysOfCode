#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(rusmaps)
library(xlsx)

df <- fortify(rus_sub)
df_sort <- read.xlsx('~/my_work/table_sort.xlsx', sheetIndex = 1, stringsAsFactors = F)
df_sort$name <- parse_character(df_sort$name, locale = locale('ru'))
df_sort$region <- parse_character(df_sort$region, locale = locale('ru'))
df_sort <- df_sort %>% mutate(pilot = as.factor(pilot),
                              sum_2015 = select(df_sort, ends_with('2015')) %>% rowSums(),
                              sum_2016 = select(df_sort, ends_with('2016')) %>% rowSums())
# pop up
n_form <- function(x) {
  format(x, big.mark = ' ', decimal.mark = ',')
}
df_sort$hover_15 <- with(df_sort, paste(
  region, '<br/>', 'Расходы:', n_form(df_sort$sum_2015), 'тыс.руб.', '<br/>',
  'ВН:', n_form(df_sort$vn_2015), 'тыс.руб.', '<br/>',
  'БиР:', n_form(df_sort$bir_2015), 'тыс.руб.', '<br/>',
  'До 1,5 лет:', n_form(df_sort$yhod_2015), 'тыс.руб.', '<br/>',
  'При рожд.:', n_form(df_sort$pri_rozdenii_2015), 'тыс.руб.', '<br/>',
  'Ранние:', n_form(df_sort$rannie_2015), 'тыс.руб.', '<br/>',
  'Погреб.:', n_form(df_sort$pogr_2015), 'тыс.руб.', '<br/>'))
df_sort$hover_16 <- with(df_sort, paste(
  region, '<br/>', 'Расходы:', n_form(df_sort$sum_2016), 'тыс.руб.', '<br/>',
  'ВН:', n_form(df_sort$vn_2016), 'тыс.руб.', '<br/>',
  'БиР:', n_form(df_sort$bir_2016), 'тыс.руб.', '<br/>',
  'До 1,5 лет:', n_form(df_sort$yhod_2016), 'тыс.руб.', '<br/>',
  'При рожд.:', n_form(df_sort$pri_rozdenii_2016), 'тыс.руб.', '<br/>',
  'Ранние:', n_form(df_sort$rannie_2016), 'тыс.руб.', '<br/>',
  'Погреб.:', n_form(df_sort$pogr_2016), 'тыс.руб.', '<br/>'))
# merge
df2 <- left_join(df, df_sort, by = 'id')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Расходы по ВНиМ"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'year',
                  label = 'Год:',
                  choices = list(`2015` = 'sum_2015', `2016` = 'sum_2016')),
      selectInput(inputId = 'pilot',
                  label = 'Выделить пилоты:',
                  choices = c('нет', 'да'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot_vnim", height = '500px')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot_vnim <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(input$year == 'sum_2015' && input$pilot == 'нет'){
      p <- ggplot(df2, aes(long, lat, group = group, fill = progolzh_15, text = hover_15)) +
        geom_polygon(colour = 'black') +
        xlab(NULL) + ylab(NULL) + labs(title = 'Карта России') +
        scale_fill_continuous(guide = guide_legend(title = 'Прод. листка:')) +
        coord_quickmap()
    } 
    if(input$year == 'sum_2016' && input$pilot == 'нет'){
      p <- ggplot(df2, aes(long, lat, group = group, fill = progolzh_16, text = hover_16)) +
        geom_polygon(colour = 'black') +
        xlab(NULL) + ylab(NULL) + labs(title = 'Карта России') +
        scale_fill_continuous(guide = guide_legend(title = 'Прод. листка:')) +
        coord_quickmap()
    } 
    if(input$year == 'sum_2015' && input$pilot == 'да'){
      p <- ggplot(df2, aes(long, lat, group = group, fill = pilot, text = hover_16)) +
        geom_polygon(colour = 'black') +
        xlab(NULL) + ylab(NULL) + labs(title = 'Карта России') +
        scale_fill_continuous(guide = guide_legend(title = 'Прод. листка:')) +
        coord_quickmap()
    }
    if(input$year == 'sum_2016' && input$pilot == 'да'){
      p <- ggplot(df2, aes(long, lat, group = group, fill = pilot, text = hover_16)) +
        geom_polygon(colour = 'black') +
        xlab(NULL) + ylab(NULL) + labs(title = 'Карта России') +
        scale_fill_continuous(guide = guide_legend(title = 'Прод. листка:')) +
        coord_quickmap()
    } 
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)