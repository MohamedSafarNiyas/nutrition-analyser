## Shiny UI component for the Dashboard

ui<-dashboardPage(
  skin="green",
  
  dashboardHeader(title="Nutrition Analysis Dashboard", 
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/mohamedsafar-s-955850251/" ,icon("linkedin"), "My Profile", target="_blank"))
                  
  ),
  
  
  dashboardSidebar(
    
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                
                # Conditional Panel for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Select the Variable" , choices = c1)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the Macronutrients" , choices = c2)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1, selected = "calories")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, selected = "iron")),
                menuItem("Info Graphics", tabName = "map", icon=icon("map"))
                
    )
  ),
  
  
  dashboardBody(
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("server"),
                              fluidRow(
                                column(width = 8, tags$img(src="https://images.unsplash.com/photo-1498837167922-ddd27525d352?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=870&q=80", width =600 , height = 400,alt="logo"),
                                       tags$br() , 
                                       tags$a("Photo represents the Organic Nutrients"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$h3("Nutrition analysis is the process of evaluating the nutritional content of foods, meals, and diets. It involves assessing the levels of various nutrients in food and comparing them to recommended daily intake levels or other established standards. Nutrition analysis can help individuals make informed choices about their diet and can also be useful for healthcare professionals, food manufacturers, and policymakers.")
                                )
                              )
                              
                              
                     ), 
                     tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
              )
              
      ),  
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Nutrition Analysis by names", value="trends",solidHeader = T, width=12,
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              withSpinner(plotlyOutput("bar"))
                     ),
                     tabPanel("Distribution", value="distro",
                              # selectInput("var", "Select the variable", choices=c("name", "calories")),
                              withSpinner(plotlyOutput("histplot", height = "350px"))),
                     tabPanel("Relationship among the food and its Nutrients", 
                              radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("lm"), selected = "lm" , inline = TRUE), 
                              withSpinner(plotlyOutput("scatter")), value="relation"),
                     side = "left"
              ),
              
      ),
      
      
      # Third Tab Item
      tabItem(tabName ='map',
              fluidRow(
                box(title='Different Nutrients in Different Food',status = 'success',solidHeader = T, width=12,
                    
                    fluidRow(
                      column(4,offset = 0.5,
                             selectizeInput('selected2','Select Food to Display',choices=unique(my_data$name),selected=T))),
                    fluidRow(
                      valueBoxOutput('info1'),
                      valueBoxOutput('info2'),
                      valueBoxOutput('info3'),
                      valueBoxOutput('info4'),
                      valueBoxOutput('info5'),
                      valueBoxOutput('info6'),
                      valueBoxOutput('info7'),
                      valueBoxOutput('info8'),
                      valueBoxOutput('info9'),
                    )))
              
              
              
              
      )
      
    )
  )
)






## Shiny Server component for dashboard

server<-function(input, output, session){
  
  # Data table Output
  output$dataT <- renderDataTable(my_data)
  
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 food's with high rate of", input$var2, "Nutrients")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 food's with low rate of", input$var2, "Nutrients")
  )
  
  
  
  output$top5 <- renderTable({
    
    my_data %>% 
      select(name, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  
  output$low5 <- renderTable({
    
    my_data %>% 
      select(name, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
    
  })
  
  
  # For Structure output
  output$structure <- renderPrint({
    my_data %>% 
      str()
  })
  
  
  # For Summary Output
  output$summary <- renderPrint({
    my_data %>% 
      summary()
  })
  
  # For histogram - distribution charts
  output$histplot <- renderPlotly({
    p1 = my_data %>% 
      plot_ly() %>% 
      add_histogram(x=~get(input$var1)) %>% 
      layout(xaxis = list(title = paste(input$var1)))
    
    
    p2 = my_data %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    # stacking the plots on top of each other
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
  })
  
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlotly({
    my_data %>% 
      plot_ly() %>% 
      add_bars(x=~name, y=~get(input$var2)) %>% 
      layout(title = paste("Foodwise Nutrients for", input$var2),
             xaxis = list(title = "name"),
             yaxis = list(title = paste(input$var2, "calories") ))
  })
  
  
  ### Scatter Charts 
  output$scatter <- renderPlotly({
    p = my_data %>% 
      ggplot(aes(x=get(input$var3), y=get(input$var4))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("Relation between", input$var3 , "and" , input$var4),
           x = input$var3,
           y = input$var4) +
      theme(  plot.title = element_textbox_simple(size=10,
                                                  halign=0.5))
    
    
    # applied ggplot to make it interactive
    ggplotly(p)
    
  })
  
  
  ## Correlation plot
  output$cor <- renderPlotly({
    my_df <- my_data %>% 
      select(-State)
    
    # Compute a correlation matrix
    corr <- round(cor(my_df), 1)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
  #info1
  output$info1 = renderValueBox({
    valueBox(paste0(format(my_data$servin_size[my_data$name == input$selected2])," serving-grams"),
             input$selected2,
             icon = icon('weight-scale'),
             color = 'teal')
  })
  
  #info2
  output$info2 = renderValueBox({
    valueBox(paste0(format(my_data$calories[my_data$name == input$selected2],)," Calories"),
             input$selected2,
             icon = icon('fire'),
             color = 'yellow')
  })
  
  #info3
  output$info3 = renderValueBox({
    valueBox(paste0(format(my_data$total_fat[my_data$name == input$selected2],)," Fat"),
             input$selected2,
             icon = icon('burger'),
             color = 'red')
  })
  #info4
  output$info4 = renderValueBox({
    valueBox(paste0(format(my_data$protein[my_data$name == input$selected2],)," Protein"),
             input$selected2,
             icon = icon('dumbbell'),
             color = 'teal')
  })
  #info5
  output$info5 = renderValueBox({
    valueBox(paste(format(my_data$cholesterol[my_data$name == input$selected2],)," Cholesterol"),
             input$selected2,
             icon = icon('bomb'),
             color = 'yellow')
  })
  #info6
  output$info6 = renderValueBox({
    valueBox(paste0(format(my_data$iron[my_data$name == input$selected2],)," Iron"),
             input$selected2,
             icon = icon('weight-hanging'),
             color = 'red')
  })
  #info7
  output$info7 = renderValueBox({
    valueBox(paste0(format(my_data$vitamin_k[my_data$name == input$selected2],),"  vitamin k"),
             input$selected2,
             icon = icon('yin-yang'),
             color = 'teal')
  })
  #info8
  output$info8 = renderValueBox({
    valueBox(paste0(format(my_data$vitamin_b6[my_data$name == input$selected2],)," vitamin b6"),
             input$selected2,
             icon = icon('glass-water'),
             color = 'yellow')
  })
  #info9
  output$info9 = renderValueBox({
    valueBox(paste0(format(my_data$vitamin_e[my_data$name == input$selected2],)," vitamin E"),
             input$selected2,
             icon = icon('eye'),
             color = 'red')
  })
  
  
}





#### Load the required packages ####
# if packages are not installed already,
# install them using function install.packages(" ")

library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating

#### Dataset Manipulation ####

my_data<-read.csv("C:\\dataholder\\nutri.csv")

rnames = colnames(my_data)
rnames
# Column names without name. This will be used in the selectinput for choices in the shinydashboard
c1 = my_data %>% 
  select(-"name",-"servin_size") %>% 
  names()

# Column names without name and serving_size . This will be used in the selectinput for choices in the shinydashboard
c2 = my_data %>% 
  select(-"name", -"servin_size") %>% 
  names()


install.packages("devtools")








shinyApp(ui=ui,server=server)
