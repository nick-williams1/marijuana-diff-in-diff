
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)

mj <- read_csv("C:/Users/niwi8/OneDrive/Documents/Practicum/MJ_DinD/Data/MJ_proportions.csv",
               col_names = TRUE)

mj$state <- as.factor(mj$state)
mj$drug <- as.factor(mj$drug)
mj$age <- as.factor(mj$age)
mj$year <- as.factor(mj$year)

mj <- mj %>% mutate(variance = se^2)

rnormA <- repeatable(rnorm, seed = 10)
rnormB <- repeatable(rnorm, seed = 20)
rnormC <- repeatable(rnorm, seed = 30)
rnormD <- repeatable(rnorm, seed = 40)

# Define UI for application 
ui <- dashboardPage(
     skin = "blue", 
     dashboardHeader(
          title = "Difference-in-Difference Analysis", 
          titleWidth = 350, 
          dropdownMenu(
               type = "messages", 
                    messageItem(
                         from = "Nick", 
                         message = "Hello World!"
                    )
               )
          ),
     dashboardSidebar(
          selectInput(inputId = "state", label = "State", 
                      choices = c("Colorado" = "colorado", 
                                  "Washington" = "wash"), 
                      selected = "colorado"),
          selectInput(inputId = "drug", label = "Drug", 
                      choices =  c("Alcohol" = "alcohol", 
                                   "Cigarettes" = "cig", 
                                   "Tobacco" = "tobacco"), 
                      selected = "alcohol"),
          selectInput(inputId = "age", label = "Age group", 
                      choices = c("12-17" = "12", 
                                  "18-25" = "18", 
                                  "26+" = "26"),
                      selected = "12"),
          selectInput(inputId = "year", label = "Year", 
                      choices = c("2016", "2015"), 
                      selected = "2016"),
          selectInput(inputId = "reference_year", label = "Reference Year", 
                      choices = c("2011", "2012"), 
                      selected = "2011"),
          div(
               img(src = "crown.png", 
                   height = 80), 
               style="text-align: center;")
     ),
     dashboardBody(
          fluidRow(
               column(width = 4,
                      box(title = "Empirical approach", 
                          status = "primary", 
                          width = NULL,
                          verbatimTextOutput(
                               outputId = "empirical"
                               ))
                      ), 
               column(width = 4,
                      box(title = "Theoretical approach",
                          status = "primary", 
                          width = NULL,
                          verbatimTextOutput(
                               outputId = "theoretical"
                               ))
                      ), 
               column(width = 4,
                      box(title = "Hybrid approach", 
                          status = "primary",
                          width = NULL,
                          verbatimTextOutput(
                               outputId = "hybrid"
                          ))
                      )
               ),
          fluidRow(
               box(title = "Data", 
                   status = "primary",
                   width = 12,
                   DT::dataTableOutput(
                        "table"
                   ))
               )
          )
     )
  
# Define server logic 

server <- function(input, output) {
     vars1 <- reactive({
          mj %>% filter(state == input$state,
                        drug == input$drug,
                        age == input$age,
                        year == input$year)
     })
     vars2 <- reactive({
          mj %>% filter(state == input$state,
                        drug == input$drug, 
                        age == input$age,
                        year == input$reference_year)
     })
     US1 <- reactive({
          mj %>% filter(state == "us",
                        drug == input$drug,
                        age == input$age,
                        year == input$year)
     })
     US2 <- reactive({
          mj %>% filter(state == "us",
                        drug == input$drug,
                        age == input$age,
                        year == input$reference_year)
     })
     
     #empirical approach calculations
     
     s1 <- reactive({
          rnormA(n = 10000,
                 mean = vars1()$percent,
                 sd = vars1()$se)
     })
     s2 <- reactive({
          rnormB(n = 10000, 
                 mean = vars2()$percent, 
                 sd = vars2()$se)
     })
     s3 <- reactive({
          rnormC(n = 10000, 
                 mean = US1()$percent,
                 sd = US1()$se)
     })
     s4 <- reactive({
          rnormD(n = 10000, 
                 mean = US2()$percent, 
                 sd = US2()$se)
     })
     
     difference1 <- reactive({
          s1() - s2()
     })
     difference2 <- reactive({
          s3() - s4()
     })
     final_difference <- reactive({
          difference1() - difference2()
     })
     
     output$empirical <- renderPrint({
          mu <- round(mean(final_difference()), 4)
          se <- round(sd(final_difference()), 4)
          ci <- round(quantile(final_difference(), c(.025, 0.975)), 4)
          list("Difference-in-difference" = mu, 
               "Standard error" = se, 
               "95% Confidence interval" = ci)
     })
     
     #theoretical approach calculations
     
     diff1 <- reactive({
          vars1()$percent - vars2()$percent
     }) 
     diff2 <- reactive({
          US1()$percent - US2()$percent
     })
     diff_diff <- reactive({
          diff1() - diff2()
     })
     se_diff_diff <- reactive({
          sqrt(vars1()$variance + vars2()$variance + US1()$variance + US2()$variance)
     })
     
     output$theoretical <- renderPrint({
          ci_lower_diffdiff <- round(diff_diff() - (1.96*(se_diff_diff())), 4)
          ci_upper_diffdiff <- round(diff_diff() + (1.96*(se_diff_diff())), 4)
          
          interval <- data.frame(ci_lower_diffdiff, ci_upper_diffdiff)
          
          interval <- interval %>% rename("Lower bound" = ci_lower_diffdiff, 
                                          "Upper bound" = ci_upper_diffdiff)
          
          list("Difference-in-difference" = diff_diff(), 
               "Standard error" = round(se_diff_diff(), 4), 
               "95% Confidence interval" = interval)
     })
     
     #hybrid approach calculations
     
     output$hybrid <- renderPrint({
          se <- round(sd(final_difference()), 4)
          
          ci_lower_hybrid <- round(diff_diff() - (1.96*(se)), 4)
          ci_upper_hybrid <- round(diff_diff() + (1.96*(se)), 4)
          
          interval2 <- data.frame(ci_lower_hybrid, ci_upper_hybrid)
          
          interval2 <- interval2 %>% rename("Lower bound" = ci_lower_hybrid, 
                                            "Upper bound" = ci_upper_hybrid)
          
          list("Difference-in-difference" = diff_diff(), 
               "Simulated standard error" = se, 
               "95% Confidence interval" = interval2)
     })
     
     # data table
     
     mj_1 <- mj %>% select(state:se)
     
     output$table <- DT::renderDataTable({
          DT::datatable(mj_1[which(mj$state == input$state & 
                                      mj$drug == input$drug &
                                      mj$age == input$age  & 
                                      mj$year == input$year |
                                      mj$state == "us" &
                                      mj$drug == input$drug & 
                                      mj$age == input$age & 
                                      mj$year == input$year |
                                      mj$state == input$state &
                                      mj$drug == input$drug &
                                      mj$age == input$age & 
                                      mj$year == input$reference_year |
                                      mj$state == "us" & 
                                      mj$drug == input$drug & 
                                      mj$age == input$age &
                                      mj$year == input$reference_year), ], 
                        rownames = FALSE, 
                        options = list(pageLength = 4))
     })
}

# Run the application 

shinyApp(ui = ui, server = server)

