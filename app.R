library(tidyverse)
library(tidymodels)
library(bslib)
library(shiny)
library(DALEX)
library(DALEXtra)
library(ranger)

model <- read_rds("rf_final.rds")
data("lending_club")

cp_profile <- function(explainer, new_obs, variable, newdata){
  rf_cpp <- predict_profile(explainer = explainer,
                            variables = variable,
                            new_observation = new_obs) 
  y <- predict(model, new_data = newdata, type = "prob")$.pred_good
  x <- newdata[[variable]]
  plot <- rf_cpp %>% 
    filter(`_vname_` %in% c(variable)) %>% 
    ggplot(aes(x = .data[[variable]])) +
    geom_line(aes(y = `_yhat_`)) +
    geom_point(x = x, y = y, color = "blue")
  return(plot)
}

states <- 
  lending_club  %>% 
  select(addr_state) %>% 
  distinct(addr_state) %>% 
  arrange(addr_state) %>% 
  pull(addr_state)

# Find min's, max's, and median's for quantitative vars:
stats_num <-
  lending_club  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))

ui <- fluidPage(
  theme = bs_theme(primary = "#ADD8E6", 
                   secondary = "#FFEBCD", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "yeti"),
  
  # Application title
  titlePanel("Probability of a loan being paid back"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      # added this for scrollable side panel:
      tags$head(tags$style(
        type = 'text/css', 'form.well { max-height: 600px; overflow-y: auto; }')
      ),
      # Numeric
      sliderInput(inputId = "acc_now_delinq",
                  label = "Number of accounts delinquent::",
                  min = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "all_util",
                  label = "Balance to credit limit:",
                  min = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 10, 
                  round = TRUE),
      sliderInput(inputId = "annual_inc",
                  label = "Annual income:",
                  min = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 10000, 
                  round = TRUE),
      sliderInput(inputId = "delinq_2yrs",
                  label = "Number of 30+ days past-due incidences for past 2 years:",
                  min = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "delinq_amnt",
                  label = "Past due amount owed:",
                  min = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1000, 
                  round = TRUE),
      sliderInput(inputId = "funded_amnt",
                  label = "Funded amount:",
                  min = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1000, 
                  round = TRUE),
      sliderInput(inputId = "inq_fi",
                  label = "inq_fi:",
                  min = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_12m",
                  label = "inq_last_12m:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_6mths",
                  label = "inq_last_6mths:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "int_rate",
                  label = "Interest rate:",
                  min = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = .1, 
                  round = TRUE),
      sliderInput(inputId = "num_il_tl",
                  label = "num_il_tl:",
                  min = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_12m",
                  label = "open_il_12m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_24m",
                  label = "open_il_24m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_6m",
                  label = "open_il_6m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "revol_util",
                  label = "Revolving line utilization rate:",
                  min = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 2, 
                  round = TRUE),
      sliderInput(inputId = "total_bal_il",
                  label = "Total current balance of installment accounts:",
                  min = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 10000, 
                  round = TRUE),
      sliderInput(inputId = "total_il_high_credit_limit",
                  label = "Total installments high credit/credit limit:",
                  min = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = 10000, 
                  round = TRUE),
      # Categorical
      selectInput(inputId = "verification_status", # to use in code
                  label = "Verification status:", # how it looks in UI
                  choices = lending_club %>% 
                    select(verification_status) %>% 
                    distinct() %>% 
                    arrange(verification_status) %>% 
                    pull(verification_status)
      ),
      selectInput(inputId = "addr_state", 
                  label = "State:", 
                  choices = states
      ),
      selectInput(inputId = "term", 
                label = "Term:", 
                choices = lending_club %>% 
                  select(term) %>% 
                  distinct(term) %>% 
                  arrange(term) %>% 
                  pull(term)
      ),
      selectInput(inputId = "sub_grade", 
              label = "Sub grade:", 
              choices = lending_club %>% 
                select(sub_grade) %>% 
                distinct(sub_grade) %>% 
                arrange(sub_grade) %>% 
                pull(sub_grade)
      ),
      selectInput(inputId = "emp_length", 
            label = "Emp length:", 
            choices = lending_club %>% 
              select(emp_length) %>% 
              distinct(emp_length) %>% 
              arrange(emp_length) %>% 
              pull(emp_length)
      ),
      selectInput(inputId = "var",
                  label = "Variable to vary in the plot:",
                  choices = list(Income = "annual_inc",
                                 `Interest rate` =  "int_rate")
      ),
      submitButton(text = "Create the CP profile"),
      ),
    mainPanel(
      plotOutput(outputId = "cp_plot")
    )
  )
)

server <- function(input, output) {
  output$cp_plot <- renderPlot({
    newdata <- tibble(funded_amnt = input$funded_amnt,
                      term = input$term,
                      int_rate = input$int_rate,
                      sub_grade = input$sub_grade,
                      addr_state = input$addr_state,
                      verification_status = input$verification_status,
                      annual_inc = input$annual_inc,
                      emp_length = input$emp_length,
                      delinq_2yrs = input$delinq_2yrs,
                      inq_last_6mths = input$inq_last_6mths,
                      revol_util = input$revol_util,
                      acc_now_delinq = input$acc_now_delinq,
                      open_il_6m = input$open_il_6m,
                      open_il_12m = input$open_il_12m,
                      open_il_24m = input$open_il_24m,
                      total_bal_il = input$total_bal_il,
                      all_util = input$all_util,
                      inq_fi = input$inq_fi,
                      inq_last_12m = input$inq_last_12m,
                      delinq_amnt = input$delinq_amnt,
                      num_il_tl = input$num_il_tl,
                      total_il_high_credit_limit = input$total_il_high_credit_limit)
    rf_explain <- 
      explain_tidymodels(
        model = model,
        data = lending_club %>% select(-Class), 
        y = lending_club %>% pull(Class),
        label = "rf"
      )
    cp_profile(rf_explain, newdata, input$var, newdata)
  })
}

shinyApp(ui = ui, server = server)