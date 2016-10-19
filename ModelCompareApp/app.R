
# library(ROCR)
library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Binomial Model Comparison"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      selectInput("data1", label = h3("Select Regression 1"),
                  choices = ls(.GlobalEnv),selected="authdata" ),
      uiOutput("columns"),

      radioButtons("type", "Compare Models",
                   choices = c("No" = FALSE,
                               "Yes" = TRUE),
                   selected = FALSE),

      actionButton("action", label = "Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # verbatimTextOutput("summary"),
      h3("Model Measures"),
      tableOutput("summary"),
      h3("Gini and AUC"),
      tableOutput("GiniAuc"),
      h3("Models Gini Curve"),
      plotOutput("plotGini")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {

  options(
    contrasts = c("contr.treatment", "contr.treatment"),
    na.option = na.exclude
  )

  output$columns = renderUI({
    if (input$type==TRUE){

    selectInput("data2", label = h3("Select Regression 2"),
                choices = ls(.GlobalEnv),selected="authdata" )
        }
  })


  data1 <- eventReactive(input$action,{
    data1 <- get(input$data1)
  })

  data2 <- eventReactive(input$action,{
    if (input$type==TRUE){
    data2 <- get(input$data2)
    }
  })

  work1 <- eventReactive(input$action,{
  data1 <- data1()
  tempData <- cbind(predict=predict(data1, data1$data, type="response"),actual=data1$data$keshel_Future,sample=data1$data$sample)
  work <- as.data.frame(tempData)
  work <- work %>% arrange(sample , desc(predict),desc(actual))
  workIs <- work %>% filter(sample==0) %>% mutate(p_pop=row_number()/n(),p_actual=row_number()/sum(actual), p_predict=cumsum(actual)/sum(actual))
  workOs <- work %>% filter(sample==1) %>% mutate(p_pop=row_number()/n(),p_actual=row_number()/sum(actual), p_predict=cumsum(actual)/sum(actual))
  work <- rbind(workIs,workOs)
  work$p_actual[work$p_actual>1] <- 1

  work <- work %>% mutate(diff_best=p_actual-p_pop, diff_predict=p_predict-p_pop)
  })

  work2 <- eventReactive(input$action,{
    if (input$type==TRUE){
    data1 <- data2()
    tempData <- cbind(predict=predict(data1, data1$data, type="response"),actual=data1$data$keshel_Future,sample=data1$data$sample)
    work <- as.data.frame(tempData)
    work <- work %>% arrange(sample , desc(predict),desc(actual))
    workIs <- work %>% filter(sample==0) %>% mutate(p_pop=row_number()/n(),p_actual=row_number()/sum(actual), p_predict=cumsum(actual)/sum(actual))
    workOs <- work %>% filter(sample==1) %>% mutate(p_pop=row_number()/n(),p_actual=row_number()/sum(actual), p_predict=cumsum(actual)/sum(actual))
    work <- rbind(workIs,workOs)
    work$p_actual[work$p_actual>1] <- 1

    work <- work %>% mutate(diff_best=p_actual-p_pop, diff_predict=p_predict-p_pop)
    }
  })

  output$summary <- renderTable({
    if (input$type==TRUE){
    data1=data1()
    data2=data2()

    sum1 <- glance(data1)
    # sum1$name <- data1$formula
    sum2 <- glance(data2)
    # sum2$name <- data2$formula
    sum <- rbind(sum1,sum2)
    } else {
      data1=data1()
      sum1 <- glance(data1)
    }
})

  output$GiniAuc <- renderTable({
    auc2 <- function(actual, predicted){
    r <- rank(predicted)
    n_pos <- sum(actual==1)
    n_neg <- length(actual) - n_pos
    auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
    gini <- auc*2-1
      auc
     }
    work1 <- work1()
    
    gini1 <- work1 %>%
              group_by(sample) %>%
              ## summarise(sum_p=sum(diff_predict),sum_a=sum(diff_best), gini=sum_p/sum_a, auc=0.5*gini+0.5) %>%
              summarise(auc=auc2(actual, predicted), gini=auc*2-1) %>%
              mutate(regression="Regression1", Sample = ifelse(sample==0,"In Sample","Out of Sample")) %>%
              select(regression,Sample,gini,auc)

    if (input$type==TRUE){
      work2 <- work2()
      gini2 <- work2 %>%
        group_by(sample) %>%
        ## summarise(sum_p=sum(diff_predict),sum_a=sum(diff_best), gini=sum_p/sum_a, auc=0.5*gini+0.5) %>%
        summarise(auc=auc2(actual, predicted), gini=auc*2-1) %>%
        mutate(regression="Regression2", Sample = ifelse(sample==0,"In Sample","Out of Sample")) %>%
        select(regression,Sample,gini,auc)
      gini1 <- rbind(gini1,gini2)
    }

    gini1

  })


  output$plotGini <- renderPlot({
    work <- work1()
    work <- filter(work,sample==0)
    plotGini <- ggplot(work,aes(y=p_predict, x=p_pop))+geom_line()+geom_line(aes(x=p_pop, y= p_actual), color='red')+geom_line(aes(x=p_pop, y= p_pop), color='green')


    if (input$type==TRUE){
      work2 <- work2()
      work2 <- filter(work2,sample==0)
      plotGini <- plotGini + geom_line(data=work2,aes(y=p_predict, x=p_pop), color='blue')
      }
    plotGini + xlab("% of Population") + ylab("% of Defaults")

  })


})
# Run the application
shinyApp(ui = ui, server = server)

