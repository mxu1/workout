#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investment interactive projection"),
  
  # Sidebar with a slider input for number of bins 
  fluidPage(
    flowLayout(
      column(width=12,
               sliderInput("initial",
                           "Initial Amount",
                           min = 0,
                           max = 100000,
                           step = 500,
                           value = 1000),
               sliderInput("annual",
                            "Annual Contribution",
                            min = 0,
                            max = 50000,
                            step = 500,
                            value = 2000)),
      column(width=12, offset = 24,
               sliderInput("return",
                           "Return Rate (in %)",
                           min = 0,
                           max = 20,
                           step = .1,
                           value = 5),
               sliderInput("growth",
                           "Growth Rate",
                           min = 0,
                           max = 20,
                           step = .1,
                           value = 2)),
      column(width=12, offset = 48,
               sliderInput("years",
                           "Years",
                           min = 0,
                           max = 50,
                           step = 1,
                           value = 20),
               selectInput("facet",
                           "Facet?",
                           c("No","Yes"))))),
      
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    library("ggplot2")
    library("plotly")
    initial <- input$initial
    annual <- input$annual
    return <- input$return /100
    growth <- input$growth / 100
    years <- input$years
    facet <- input$facet
    # print(initial)
    # print(annual)
    # print(return)
    # print(growth)
    # print(years)
    growing_annuity <- function(contrib, rate, growth, years) {
      top <- (1+rate)^years - (1+growth)^years
      bottom  <- rate - growth
      return(contrib * top / bottom)
    }
    annuity <- function(contrib,rate,years) {
      top <- (1+rate)^years - (1)
      bottom  <- rate
      return(contrib * top / bottom)
    }
    future_value <- function(amount,rate,years) {
      return (amount  * (1+rate)^years)
    }
    year <- c(0)
    FV_no_annuity <- c(initial)
    FV_annuity <- c(initial)
    FV_growing_annuity <- c(initial)
    
    for (i in seq(1,years,by=1)) {
      year <- c(year,i)
      FV_no_annuity <- c(FV_no_annuity, future_value(initial,return,i))
      FV_annuity <- c(FV_annuity,future_value(initial,return,i) + annuity(annual,return,i))
      FV_growing_annuity <- c(FV_growing_annuity,future_value(initial,return,i) + growing_annuity(annual,return,growth,i))
    }
    modalities <- data.frame(year, FV_no_annuity,FV_annuity, FV_growing_annuity)
    colnames(modalities) = c("year","no_contrib","fixed_contrib","growing_contrib")
    rownames(modalities) <- NULL
    print(modalities)
    
    if (facet == "No") {
      df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                       variable=rep(paste0("category", 1:9), each=5))
      
      plot(modalities$year, modalities$growing_contrib, type = "l", col = "red", xlab = "Year", ylab = "Contrib")
      lines(modalities$year, modalities$fixed_contrib, type = "l", col = "blue")
      lines(modalities$year, modalities$no_contrib, type = "l", col = "green")
      legend("topleft", legend=c("Growing Contrib", "Fixed Contrib", "No Contrib"),
             col=c("red", "blue", "green"), lty = 1, cex=0.8)
      grid()
      title("Timeline Graph of the Different Contrib")
    }
    if (facet == "Yes") {
      # FV_no_annuity <- c(FV_no_annuity, future_value(initial,return,i))
      # FV_annuity <- c(FV_annuity,future_value(initial,return,i) + annuity(annual,return,i))
      # FV_growing_annuity <- c(FV_growing_annuity,future_value(initial,return,i) + growing_annuity(annual,return,growth,i))
      
      # g1 <- ggplot() + geom_line(aes(x=years, y=regular_no_contribution), color="blue") + 
      #   geom_line(aes(x=years, y=regular_fixed_contribution), color="red") + 
      #   geom_line(aes(x=years, y=regular_growing_contribution), color="green") + ylab("regular savings") +xlab("year") + guides(fill = guide_legend(title = "LEFT", title.position = "left"))
      # ggplotly()
      # g2 <- ggplot() + geom_line(aes(x=years, y=high_yield_no_contribution), color="blue") + 
      #   geom_line(aes(x=years, y=high_yield_fixed_contribution), color="red") + 
      #   geom_line(aes(x=years, y=high_yield_growing_contribution), color="green") + xlab("Years") +ylab("high-yield savings")
      # ggplotly()
      # g3 <- ggplot() + geom_line(aes(x=years, y=index_fund_no_contribution), color="blue") + 
      #   geom_line(aes(x=years, y=index_fund_fixed_contribution), color="red") + 
      #   geom_line(aes(x=years, y=index_fund_growing_contribution), color="green") + xlab("Years") +ylab("index fund")
      # 
      amount <- c(FV_no_annuity,FV_annuity,FV_growing_annuity)
      label <- c()
      for( i in seq(1,length(FV_no_annuity),by=1)) {
        label <- c(label, "no_contrib")
      }
      for( i in seq(1,length(FV_annuity),by=1)) {
        label <- c(label, "fixed_contrib")
      }
      for( i in seq(1,length(FV_growing_annuity),by=1)) {
        label <- c(label, "growing_contrib")
      }
      years <- c(year,year,year)
      # ggplotly()
      data <- data.frame(amount,label,year)
      data <- data %>% mutate(category = factor(label))
      print(data)
      gg <- ggplot(data,mapping = aes(x=years, y=amount, colour=category,fill=category)) + geom_area(aes(fill=category)) + geom_line(aes(x=years,amount)) + ylab("dollars") + facet_grid(.~label) 
      # + scale_shape_discrete(name  ="Legend",breaks=c("regular"), labels=c("regular"))
      print(gg)
      # ggplotly()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

