# Based on: Commercial Real Estate Analysis and Investments, David Geltner and Norman G. Miller
# Chapter 6.2: Formal Model of Space Market Equilibrium and Cyclicality (page 115)
# Coded by Michał Kaftanowicz, kaftanowicz.com

# server.R

shinyServer(
  function(input, output) {
    
    
    mrs <- reactive({
      
    #Inputs from ui.R
    Rent1 <- input$Rent1
    trigger.rent <- input$trigger.rent # $/SF; also known as K   
    Stock1 <- input$Stock1 # SF
    Need1 <- input$Need1 # number of employees
    need.growth.rate <- input$need.growth.rate
    demand.per.need <- input$demand.per.need  # SF/employee; also known as tau
    NaturalVacancyRate <- input$NaturalVacancyRate
    rent.sensitivity <- input$rent.sensitivity  # also known as lambda
    supply.sensitivity <- input$supply.sensitivity # also known as epsilon
    demand.sensitivity <- input$demand.sensitivity # also known as eta  
    demand.intercept <- input$demand.intercept # SF; also known as alpha 
    construction.lag <- input$construction.lag # years; also known as L
    years.max <- input$years.max
    # End of inputs from ui.R
    
    # Initialization of vectors:
    Stock <- vector(mode="numeric", length=years.max)
    Need <- vector(mode="numeric", length=years.max)
    VacancyRate <- vector(mode="numeric", length=years.max)
    Rent <- vector(mode="numeric", length=years.max)
    Construction <- vector(mode="numeric", length=years.max)
    Demand <- vector(mode="numeric", length=years.max)
    OccupiedSpace <- vector(mode="numeric", length=years.max)
    
    # Giving initial values to first elements of vectors (representing year 1):
    Need[1] <- Need1
    Rent[1] <- Rent1
    Construction[1] <- 0
    Stock[1] <- Stock1
    Demand[1] <- demand.intercept -demand.sensitivity*Rent[1] +demand.per.need*Need[1]
    OccupiedSpace[1] <- min(Demand[1], Stock[1])
    VacancyRate[1] <- (Stock[1] - OccupiedSpace[1])/Stock[1]
    
    # System evolution in following years - from year 2 to years.max
    years <- seq(from = 2, to = years.max, by = 1)
    
    for (t in years){
      # If (t-construction.lag) <= 1, we base the calculation on rent in year 1
      if ((t-construction.lag) <= 1){
        t.prime <- 1
      }else{
        t.prime <- t-construction.lag
      }
      # Calculating construction in a given year:
      if (Rent[t.prime] > trigger.rent){
        Construction[t] <- supply.sensitivity*(Rent[t.prime]-trigger.rent) 
      }else{
        Construction[t] <- 0
      }
      Stock[t] <- Stock[t-1] + Construction[t]
      OccupiedSpace[t] <- min(Demand[t-1], Stock[t])
      VacancyRate[t] <- (Stock[t] - OccupiedSpace[t])/Stock[t]
      Rent[t] <- Rent[t-1]*(1-rent.sensitivity*(VacancyRate[t] - NaturalVacancyRate)/NaturalVacancyRate)
      Need[t] <- Need[t-1]*(1+need.growth.rate)
      Demand[t] <- demand.intercept -demand.sensitivity*Rent[t] +demand.per.need*Need[t]  
    }
    mr <- data.frame(Need, Demand, Rent, Construction, Stock, OccupiedSpace, VacancyRate)
    names(mr) <- c("Need", "Demand", "Rent", "Construction", "Stock", "OccupiedSpace", "VacancyRate")
    return(mr)
    })
        
    output$DemandPlot <- renderPlot({
      S <- mrs()$Stock
      C <- mrs()$Construction
      D <- mrs()$Demand
      OS <- mrs()$OccupiedSpace
      linethck <- 1.5
      xmax <- length(S)
      ymax <- max(max(S), max(C), max(D), max(OS))
      plot(D, main = "Demand, Stock, Occupied Space and Construction",
           col="blue", type="l", lwd=linethck, xlab="Year", 
           ylab="Square feet", xlim=c(1, xmax), ylim=c(0, ymax))
      lines(S, type="l", lwd=linethck, col="black")
      lines(OS, type="l", lwd=linethck, col="green")
      lines(C, type="l", lwd=linethck, col="red")
      legend("right", legend=c("Stock", "Demand", "Occupied Space", "Construction"),
             lwd=2, col=c("black", "blue", "green", "red"))
    })
    
    output$ConstructionPlot <- renderPlot({
      C <- mrs()$Construction
      linethck <- 1.5
      xmax <- length(C)
      ymax <- max(C)
      plot(C, main = "Construction - zoom in",
           col="red", type="l", lwd=linethck, xlab="Year", 
           ylab="Square feet", xlim=c(1, xmax), ylim=c(0, ymax))
    })
    
    output$NeedPlot <- renderPlot({
      N <- mrs()$Need
      linethck <- 1.5
      xmax <- length(N)
      ymax <- max(N)
      plot(N, main = "Need",
           col="blue", type="l", lwd=linethck, xlab="Year", 
           ylab="Space users", xlim=c(1, xmax), ylim=c(0, ymax))
    })
    
    output$RentPlot <- renderPlot({
      R <- mrs()$Rent
      linethck <- 1.5
      xmax <- length(R)
      ymax <- max(R)
      plot(R, main = "Rent",
           col="orange", type="l", lwd=linethck, xlab="Year", 
           ylab="Dollars per square feet", xlim=c(1, xmax), ylim=c(0, ymax))
    })
    
    output$VacancyPlot <- renderPlot({
      VR <- mrs()$VacancyRate
      NVR <- 1
      linethck <- 1.5
      xmax <- length(VR)
      ymax <- 1
      plot(VR, main = "Vacancy Rate",
           col="green", type="l", lwd=linethck, xlab="Year", 
           ylab="", xlim=c(1, xmax), ylim=c(0, ymax))
    })
  })
    
