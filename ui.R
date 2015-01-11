# Based on: Commercial Real Estate Analysis and Investments, David Geltner and Norman G. Miller
# Chapter 6.2: Formal Model of Space Market Equilibrium and Cyclicality (page 115)
# Coded by Michał Kaftanowicz, kaftanowicz.com

# ui.R

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Formal Model of Space Market Equilibrium and Cyclicality"),
  sidebarPanel(width=3,
    sliderInput("Rent1", label="Initial rent (dollars per square foot)", min = 1, max = 100, value = 20, step = 1),
    sliderInput("trigger.rent", label="Trigger rent (dollars per square foot)", min = 0, max = 100, value = 25, step = 1),
    sliderInput("Stock1", label="Initial stock (square feet)", min = 0, max = 50*10^6, value = 20*10^6, step = 10^6),
    sliderInput("Need1", label="Initial need (number of office workers)", min = 10^3, max = 200*10^3, value = 70*10^3, step = 10^3),
    sliderInput("need.growth.rate", label="Need growth rate", min = 0, max = 1, value = 0.01, step = 0.01),
    sliderInput("demand.per.need", label="Demand per need (square feet per office worker)", min = 0, max = 1000, value = 200, step = 1),
    sliderInput("NaturalVacancyRate", label="Natural vacancy rate", min = 0, max = 1, value = 0.1, step = 0.01),
    sliderInput("rent.sensitivity", label="Rent sensitivity", min = 0, max = 1, value = 0.3, step = 0.01),
    sliderInput("supply.sensitivity", label="Supply sensitivity (square feet per dollar of rent above trigger rent)", min = 0, max = 1*10^6, value = 0.3*10^6, step = 10^4),
    sliderInput("demand.sensitivity", label="Demand sensitivity (square feet per dollar of rent)", min = 0, max = 1*10^6, value = 0.3*10^6, step = 10^4),
    sliderInput("demand.intercept", label="Demand intercept (square fet)", min = 0, max = 50*10^6, value = 10*10^6, step = 10^4),
    sliderInput("construction.lag", label="Construction lag (years)", min = 0, max = 10, value = 3, step = 1),
    sliderInput("years.max", label="Time horizon of simulation (years)", min = 5, max = 100, value = 60, step = 1)
  ),
  mainPanel( 
             plotOutput("DemandPlot"),
             plotOutput("ConstructionPlot"),
             plotOutput("RentPlot"),
             plotOutput("VacancyPlot")
  )
))
