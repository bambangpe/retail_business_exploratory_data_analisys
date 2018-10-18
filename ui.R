
library(tidyverse)
library(tidyquant)
library(modelr)
library(gridExtra)
library(grid)
library(shiny)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    ")
    ),
    fluidRow(
      column(width = 4, wellPanel(
        radioButtons("plot_type", "Exploratory Data Analysis (EDA)",
                     c("Transaction UK", 
                       "Transaction all country",
                       "Transaction UK & Transaction all country",
                       "Transaction over time",
                       "Income and Return Transaction",
                       "Income from purchases/returns per time of day",
                       "Purchased return per day per time",
                       "Net income comparing month and day",
                       "Majority item purchased",
                       "Transactions of WHITE HANGING HEART T-LIGHT HOLDER",
                       #"Purchased most often per day",
                       "Proportion one time and repeat customer",
                       "Proportion of quantity of items",
                       "Mean unit price of sold items",
                       "Diff items are purchased and return per day",
                       "Net income and quant summaries",
                       "Mean price per unit sold per day",
                       "Purchased most sold per items")
        )),
        
        helpText( 
          ("NOTE |"),
          ("T1 : Stock | "),
          ("T2 : Incom/month/day |"), 
          ("T3 : Purchase & Return |"), 
          ("T4 : Trans quant onetime & repeat |"),
          ("T5 : Table sales in UK |"),
          ("T6 : Prop uk & other country |"),
          ("T7 : One time and repeat Customer |"),
          ("T8 : Table extreem outliers/suspected transaction |"),
          ("T9 : Table explanation of suspected transaction |"),
          ("T10 : Table explanation of suspected transaction |"),
          ("T11 : Detail customer suspected |"),
          ("T12 :Table repeat customer |"),
          ("T13 : Diff purchased return/day |"),
          ("T14 : Net incom quant summaries |"),
          ("DATA: http://archive.ics.uci.edu/ml/dataset/online+retail |"),
          ("Transactions between 01/12/2010 and 1/6/2011/ |"),
          ("Customers are wholesalers |")
        )), 
      
      mainPanel(
        tabsetPanel(
          tabPanel("T1",
                   dataTableOutput("gmost_sold")),
          tabPanel("T2",
                   dataTableOutput("day_mont_transac")),
          tabPanel("T3",  
                   dataTableOutput("gpurch_retur")),
          tabPanel("T4",
                   dataTableOutput("gcustomer_purch")),
          tabPanel("T5",
                   dataTableOutput("gcountry_purch")),
          tabPanel("T6",
                   dataTableOutput("gn_items")),
          tabPanel("T7",
                   dataTableOutput("grep_customer_day")),
          tabPanel("T8",
                   dataTableOutput("mnp6")),
          tabPanel("T9",
                   dataTableOutput("gmp6")),
          tabPanel("T10",
                   dataTableOutput("gmp6m")),
          tabPanel("T11",
                   dataTableOutput("gmp7")),
          tabPanel("12",
                   dataTableOutput("gincome")),
          tabPanel("13",
                   dataTableOutput("mean_unit_price")),
          tabPanel("T14",
                   dataTableOutput("gmost")),
          column(width = 2,
                 # In a plotOutput, passing values for click, dblclick, hover, or brush
                 # will enable those interactions.
                 plotOutput("plot_type", brush = "plot_brush", width = 500, height = 350),
                 verbatimTextOutput("info"))
        ))
    )
    )
  )