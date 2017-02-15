source("helper_maverick.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("XALT - Spark"),
  fluidRow(
    #  sidebarLayout(
    column(3,
           wellPanel(
             helpText(h4("Analysis of Exectuable Usages and Fields of Science on Stampede")),
             #      br(),
             helpText(h5("Time range")),
             selectInput("StartMonth", 
                         label = "Start Month",
                         choices = timeRange,
                         selected = "2014-07"),
             selectInput("EndMonth", 
                         label = "End Month",
                         choices = timeRange,
                         selected = "2015-06")
           )
    ),
    column(3,
           wellPanel(
             helpText(h5("Distribution analysis setting")),
             selectInput("column_dis_b", 
                         label = "Groups on x axis",
                         choices = column_names,
                         selected = "exec_path"),
             selectInput("column_dis_v", 
                         label = "Categoris on legend",
                         choices = column_names,
                         selected = "field_of_science")
           )
           
    ),
    column(3,
           wellPanel(
             helpText(h5("Association analysis setting")),
             numericInput("conf", "Confidence", 0.2,
                          min = 0.1, max = 1.0, step=0.1),
             numericInput("sup", "Support", 0.01,
                          min = 0.01, max = 0.10, step=0.01),
             #      br(),
             selectInput("association_b", 
                         label = "Aggregation level",
                         choices = column_names,
                         selected = "user"),
             checkboxGroupInput("association_v", width='100%',inline=FALSE,
                                label = h5("Fields"), 
                                # c("_corrupt_record","allocation","build_date","build_user","date","exec_path",
                                #   "field_of_science","host","job_id","linkA","link_program","module_name",
                                #   "num_cores","num_nodes","num_threads","run_time","start_time","user")
                                choices = list( "exec_path"=5, "field_of_science"=6,
                                                "num_cores"=12,"num_nodes"=13
                                ),
                                selected = c(5,6))
           )
    ),
    column(3,
           wellPanel(
             helpText(h5("Plot setting")),
             numericInput("top_m", "Top applications for barplot", 10,
                          min = 4, max = 20),
             
             numericInput("rules", "Rules for association plot", 6,
                          min = 5, max = 50),
             selectInput("plot_type", 
                         label = "Choose a plot to display",
                         choices = c("Barplot", "Association plot_shading", "Association plot_itemset"),
                         selected = "Barplot")
             
           ),
           wellPanel(
             submitButton("Submit")
           )
           
    )
    
  ),
  
  fluidRow(
    br(),
    br(),
    column(8,
           plotOutput("plot"),
           offset = 2)
    
    #  )
  ),
  fluidRow(
    helpText(h5("      "))
  )
  
))


