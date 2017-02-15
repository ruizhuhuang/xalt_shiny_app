source("helper_maverick.R")

shinyServer(function(input, output,session) {
  dataInput <- reactive({
    runXaltJava(input$StartMonth, input$EndMonth, input$conf, input$sup, out_dir,
                input$column_dis_b,input$column_dis_v,
                input$association_b,input$association_v,
                input$top_m,input$rules,input$plot_type)
    return(list.files(tmp_dir))
  })

  output$plot = renderPlot({
    # Create a Progress object
    progress <- shiny::Progress$new(session, min=1, max=2)
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = 'Processing: This may take a while...',
                      detail = '',value=0)
    # progress$set(message = "Processing...", value = 0)
    
    dat = dataInput()
    if (input$plot_type == "Barplot"){
      bar_plot(input$top_m,input$column_dis_b,input$column_dis_v)
    }
    if (input$plot_type == "Association plot_shading"){
      association_plot_shading(input$rules)
    }
    if (input$plot_type == "Association plot_itemset"){
      association_plot_itemset(input$rules)
    }
  })
})