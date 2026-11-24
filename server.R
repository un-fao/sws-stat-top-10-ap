#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 

shinyServer(function(input, output, session) {

    # Filter data based on selections : DYNAMIC PRIMARY
    output$dynamicPrimary <- DT::renderDataTable(
        
        if (input$comm == "All") {
            datatable(world_crop3) %>% 
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage,input$percentage), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        } else {
            new1 <- country_crop3[Item == input$comm,][order(-`2025`)]
            head5_1 <- new1[, head(.SD, 5)]
            datatable(head5_1) %>% 
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage,input$percentage), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')
            
        } 
        )
    # Filter data based on selections : DYNAMIC LIVE ANIMAL
    output$dynamicLiveanimal <- DT::renderDataTable(

        if (input$ani == "All") {
            datatable(world_liveanimal3) %>%
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage,input$percentage), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        } else {
            new2 <- country_liveanimal3[Item == input$ani,][order(-`2025`)]
            head5_2 <- new2[, head(.SD, 5)]
            datatable(head5_2) %>%
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage,input$percentage), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        }
    )
    
    output$staticPrimary <- DT::renderDataTable(
        
        if (input$comm2 == "All"){
            datatable(world_crop3_stat) %>% 
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage2,input$percentage2), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')
        } else {
            new3 <- country_crop3_stat[Item == input$comm2,][order(-`2025`)]
            head5_3 <- new3[, head(.SD, 5)]
            datatable(head5_3) %>% 
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage2,input$percentage2), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')
        }
    )
    
    output$staticLiveanimal <- DT::renderDataTable(

        if (input$ani2 == "All") {
            datatable(world_liveanimal3_stat) %>%
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage2,input$percentage2), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        } else {
            new4 <- country_liveanimal3_stat[Item == input$ani2,][order(-`2025`)]
            head5_4 <- new4[, head(.SD, 5)]
            datatable(head5_4) %>%
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage2,input$percentage2), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        }
    )
    
})
