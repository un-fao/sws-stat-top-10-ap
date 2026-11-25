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
            new1 <- country_crop3[Item == input$comm,][order(-get(base_col))]
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
            new2 <- country_liveanimal3[Item == input$ani,][order(-get(base_col))]
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
            new3 <- country_crop3_stat[Item == input$comm2,][order(-get(base_col))]
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
            new4 <- country_liveanimal3_stat[Item == input$ani2,][order(-get(base_col))]
            head5_4 <- new4[, head(.SD, 5)]
            datatable(head5_4) %>%
                formatPercentage('% change', 2) %>%
                formatStyle(columns = '% change', color = styleInterval(cuts = c(-input$percentage2,input$percentage2), values = c("red", "black", "red"))) %>% 
                formatCurrency(3:7, '')

        }
    )
    
    # ---- Refresh data: base year selection and SWS fetch ----
    status <- reactiveVal("")
    output$data_status <- renderText(paste0(
        "Showing: ", base_year, " vs ", base_year - 1, "\n",
        "Data folder: ", normalizePath(data_dir), "\n",
        "Saved base years: ", paste(available_base_years(), collapse = ", "), "\n\n",
        status()))

    # reload all data/tables for base year b (applies to the whole app), then refresh the page
    switch_base_year <- function(b) {
        old <- base_year
        assign("base_year", b, envir = globalenv())
        ok <- tryCatch({ source("global.R", local = globalenv()); TRUE }, error = function(e) {
            status(paste("Loading", b, "failed:", conditionMessage(e)))
            assign("base_year", old, envir = globalenv())
            source("global.R", local = globalenv())
            FALSE
        })
        if (ok) session$reload()
    }

    observeEvent(input$load_data, {
        b <- as.integer(input$base_year)
        if (is.na(data_dir_for(b))) {
            status(paste0("No saved data for ", b - 3, "-", b, ". Use 'Fetch from SWS and load'."))
        } else switch_base_year(b)
    })

    observeEvent(input$fetch_data, {
        b <- as.integer(input$base_year)
        out_dir <- shared_data_dir
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        if (!dir.exists(out_dir) || file.access(out_dir, 2) != 0) out_dir <- "."
        # pass the token to the fetch script through the environment (not the command line)
        token <- trimws(input$sws_token)
        if (nzchar(token)) Sys.setenv(SWS_TOKEN = token) else Sys.unsetenv("SWS_TOKEN")
        on.exit(Sys.unsetenv("SWS_TOKEN"), add = TRUE)
        log <- withProgress(message = sprintf("Fetching %d-%d from SWS...", b - 3, b), value = 0.5,
            suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                c("fetch_sws_ap.R", "--years", sprintf("%d:%d", b - 3, b), "--out-dir", shQuote(out_dir)),
                stdout = TRUE, stderr = TRUE)))
        if (!is.null(attr(log, "status"))) {
            status(paste(c("Fetch from SWS failed:", tail(log, 15)), collapse = "\n"))
        } else switch_base_year(b)
    })

})
