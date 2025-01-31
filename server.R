shinyServer(function(input, output, session){

  # Initialisation of reactive values ----
  vals = reactiveValues(x = NULL, y = NULL, labels = NULL)
  clusters = reactiveValues(labels = NULL, centres = list(x = c(), y = c()))

  maxValx = reactive({round(max(vals$x), 0)})
  minValx = reactive({round(min(vals$x), 0)})

  maxValy = reactive({round(max(vals$y), 0)})
  minValy = reactive({round(min(vals$y), 0)})

  dataset = reactiveValues(
    df = NULL, 
    error = NULL, 
    df_temp = NULL, 
    additional_info_temp = NULL, 
    additional_info = NULL
  )
  
  # Only update nbNodes and nbClusters when the reset button is pressed
  nbNodes = eventReactive(input$reset, input$nbNodes)
  nbClusters = eventReactive(input$reset, input$nbClusters)

  # Plot creation ----
  output$kMeans = renderPlot({
    if (!is.null(vals$x) && !is.null(vals$y)){
      plot(
        vals$x, 
        vals$y,
        xlim = c(minValx(), maxValx()),
        ylim = c(minValy(), maxValy()),
        xlab = "",
        ylab = "",
        axes = T,
        pch = 16,
        col = clusters.colors[clusters$labels + 1]
      )

      points(
        clusters$centres[['x']], 
        clusters$centres[['y']],
        pch = 23,
        cex = 3,
        bg = clusters.colors[2:11],
        lwd = 3
      )
      
      if (input$useLabels){
        text(
          vals$x, 
          vals$y + ((maxValy() - minValy()) / 100),
          vals$labels
        )
      }
    }
  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,100)))

  # Update values on button press ----
  observeEvent(input$reset, {
    if (is.null(dataset$df)){
      vals$x = runif(nbNodes())
      vals$y = runif(nbNodes())
    }
    if (!input$placeInitClusters){
      clusters$labels = getInitialLabels(length(vals$x), nbClusters())
      clusters$centres = updateCenters(vals$x, vals$y, clusters$labels, nbClusters())
    } else {
      clusters$labels = rep(0, length(vals$x))
      clusters$centres = list(x = c(), y = c())

      for (i in 1:nbClusters()) {
        clusters$centres[['x']] = c(clusters$centres[['x']], input[[paste0('manualCluster', i, 'x')]])
        clusters$centres[['y']] = c(clusters$centres[['y']], input[[paste0('manualCluster', i, 'y')]])
      }
    }
  })

  # Compute next step on button press ----
  observeEvent(input$step,{
    previous_labels = clusters$labels
    clusters$labels = computeNextStep(vals$x, vals$y, clusters$centres, mink_deg[[input$distanceType]])
    clusters$centres = updateCenters(vals$x, vals$y, clusters$labels, nbClusters())
    if (all(clusters$labels == previous_labels)){
      showModal(convergedModal)
    }
  })

  observeEvent(input$defaultManualClusters, defaultManualClusterInputs(nbClusters(), minValx(), maxValx(), minValy(), maxValy()))

  observe({updateClusterVisibility(input$nbClusters)})

  # Logic for the dataset selection modal ----
  observeEvent(input$showDatasetModal, showModal(datasetModal))

  observe({
    funcOut = updateDataset(
      input$datasetFile, 
      input$dataframeName,
      list(
        csvHeader = input$csvHeader,
        csvSep = input$csvSep,
        decimalSep = input$decimalSep
      ))
    dataset$df_temp = funcOut$df
    dataset$error = funcOut$error
    dataset$additional_info_temp = funcOut$additional_info
  })

  output$datasetInfo = renderText({
    if (!is.null(dataset$error)){dataset$error}
  })

  output$datasetPreview = renderTable({
    if (!is.null(dataset$df_temp)){head(dataset$df_temp)}
  })

  output$currentDatafile = renderText({
    if (!is.null(dataset$additional_info)){paste0('Reading data from file "',dataset$additional_info[['datafileName']], '"')}
  })

  output$currentDataframe = renderText({
    if (!is.null(dataset$additional_info) && !is.null(dataset$additional_info[['dataframeName']])){paste0('with dataframe "', dataset$additional_info[['dataframeName']], '"')}
  })

  observeEvent(input$dfconfirm, {
    if (!is.null(dataset$df_temp)){
      dataset$df = dataset$df_temp
      dataset$df_temp = NULL

      dataset$additional_info = dataset$additional_info_temp
      dataset$additional_info_temp = NULL

      col_quanti = names(dataset$df)[lapply(names(dataset$df), FUN = function(x){return(class(dataset$df[[x]]))}) %in% c('integer', 'numeric')]
      updateSelectInput('colX', choices = col_quanti, selected = col_quanti[1], session = session)
      updateSelectInput('colY', choices = col_quanti, selected = col_quanti[2], session = session)
      updateSelectInput('colLabel', choices = names(dataset$df), selected = names(dataset$df)[1], session = session)

      removeModal()
    } else {
      showModal(errorModal)
    }
  })

  observe({
    if (!is.null(dataset$df)){
      vals$x = dataset$df[[input$colX]]
      vals$y = dataset$df[[input$colY]]
      vals$labels = dataset$df[[input$colLabel]]
    }
  })

  observe({
    shinyjs::toggle(
      id = 'csvOptions', 
      condition = (!is.null(input$datasetFile[['name']]) && str_extract(input$datasetFile[['name']], regex('\\.[^.]+$')) %in% c('.csv'))
    )

    shinyjs::toggle(
      id = 'dataframeName',
      condition = (!is.null(input$datasetFile[['name']]) && str_extract(input$datasetFile[['name']], regex('\\.[^.]+$')) %in% c('.rdata'))
    )

    shinyjs::toggle(
      selector = '.req-df',
      condition = (!is.null(dataset$df))
    )

    shinyjs::toggle(
      id = 'nbNodes',
      condition = (is.null(dataset$df))
    )
  })

  onStop(function() {
    observe(close(input$datasetFile))
  })
})
