shinyUI(fluidPage(

  includeCSS('www/styles.css'),
  shinyjs::useShinyjs(),

  # Title ----
  tags$header(
    'Interactive visualization of the K-means algorithm',
    icon('circle-question', id = 'k-means-info'),
    shinyBS::bsTooltip(id = 'k-means-info', title = 'The K-means algorithm is a clustering algorithm which consists in associating each point to the closest cluster centroid at each step.')
  ),
  sidebarLayout(

    # Sidebar ----
    sidebarPanel(
      p('You can see the source code of this app ', a(href = "https://github.com/Asanned/test_K-means/tree/main", 'here')),
      numericInput('nbNodes', 'Number of nodes', 100, min = 10),
      numericInput('nbClusters', 'Number of clusters', 4, min = 1, max = 9),

      selectInput('distanceType', 'distance', choices = c('Euclidean', 'Manhattan'), selected = 'Euclidean'),

      div(class = 'dataset-info',
        div(
          actionButton('showDatasetModal', 'Select a dataset to visualize'),
          class = 'div1'
        ), div(
          textOutput('currentDatafile'),
          class = 'div2 req-df'
        ), div(
          textOutput('currentDataframe'),
          class = 'div3 req-df'
        ), div(
          selectInput('colX', 'Select the x-axis', choices = NULL),
          class = 'div4 req-df'
        ), div(
          selectInput('colY', 'Select the y-axis', choices = NULL),
          class = 'div5 req-df'
        ), div(
          checkboxInput('useLabels', 'Use custom labels'),
          class = 'div6 req-df'
        ), div(
          conditionalPanel(
            condition = "input.useLabels",
            selectInput('colLabel', 'Column to use as labels', choices = NULL)
          ),
          class = 'div7 req-df'
        ),
      ),

      br(),
      actionButton('reset', 'Reset', icon = icon('arrows-rotate')),
      actionButton('step', 'Step', icon = icon('arrow-right')),

      br(),
      hr(),

      checkboxInput('placeInitClusters', 'Manually place initial clusters'),
      actionButton('defaultManualClusters', 'Set manual clusters to default'),
      br(),
      conditionalPanel("input.placeInitClusters",
        div(id = 'cluster-container',
            lapply(1:9, function(i) {
              div(class = 'mc-grid-parent cluster',
                  div(paste0('Cluster n° ', i), class = 'mc-text'),
                  div(numericInput(
                    paste0('manualCluster', i, 'x'),
                    'x',
                    value = 0.5,
                    min = 0,
                    max = 1,
                    step = 0.1
                  ), class = 'mc-x'),
                  div(numericInput(
                    paste0('manualCluster', i, 'y'),
                    'y',
                    value = 0.5,
                    min = 0,
                    max = 1,
                    step = 0.1
                  ), class = 'mc-y')
              )
            })
        )
      )
    ),
    # Plot output ----
    mainPanel(
      div(
        plotOutput('kMeans'),
        class = 'plot'
      )
    )
  ),

  # Javascript code ----
  tags$script(HTML(
    '
    // set a shiny input variable for the width of the window
    $(document).on("shiny:connected", function(e) {
      Shiny.onInputChange("innerWidth", window.innerWidth);
    });

    // update it on window resize
    $(window).resize(function(e) {
      Shiny.onInputChange("innerWidth", window.innerWidth);
    });

    // Press the reset button as soon as possible to initialize the points and the plot
    $(document).on("shiny:sessioninitialized", function() {
      document.getElementById("reset").click();
    });
    '
  ))
))



