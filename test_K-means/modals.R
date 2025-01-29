datasetModal = modalDialog(
  div(
    class = 'df-selection',
    fileInput('datasetFile', ' ', buttonLabel = 'Select a file', multiple = FALSE, accept = c('.csv', '.rdata')),
    textInput('dataframeName', 'Enter the name of the dataframe to import', placeholder = 'i.e. df'),
  ),
  div(
    checkboxInput('csvHeader', 'The file contains a header', value = TRUE),
    selectInput('csvSep', 'Separator', c(';', ',', 'TAB'), selected = ';', multiple = FALSE),
    selectInput('decimalSep', 'Decimal separator', c('.', ','), multiple = FALSE),
    id = 'csvOptions'
  ),
  textOutput('datasetInfo'),
  hr(),
  div(icon('table-columns'), 'Data preview'),
  div(
    tableOutput('datasetPreview'),
    class = 'table'
  ),
  title = 'Select a dataset',
  footer = div(
    modalButton('Cancel'),
    actionButton('dfconfirm', 'Confirm')
  ),
  size = 'l',
  easyClose = TRUE
)

errorModal = modalDialog(
  h2('The dataset could not be imported correctly'),
  title = 'Error',
  easyClose = TRUE,
  size = 'l'
)