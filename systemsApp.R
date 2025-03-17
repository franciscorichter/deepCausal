library(shiny)
library(visNetwork)
library(htmltools)    # for withMathJax()
library(htmlwidgets)  # for JS()

ui <- fluidPage(
  titlePanel("Real-Time DAG with Auto-Accepted Edges & Live Equations"),
  
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_x", "Number of X nodes:", value = 3, min = 1, max = 50),
      numericInput("num_a", "Number of A (intervention) nodes:", value = 1, min = 0, max = 10),
      actionButton("create_graph", "Create / Reset Graph"),
      br(), br(),
      helpText(
        "Instructions:",
        "1) Click 'Create / Reset Graph' to place nodes (X_i, A_i, Y).",
        "2) Click 'Edit' in the top-left corner of the visNetwork, select 'Add Edge',",
        "   and drag from PARENT -> CHILD. No pop-up will appear; edges are accepted instantly.",
        "3) The adjacency and structural equations update automatically below the graph."
      )
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               visNetworkOutput("graph", height = "500px")
        ),
        column(6,
               h4("Adjacency (live)"),
               verbatimTextOutput("adjacency_out"),
               h4("Structural Equations (live)"),
               uiOutput("equations_out")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values for nodes/edges in the *editable* graph
  rv_nodes <- reactiveVal(NULL)
  rv_edges <- reactiveVal(NULL)
  
  # 1) Create/Reset the graph
  observeEvent(input$create_graph, {
    # Build node IDs
    nX <- input$num_x
    nA <- input$num_a
    
    x_nodes <- paste0("X_", seq_len(nX))
    a_nodes <- paste0("A_", seq_len(nA))
    y_node  <- "Y"
    
    all_nodes <- c(x_nodes, a_nodes, y_node)
    
    # Create a data.frame for visNetwork
    nodes_df <- data.frame(
      id    = all_nodes,
      label = all_nodes,
      group = c(rep("X", length(x_nodes)),
                rep("A", length(a_nodes)),
                "Y"),
      stringsAsFactors = FALSE
    )
    
    edges_df <- data.frame(from=character(0), to=character(0), stringsAsFactors=FALSE)
    
    rv_nodes(nodes_df)
    rv_edges(edges_df)
  })
  
  # 2) Render visNetwork with auto-accept for new edges
  output$graph <- renderVisNetwork({
    req(rv_nodes(), rv_edges())
    
    visNetwork(nodes = rv_nodes(), edges = rv_edges()) %>%
      visEdges(arrows = "to") %>%
      # Color nodes by group
      visGroups(groupname = "X", color = list(background="#A0C8F0", border="#000000")) %>%
      visGroups(groupname = "A", color = list(background="#90EE90", border="#000000")) %>%
      visGroups(groupname = "Y", color = list(background="#FFD700", border="#000000")) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visOptions(
        manipulation = list(
          enabled = TRUE,
          # Auto-accept new edges
          addEdge = JS(
            "function(edgeData, callback) {",
            "  edgeData.arrows = 'to';",  # ensures arrow on the 'to' side
            "  callback(edgeData);",      # accept the edge (no pop-up)
            "}"
          ),
          deleteEdge = TRUE
        )
      )
  })
  
  # 3) Listen for user adding/removing edges
  observeEvent(input$graph_edges, {
    edges_in <- input$graph_edges
    if (is.null(edges_in)) {
      rv_edges(data.frame(from=character(0), to=character(0), stringsAsFactors=FALSE))
    } else {
      df <- data.frame(
        from = sapply(edges_in, `[[`, "from"),
        to   = sapply(edges_in, `[[`, "to"),
        stringsAsFactors = FALSE
      )
      rv_edges(df)
    }
  })
  
  # 4) Build adjacency & show in real time
  output$adjacency_out <- renderPrint({
    req(rv_nodes(), rv_edges())
    node_ids <- rv_nodes()$id
    edges_df <- rv_edges()
    
    # adjacency[[child]] = vector of parents
    adjacency <- lapply(node_ids, function(child) {
      edges_df$from[edges_df$to == child]
    })
    names(adjacency) <- node_ids
    
    cat("adjacency[[child]] = c(parents...)\n\n")
    print(adjacency)
  })
  
  # 5) Structural equations in real time
  output$equations_out <- renderUI({
    req(rv_nodes(), rv_edges())
    node_ids <- rv_nodes()$id
    edges_df <- rv_edges()
    
    # Build adjacency
    adjacency <- lapply(node_ids, function(child) {
      edges_df$from[edges_df$to == child]
    })
    names(adjacency) <- node_ids
    
    # For each node: child = sum_of_parents + eps
    eq_list <- lapply(node_ids, function(child) {
      pars <- adjacency[[child]]
      if (length(pars) == 0) {
        paste0("$$ ", child, " = \\varepsilon_{", child, "} $$")
      } else {
        rhs <- paste(c(pars, paste0("\\varepsilon_{", child, "}")), collapse = " + ")
        paste0("$$ ", child, " = ", rhs, " $$")
      }
    })
    
    tagList(lapply(eq_list, function(eq_line) {
      div(eq_line)
    }))
  })
}

shinyApp(ui, server)

