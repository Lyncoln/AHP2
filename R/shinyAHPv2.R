library(shiny)
library(shinyMatrix)
library(rhandsontable)
library(rlang)

cria_matriz = function(){
    reseta_matriz = function(dim){
    matriz = diag(dim)
    for(i in 1:(dim-1)){
      matriz[(i+1):dim,i] = c("")
    }
    return(matriz)
  }

  verifica = function(num){
    num = eval(parse(text=num))
    if(num == 0 ) return("")
    return(round(1/num,3))
  }

  inverso = function(matriz){
    dim = dim(matriz)[1]
    for(i in 1:(dim-1)){
      matriz[(i+1):dim,i] = mapply(function(x) verifica(x), matriz[i,(i+1):dim])
    }
    return(matriz)
  }



  m = reseta_matriz(3)

  ui = fluidPage(

  numericInput("dim", "Qual é a dimenção da matriz?",3),
  actionButton("button", "Update"),
  h4("Preencha a matriz: "),
  matrixInput(inputId = "matrix",value = m),
  h4("Matriz inversa:"),
  rHandsontableOutput("table"),
  actionButton("button2", "Codigo"),
  verbatimTextOutput("texto")

  )




  server = function(input, output, session){
    output$table <- renderRHandsontable({

      matriz = input$matrix
      rhandsontable(inverso(matriz))

    })

    observeEvent(input$button,{
      nrow = input$dim
      m = reseta_matriz(nrow)
      updateMatrixInput(session, "matrix", m)

    })

    observeEvent(input$button2,{
      output$texto = renderPrint({

        codigo = paste0("matrix(c(",paste(inverso(input$matrix),collapse=", "),"),ncol = 3)")
        codigo


      })

    })


  }
  #viewer <- paneViewer(300) # Aparece no Viewer
  viewer = dialogViewer("icut", width = 800, height = 700) # Abre uma janela
  runGadget(ui, server, viewer = viewer)
}


