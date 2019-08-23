matriz = matrix(c(1,2,1/2,1/7,1/2,1,1/5,1/9,2,5,1,1/5,7,9,5,1),ncol = 4)

autoVetor = function(matriz){
  
  #Achando o autovetor associado ao maior autovalor
  autoValores = Re(eigen(matriz)$values)
  autoVetores = Re(eigen(matriz)$vectors)
  autoValorMax = which.max(autoValores)
  autoVetorAssociado = autoVetores[,autoValorMax]
  #
  
  autoVetorNormalizado = autoVetorAssociado/sum(autoVetorAssociado)
  
  return(autoVetorNormalizado)
  
  
}

CR = function(matriz){
  autoValores = Re(eigen(matriz)$values)
  autoVetores = Re(eigen(matriz)$vectors)
  autoValorMax = max(autoValores)
  tamanhoMatriz = length(matriz[1,])
  IndiceConsistencia = abs(autoValorMax - tamanhoMatriz) / (tamanhoMatriz - 1)
  consistenciaAleatoria = c(0,0,0.52,0.89,1.11,1.25,1.35,1.40,1.45,1.49,1.51,1.54,1.56,1.57,1.58)
  
  return(IndiceConsistencia/consistenciaAleatoria[tamanhoMatriz])
}


matrizJulgamento = function(qtd){
  matriz = diag(1, qtd ,qtd)
  for (i in 1:(qtd-1)){
    for(j in (i+1):(qtd)){
    valor = eval(parse(text = (readline(paste0("Qual é a importância do critério ",as.character(i)," em relação ao critério ",as.character(j),": ")))))
    matriz[i,j] = valor
    matriz[j,i] = 1/valor
    }
  }
  return(matriz)
}


library(readxl);library(tibble)

ler = function(caminho){
  planilhas = readxl::excel_sheets(caminho)
  matrizes = suppressMessages(lapply(planilhas ,function(x) readxl::read_excel(path = caminho, col_names = FALSE, sheet = x)))
  names(matrizes) = planilhas
  return(matrizes)
}


autoVetorN = function(caminho){
  matrizes = ler(caminho)
  prioridades = lapply(matrizes, function(x) autoVetor(x))
  return(prioridades)
}


vetor_prioridades = function(lista){
  vetorPrioridades = c() 
  focoPrincipal = lista[[length(lista)]]
  qtdAlternativas = length(lista[[1]])
  qtdFocoPrincipal = length(focoPrincipal)
  for(i in 1:qtdAlternativas){
    soma = 0
    for(j in 1:qtdFocoPrincipal){
      soma = soma + focoPrincipal[j] * lista[[j]][i]
    }
    vetorPrioridades[i] = soma
  }
  return(vetorPrioridades)
}




