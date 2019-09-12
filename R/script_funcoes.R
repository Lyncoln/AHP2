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
  if(consistenciaAleatoria[tamanhoMatriz]==0) return(0)
  return(IndiceConsistencia/consistenciaAleatoria[tamanhoMatriz])
}


matrizJulgamento = function(qtd,CR = TRUE){
  matriz = diag(1, qtd ,qtd)
  for (i in 1:(qtd-1)){
    for(j in (i+1):(qtd)){
    valor = eval(parse(text = (readline(paste0("Qual é a importância do critério ",as.character(i)," em relação ao critério ",as.character(j),": ")))))
    matriz[i,j] = valor
    matriz[j,i] = 1/valor
    }
  }
  if(CR == TRUE)return(list(Matriz = matriz,CR = round(CR(matriz),3)))
  else return(matriz)
}


library(readxl);library(tibble)

ler = function(caminho){
  planilhas = readxl::excel_sheets(caminho)
  matrizes = suppressMessages(lapply(planilhas ,function(x) readxl::read_excel(path = caminho, col_names = FALSE, sheet = x)))
  names(matrizes) = planilhas
  return(matrizes)
}


autoVetorN = function(lista){
  prioridades = lapply(lista, function(x) autoVetor(x))
  return(prioridades)
}


# vetor_prioridades = function(lista){
#   vetorPrioridades = c()
#   focoPrincipal = lista[[length(lista)]]
#   qtdAlternativas = length(lista[[1]])
#   qtdFocoPrincipal = length(focoPrincipal)
#   for(i in 1:qtdAlternativas){
#     soma = 0
#     for(j in 1:qtdFocoPrincipal){
#       soma = soma + focoPrincipal[j] * lista[[j]][i]
#     }
#     vetorPrioridades[i] = soma
#   }
#   return(vetorPrioridades)
# }

##Dar uma revisada
# tabela = function(lista, alternativas = "PADRAO"){
#   qtdAutovetores = length(lista)
#   pesoCriterios = lista[qtdAutovetores]
#   pesoAlternativas = lista[1:qtdAutovetores-1]
#   qtdAlternativas = length(pesoAlternativas[[1]])
#   if(alternativas[1] == "PADRAO") nomeAlternativas = LETTERS[1:qtdAlternativas]
#   else nomeAlternativas = alternativas
#   tabelaPesoAlternativas = list()
#
#   for(i in 1:qtdAlternativas){
#     aux = lapply(pesoAlternativas,function(x) x[i])
#     tabelaPesoAlternativas[[i]] = unlist(aux)*unlist(pesoCriterios)
#   }
#   names(tabelaPesoAlternativas) = nomeAlternativas
#   tabelaPesoCriterios = list(Pesos = unlist(pesoCriterios))
#
#   tabelaJunta = tibble::as_tibble(append(tabelaPesoCriterios,tabelaPesoAlternativas))
#
#
#   vetorSomaPesos = apply(tabelaJunta, 2, sum)
#
#   tabelaGeral = rbind(tabelaJunta,c(vetorSomaPesos))
#
#   tabelaGeral = dplyr::mutate(tabelaGeral, Criterios = c(names(lista)))
#
#   return(dplyr::select(tabelaGeral,Criterios, dplyr::everything()))
# }


# ahp1 = function(caminho,alternativas = "PADRAO"){
#   if(class(caminho) == "list") matrizes = caminho
#   if(class(caminho) == "character") matrizes = ler(caminho)
#   autovetores = autoVetorN(matrizes)
#   tabela = tabela(autovetores,alternativas)
#   cr = unlist(lapply(matrizes,function(x) CR(x)))
#   tabelaGeral = dplyr::mutate(tabela, CR = cr)
#   return(tabelaGeral)
# }


#' #'AHP excel
#' #'
#' #'Aplica AHP em um grupo de matrizes devidamente formatadas em um arquivo excel.
#' #'
#' #'@param caminho e uma string do caminho do arquivo excel ou uma lista com as matrizes
#' #'@param alternativas espera um vetor com os nomes das alternativas a serem estudadas.
#' #'Se "PADRAO" ira preencher o nome das alternativas com letras de A:Z
#' #'@param cores altera as cores do fundo das celulas da tabela. pode ser usados
#' #'"PADRAO", "CINZA", "BRANCO"
#' #'
#' #'@return tabela formatada para visualizacao
#' #'
#' #'@import formattable
#' #'@import dplyr
#' #'@import tibble
#' #'
#' #'@export
#' aplica_ahp = function(caminho, alternativas = "PADRAO", cores = "PADRAO"){
#'   tabela = ahp1(caminho,alternativas)
#'   tabela = formata_tabela(tabela,cores)
#'   return(tabela)
#' }
