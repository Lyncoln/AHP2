#BD5 = ler("banco_de_dados//BD5.xlsx")

ahp = function(base,mapeamento,nomes_alternativas){

  preferencias = autoVetorN(base); preferencias
  objetivo = preferencias[1]; objetivo
  criterios = preferencias[2:(length(mapeamento)+1)]; criterios
  alternativas = preferencias[(length(mapeamento) + 2):length(preferencias)]; alternativas
  matriz_criterios = base[2:(length(mapeamento)+1)]
  matriz_alternativas = base[(length(mapeamento) + 2):length(base)]

  #normalizando

  criterios_normalizados = c()
  for(i in 1:length(mapeamento)){
    criterios_normalizados[[i]] = criterios[[i]] * objetivo[[1]][i]
  }
  names(criterios_normalizados) = names(criterios); criterios_normalizados

  #Gerando nomes para a tabela

  nomes = c(paste0("---",names(objetivo)))
  CR_saaty = c(CR(base[[1]]))
  aux = 1 # Me ajuda a me guiar pelas matrizes de subcriterios
  for(i in 1:length(mapeamento)){
    nomes = append(nomes, paste0("--",names(criterios[i])))
    CR_saaty = c(CR_saaty, CR(matriz_criterios[[i]]))
    for(j in 1:mapeamento[i]){
      if(mapeamento[i] == 0) break
      nomes = append(nomes, paste0("-",names(alternativas[aux])))
      CR_saaty = c(CR_saaty, CR(matriz_alternativas[[aux]]))
      aux = aux + 1
    }
  }
  nomes

  #Gerando coluna de pesos de critérios e subcritérios

  pesos = c(sum(objetivo[[1]]))
  for(i in 1:length(mapeamento)){
    pesos = append(pesos, objetivo[[1]][i])
    aux = 1
    for(j in 1:mapeamento[i]){
      if(mapeamento[i] == 0)break
      pesos = append(pesos,criterios_normalizados[[i]][aux])
      aux = aux+1
    }
  }
  pesos

  #testando primeira parte
  tabela = tibble(Criterios = nomes, Pesos = pesos)
  #tabela


  #Aqui estou organizando a proporção de cada criterio por alternativas
  qtd_alternativas = length(alternativas[length(alternativas)][[1]]); qtd_alternativas
  pesos_alternativas = list()
  #Criando a lista que serão preenchidas
  for(i in 1:qtd_alternativas){
    pesos_alternativas[[i]] = 0
  }
  #names(pesos_alternativas) = LETTERS[1:qtd_alternativas]
  names(pesos_alternativas) = nomes_alternativas
  pesos_alternativas


  aux = 1 #Navega entre a posição das matrizes de alternativas
  aux2 = 1 #Navega entra a posição do preenchimento das alternativas na nova lista ordenada por linha
  for(i in 1:length(mapeamento)){
    #Se não existir subcriterios:
    if(mapeamento[[i]] == 0 ){
      for(j in 1:length(criterios_normalizados[[i]])){
        pesos_alternativas[[j]][aux2] = criterios_normalizados[[i]][j]
      }
      aux2 = aux2 + 1
    }
    #Se existir subcritérios:
    else{
      for(j in 1:length(criterios_normalizados[[i]])){
        #print("----")
        #print(criterios_normalizados[[i]][j])
        #print(names(alternativas[aux]))
        for(k in 1:length(alternativas[aux])){
          #print(alternativas[[aux]])
          for(p in 1:qtd_alternativas) {
            #print(alternativas[[aux]][p])
            pesos_alternativas[[p]][aux2] = alternativas[[aux]][p]*criterios_normalizados[[i]][j]
          }
          aux2 = aux2 + 1
        }
        aux = aux +1
      }
    }

  }
  pesos_alternativas


  ##


  #Agora irei aplicar a soma de proporções dos critérios para sub critérios

  pesos_alternativas_organizados = pesos_alternativas

  for( i in 1:qtd_alternativas){
    inferior = 2
    superior = 0
    pesos_alternativas_organizados[[i]] = c(sum(pesos_alternativas_organizados[[i]]), pesos_alternativas_organizados[[i]])
    #print(names(pesos_alternativas[i]))
    vetor = c(pesos_alternativas_organizados[[i]][1])
    #print(pesos_alternativas_organizados[[i]])
    for(j in 1:length(mapeamento)){
      if(mapeamento[j] == 0 ) {
        vetor = c(vetor, pesos_alternativas_organizados[[i]][inferior])
        inferior = inferior + 1
      }
      else{
        superior = inferior + mapeamento[j] - 1
        #print("----")
        #print(pesos_alternativas_organizados[[i]][inferior:superior])
        valor = sum(pesos_alternativas_organizados[[i]][inferior:superior])
        vetor = c(vetor, valor, pesos_alternativas_organizados[[i]][inferior:superior])
        #print(valor)
        #print("----")
        inferior = superior + 1
      }

    }
    pesos_alternativas_organizados[[i]] = vetor
  }

  pesos_alternativas_organizados

  #CR_saaty = unlist(lapply(base, function(x) return(CR(x))),use.names = F); CR_saaty
  tabela = append(tabela,pesos_alternativas_organizados)
  tabela = append(tabela, list("CR"= CR_saaty))
  return(dplyr::as_tibble(tabela))
}
ahp_geral = function(objeto, mapeamento, nomes_alternativas = "PADRAO"){
  if(class(objeto) == "character") base = ler(objeto)
  else base = objeto
  if(nomes_alternativas[[1]] == "PADRAO") nomes_alternativas = LETTERS[1:length(base[[length(base)]][[1]])]
  tabela = ahp(base, mapeamento,nomes_alternativas)

  return(tabela)
}

ranque = function(tabela){
  num_colunas = length(tabela[1,])
  nun_linhas = length(tabela[,1])
  alternativas = tabela[1,3:(num_colunas-1)]
  return(dplyr::select(dplyr::mutate(dplyr::arrange(tidyr::gather(alternativas,Alternativas,Pesos),desc(Pesos)),Ranque = c(1:length(alternativas[1,]))),Ranque,dplyr::everything()))
}
