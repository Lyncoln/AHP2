BD5 = ler("banco_de_dados//BD5.xlsx")

mapeamento = c(0,3,2); mapeamento
BD5
preferencias = autoVetorN(BD5); preferencias
objetivo = preferencias[1]; objetivo
criterios = preferencias[2:(length(mapeamento)+1)]; criterios
alternativas = preferencias[(length(mapeamento) + 2):length(preferencias)]; alternativas

#normalizando

criterios_normalizados = c()
for(i in 1:length(mapeamento)){
  criterios_normalizados[[i]] = criterios[[i]] * objetivo[[1]][i]
}
names(criterios_normalizados) = names(criterios); criterios_normalizados

#Gerando nomes para a tabela

nomes = c(paste0("---",names(objetivo)))
aux = 1 # Me ajuda a me guiar pelas matrizes de subcriterios
for(i in 1:length(mapeamento)){
  nomes = append(nomes, paste0("--",names(criterios[i])))
  for(j in 1:mapeamento[i]){
    if(mapeamento[i] == 0) break
    nomes = append(nomes, paste0("-",names(alternativas[aux])))
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
tabela


#Aqui estou organizando a proporção de cada criterio por alternativas
qtd_alternativas = length(alternativas[length(alternativas)][[1]]); qtd_alternativas
pesos_alternativas = list()
#Criando a lista que serão preenchidas
for(i in 1:qtd_alternativas){
  pesos_alternativas[[i]] = 0
}
names(pesos_alternativas) = LETTERS[1:qtd_alternativas]
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
  print(names(pesos_alternativas[i]))
  print(pesos_alternativas_organizados[[i]])
  for(j in 1:length(mapeamento)){
    if(mapeamento[j] == 0 ) {
      inferior = inferior + 1
    }
    else{
      superior = inferior + mapeamento[j] - 1
      print("----")
      print(pesos_alternativas_organizados[[i]][inferior:superior])
      valor = sum(pesos_alternativas_organizados[[i]][inferior:superior])
      print(valor)
      print("----")
      inferior = superior + 1
    }

  }
}

pesos_alternativas_organizados


