BD5 = ler("banco_de_dados//BD5.xlsx")

mapeamento = c(0,3,2); mapeamento

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
