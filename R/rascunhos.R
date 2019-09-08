mapeamento = c(3,3,3,3,4) #3 critérios  #3subcriterios #3subcriterios #3subcriterios #4 alternativas

#Aqui estou normalizando os pesos dos critérios

valores = autoVetorN(BD3)
matriz_criterios1 = valores[1]; matriz_criterios1
valores = valores[-1]
matriz_criterios2 = valores[1:mapeamento[1]]; matriz_criterios2
matriz_alternativas = valores[seq(-mapeamento[1],-1)];matriz_alternativas


i=0; matriz_criterios2Normalizados = lapply(matriz_criterios1[[1]], function(x){i <<- i+1;lapply(matriz_criterios2[i], function(y) return(unlist(y)*x))});matriz_criterios2Normalizados
matriz_criterios2Normalizados = lapply(matriz_criterios2Normalizados, function(x) return(x[[1]]));matriz_criterios2Normalizados
names(matriz_criterios2Normalizados) = names(matriz_criterios2); matriz_criterios2Normalizados


matriz_criterios1
matriz_criterios2Normalizados
matriz_alternativas



#Aqui estou pegando e tabelando o nome de criterios e subcriterios
nomes = names(autoVetorN(BD3)); nomes
nomes_criterios = names(matriz_criterios2Normalizados); nomes_criterios
nomes_alternativas = names(matriz_alternativas); nomes_alternativas

nomes = c(paste0("---",names(matriz_criterios1)))
for( i in 1:mapeamento[1]){
  nomes = append(nomes,paste0("--",nomes_criterios[i]))
  for( j in 1:mapeamento[i+1]){
    nomes = append(nomes, paste0("-",nomes_alternativas[1] ))
    nomes_alternativas = nomes_alternativas[-1]
  }
}
# tabela = tibble(Criterios = nomes)

tabela
matriz_criterios1
matriz_criterios2Normalizados

#Aqui estou dando os pesos para os criterios
#Posso juntar o processo de gerar nome e gerar pesos para 1 único for(fazer isso depois se der tempo!)

pesos_criterios1 = unlist(matriz_criterios1, use.names = F); pesos_criterios1
pesos_criterios2 = unlist(matriz_criterios2Normalizados, use.names = F); pesos_criterios2

pesos = c()
pesos = append(pesos, sum(pesos_criterios1))
for( i in 1:mapeamento[1]){
  pesos = append(pesos,pesos_criterios1[i])
  for( j in 1:mapeamento[i+1]){
    pesos = append(pesos, pesos_criterios2[1] )
    pesos_criterios2 = pesos_criterios2[-1]
  }
}
# tabela = tibble(Criterios = nomes, Pesos = pesos);tabela



#Aqui estou dando pesos as alternativas

# x = unlist(matriz_criterios2Normalizados, use.names = F)
# y = unlist(matriz_alternativas,use.names = F)
#
# mapply(function(x) return(x[1]), matriz_alternativas,USE.NAMES = F) * x


mapeamento
pesos_criterios1 = unlist(matriz_criterios1, use.names = F); pesos_criterios1
pesos_criterios2 = unlist(matriz_criterios2Normalizados, use.names = F); pesos_criterios2
pesos_alternativas = unlist(matriz_alternativas,use.names = F); pesos_alternativas

# Função que separada os pesos da matriz_alternativas pela quantidade de alternativas

separa_lista_alternativas = function(lista, qtd_alternativas,nome_alternativas = "PADRAO"){
  separados = list()
  if(nome_alternativas[1] == "PADRAO") nome_alternativas = LETTERS[1:qtd_alternativas]
  for(i in 1:qtd_alternativas){
    separados[[i]] = unlist(lapply(lista, function(x) x[i]),use.names = F)
  }
  names(separados) = nome_alternativas
  return(separados)
}

pesos_alternativas = separa_lista_alternativas(matriz_alternativas, mapeamento[length(mapeamento)]);pesos_alternativas
#Aqui eu já tenho todas os pesos das alternativas normalizados
#falta ainda fazer a somas dos critérios de nível
pesos_alternativas_normalizados = lapply(pesos_alternativas, function(x) x*pesos_criterios2);pesos_alternativas_normalizados

#Essa função ajuda a somar os valores dos critérios de nível 2 das alternativas para chegar no valor
#de critério do nivel 1. E depois aplica esses valores na lista para ter os pesos completos
soma_total_criterio_alternativa = function(vetor,mapeamento){
  inferior = 1
  superior = 0
  valor = c()
  vetor_total = c()
  for(i in 1:mapeamento[1]){
    superior = mapeamento[i+1] + superior
    valor[i] = sum(vetor[inferior:superior])
    vetor_total = c(vetor_total,valor[i],vetor[inferior:superior])
    inferior = superior + 1
  }
  vetor_total = c(sum(vetor),vetor_total)
  return(vetor_total)
}

pesos_alternativas_normalizados_completos = lapply(pesos_alternativas_normalizados, function(x) soma_total_criterio_alternativa(x,mapeamento)); pesos_alternativas_normalizados_completos




#Aqui estou organizando os erros para a tabela

erros = unlist(lapply(BD3, function(x) CR(x)),use.names = F);erros
erros_organizados = c(erros[1]);erros_organizados
erros = erros[-1];erros
erros_criterios = erros[1:mapeamento[1]];erros_criterios
erros_subcriterios = erros[-c(1:mapeamento[1])]; erros_subcriterios

erros_organizados
for( i in 1:mapeamento[1]){
  erros_organizados = append(erros_organizados,erros_criterios[i])
  for( j in 1:mapeamento[i+1]){
    erros_organizados = append(erros_organizados, erros_subcriterios[1] )
    erros_subcriterios = erros_subcriterios[-1]
  }
}



tabela = list(Criterios = nomes, Pesos = pesos)
tabela = append(tabela,pesos_alternativas_normalizados_completos)
tabela = append(tabela,list(CR = erros_organizados))
tabela = dplyr::as_tibble(tabela)
tabela




