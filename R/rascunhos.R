# tabela_ahp2 = function(lista, mapeamento){
#
#   qtd_criterios1 = mapeamento[1]
#   qtd_criterios2 = sum(mapeamento[2:(length(mapeamento) -1)])
#
#
#
#   lapply(qtd_criterios1, function(x) return(c(x)))
# }
#
#
# caminho = "C://Users//Smart//Documents//AHP//data//BD3.xlsx"
# BD = ler(caminho)
#
# mapeamento = c(3,3,3,3,4) #3 critérios  #3subcriterios #3subcriterios #3subcriterios #4 alternativas
#
#
# valores = autoVetorN(BD)
# matriz_criterios1 = valores[1]; matriz_criterios1
# valores = valores[-1]
# matriz_criterios2 = valores[1:mapeamento[1]]; matriz_criterios2
# matriz_alternativas = valores[seq(-mapeamento[1],-1)];matriz_alternativas
#
# i=0
# matriz_criterios2Normalizados = lapply(matriz_criterios1[[1]], function(x){i <<- i+1;lapply(matriz_criterios2[i], function(y) return(unlist(y)*x))});matriz_criterios2Normalizados
