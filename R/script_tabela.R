transforma_tabela = function(tabela){
  numero_linhas = dim(tabela)[1]
  numero_colunas = dim(tabela)[2]

  tabela_porcento = dplyr::mutate_if(tabela, is.numeric, function(x) paste0(round(100*x,2),"%"))
  tabela_porcento = dplyr::slice(tabela_porcento, numero_linhas, 1:(numero_linhas - 1))
  nomes_criterios = c(tabela_porcento$Criterios[1], unlist(lapply(tabela_porcento$Criterios[2:numero_linhas],function(x) paste0("-  ",x))))
  tabela_porcento = dplyr::mutate(tabela_porcento, Criterios = nomes_criterios)

  return(tabela_porcento)

}

formata_tabela = function(tabela, cores = "PADRAO"){
  if(cores[1] == "PADRAO"){ #Cores escolhidas utilizando a regra de harmonia de cores triade
    limiteInferiorCriterios = "#DeF7E9"
    limiteSuperiorCriterios = "#71CA97"
    limiteInferiorAlternativas = "#B6D4FF"
    limiteSuperiorAlternativas = "#0060D3"
    limiteInferiorCR = "#FFC5C5"
    limiteSuperiorCR = "#B0FFD5"
    cor_letra = "black"
  }
  if(cores[1] == "CINZA"){
    limiteInferiorCriterios = "#9e9e9e"
    limiteSuperiorCriterios = "#4f4f4f"
    limiteInferiorAlternativas = "#9e9e9e"
    limiteSuperiorAlternativas = "#4f4f4f"
    limiteInferiorCR = "#4f4f4f"
    limiteSuperiorCR = "#9e9e9e"
    cor_letra = "white"
  }
  if(cores[1] == "BRANCO"){
    limiteInferiorCriterios = "#ffffff"
    limiteSuperiorCriterios = "#ffffff"
    limiteInferiorAlternativas = "#ffffff"
    limiteSuperiorAlternativas = "#ffffff"
    limiteInferiorCR = "#ffffff"
    limiteSuperiorCR = "#ffffff"
    cor_letra = "black"

  }

  numero_linhas = dim(tabela)[1]
  numero_colunas = dim(tabela)[2]


  tabela_porcento = transforma_tabela(tabela)
  maior_alternativa = round(max(100*as.numeric(unlist(lapply(tabela[numero_linhas,3:(numero_colunas-1)],function(x) gsub("%","",x))))),2)

  formato = function(cor1,cor2){formattable::formatter(.tag = "span",
                                          style =function(x)style("background-color" =csscolor(gradient(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))), cor1, cor2)),
                                                                  "border-radius" = "4px",
                                                                  "color" = cor_letra,
                                                                  display = "block"))
  }

  formata_maior_alternativa = formattable::formatter("span",
                                        style = x ~ style("font-weight" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) == maior_alternativa, "bold", NA),
                                                          "font-size" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) == maior_alternativa, "130%", NA)))

  formato_CR = formattable::formatter(.tag = "span",
                         style =function(x)style("background-color" =ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) >= 10,limiteInferiorCR,limiteSuperiorCR),
                                                 "border-radius" = "4px",
                                                 "color" = cor_letra,
                                                 display = "block"))

  tabela_formatada = formattable::formattable(tabela_porcento,
                                               align = c("l",rep("c", numero_colunas - 1)),
                                               list(
                                                 "Criterios" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                 area(row = 2:(numero_linhas), col = 2) ~ formato(limiteInferiorCriterios,limiteSuperiorCriterios),
                                                 area(row = 2:(numero_linhas), col = 3:(numero_colunas-1)) ~ formato(limiteInferiorAlternativas,limiteSuperiorAlternativas),
                                                 area(col = numero_colunas) ~ formato_CR,
                                                 area(row = 1, col = (3:numero_colunas-1)) ~ formata_maior_alternativa

                                                 )

                                               )


  return(tabela_formatada)
}
