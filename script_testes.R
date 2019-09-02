library(formattable)

formata_tabela = function(tabela){
  limiteInferiorCriterios = "#DeF7E9"
  limiteSuperiorCriterios = "#71CA97"
  limiteInferiorAlternativas = "#B6D4FF"
  limiteSuperiorAlternativas = "#0060D3"
  limiteInferiorCR = "#FFC5C5"
  limiteSuperiorCR = "#EF5454"
  
  numero_linhas = dim(tabela)[1]
  numero_colunas = dim(tabela)[2]
  

  
  tabela_porcento = dplyr::mutate_if(tabela, is.numeric, function(x) paste0(round(100*x,2),"%"))
  maior_alternativa = round(max(100*as.numeric(unlist(lapply(tabela[numero_linhas,3:(numero_colunas-1)],function(x) gsub("%","",x))))),2)
  
  formato = function(cor1,cor2){formatter(.tag = "span",
                                          style =function(x)style("background-color" =csscolor(gradient(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))), cor1, cor2)),
                                                                  "border-radius" = "4px",
                                                                  display = "block"))
  }
  
  formata_maior_alternativa = formatter("span",
                                        style = x ~ style("font-weight" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) == maior_alternativa, "bold", NA)))
  
  formato_CR = formatter(.tag = "span",
                         style =function(x)style("background-color" =ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) >= 5,"#FFC5C5","#B0FFD5"),
                                                 "border-radius" = "4px",
                                                 display = "block"))
    
  tabela_formatada = formattable(tabela_porcento,
                                               align = c("l",rep("r", numero_colunas - 1)),
                                               list(
                                                 "Criterios" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                 area(row = 1:(numero_linhas-1), col = 2) ~ formato(limiteInferiorCriterios,limiteSuperiorCriterios),
                                                 area(row = 1:(numero_linhas-1), col = 3:(numero_colunas-1)) ~ formato(limiteInferiorAlternativas,limiteSuperiorAlternativas),
                                                 area(col = numero_colunas) ~ formato_CR,
                                                 area(row = numero_linhas, col = (3:numero_colunas-1)) ~ formata_maior_alternativa
                                                 
                                                 )
                                               
                                               )
    
    
  return(tabela_formatada)
}



#Estou transformando a tabela em colunas de caracteres colocando o valor da célula e concatenando "%"
#No formato criado irá considerar somente o valor da tabela para fazer a variação de cores

formato = function(cor1,cor2){formatter(.tag = "span",
                    style =function(x)style("background-color" =csscolor(gradient(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))), cor1, cor2)),
                                            "font-weight" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x))))==100,"bold",NA),
                                            "border-radius" = "4px",
                                            display = "block"))
}

tabela1 = ahp1(caminho)
tabela1 = dplyr::mutate_if(tabela1, is.numeric, function(x) paste0(round(100*x,2),"%"))
#as.numeric(unlist(lapply(tabela1[,2],function(x) gsub("%","",x))))

formattable(tabela1, align = c("l",rep("r", NCOL(tabela1) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:6) ~ formato("green","royalblue")))
