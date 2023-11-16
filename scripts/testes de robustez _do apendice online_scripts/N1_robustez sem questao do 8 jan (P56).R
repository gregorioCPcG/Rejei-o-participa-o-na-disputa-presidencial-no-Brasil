#df é a base de dados 
df_semP56 <- subset(df, select=c(P51_recod_numeric,
                             P52_recod_numeric,
                             P53_recod_numeric,
                             P54_recod_numeric,
                             P55_recod_numeric,
                             P58_recod_numeric))
library(mice)
imp <- mice(df_semP56, seed=23109)# o nome da base e a seed sempre essa 23109
df_semP56 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df_semP56)


model <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_mice <- cfa(model, data = df_semP56)
summary(cfa_model_mice, standardized = TRUE)

#o novo vs o anterior
semTools::fitmeasures(cfa_model_mice, c("tli", "cfi", "rmsea", "srmr", "gfi", "aic", "bic"))

scores <- lavPredict(cfa_model_mice)
as.numeric(scores) -> df_semP56$scores

df_semP56$scores <- scales::rescale(df_semP56$scores, to = c(0, 1))#Esse código utiliza a função rescale() do pacote scales para reescalar os valores da variável df_semP56$scores. A função rescale() ajusta os valores para um novo intervalo de valores definido pelos argumentos to, que neste caso é de 0 a 1. Isso é útil quando se deseja comparar variáveis que têm escalas diferentes ou quando se deseja normalizar os valores de uma variável para que eles sejam mais facilmente interpretáveis.
summary(df_semP56$scores)#deu boa
hist(df_semP56$score, breaks=50)

