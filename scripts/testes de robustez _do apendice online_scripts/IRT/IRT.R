library(mirt)
mirtCluster()
semP50 <- subset(df, select=c(P51_recod_numeric,
                              P52_recod_numeric,
                              P53_recod_numeric,
                              P54_recod_numeric,
                              P55_recod_numeric,
                              P56_recod_numeric_apoio_positivo,
                              P58_recod_numeric))

na.omit(semP50) -> semP50
TRI1 <- mirt(semP50, 1,itemtype = 'graded')#+  500 iterações
TRI1
summary(TRI1)

M2(TRI1)
coef(TRI1, simplify = TRUE)
itemfit(TRI1) # H0 = item é bom! Aqui, o ideal é que p > 0.05!

#plotar
plot(TRI1) #valor total esperado
plot(TRI1, type = 'SE') # erro
plot(TRI1, type = 'info') #informação do teste
plot(TRI1, type = 'trace') #todos os itens Modelos unidimensionais!

itemplot(TRI1, 1) # curva de cada item dado as categorias, só colocar o item



library(psych)
scree(semP50[,1:7],pc=F,hline=T,add=F,main="Sem P50")#1 PCA
fa1 <- fa(semP50)
summary(fa1)
fa1$loadings
psych::describe(semP50)

TRI2 <- mirt(semP50, 2,itemtype = 'graded')#+  500 iterações
summary(TRI2)
TRI3<- mirt(semP50, 3,itemtype = 'graded')# + 500 iterações
summary(TRI3)


library(easystats)
results <- correlation(semP50)
results %>%
  summary(redundant = TRUE) %>%
  plot()
resultado_alpha <- alpha(semP50)
resultado_alpha#0.34


TRI1_1PL <- mirt(semP50, 1, itemtype = 'graded', pars = '1PL') #+500 iteraç
TRI1_2PL <- mirt(semP50, 1, itemtype = 'graded', pars = '2PL') #+500 iteraç
TRI1_3PL <- mirt(semP50, 1, itemtype = 'graded', pars = '3PL') #+500 iteraç
TRI1_4PL <- mirt(semP50, 1, itemtype = 'graded', pars = '4PL') #+500 iteraç




all <- subset(df, select=c(P50_antidemoc_positivo,
                           P51_recod_numeric,
                           P52_recod_numeric,
                           P53_recod_numeric,
                           P54_recod_numeric,
                           P55_recod_numeric,
                           P56_recod_numeric_apoio_positivo,
                           P58_recod_numeric))



semP53 <- subset(df, select=c(P50_antidemoc_positivo,P51_recod_numeric,
                              P52_recod_numeric,
                              P54_recod_numeric,
                              P55_recod_numeric,
                              P56_recod_numeric_apoio_positivo,
                              P58_recod_numeric))

semP50eSemP56 <- subset(df, select=c(P51_recod_numeric,
                                     P52_recod_numeric,
                                     P53_recod_numeric,
                                     P54_recod_numeric,
                                     P55_recod_numeric,
                                     P58_recod_numeric))
semP50_P56_P58 <- subset(df, select=c(P51_recod_numeric,
                                      P52_recod_numeric,
                                      P53_recod_numeric,
                                      P54_recod_numeric,
                                      P55_recod_numeric))
semP50_P53_P56_P58  <- subset(df, select=c(P51_recod_numeric,
                                           P52_recod_numeric,
                                           P54_recod_numeric,
                                           P55_recod_numeric))

semP50eSemP53 <- subset(df, select=c(P51_recod_numeric,
                                     P52_recod_numeric,
                                     P56_recod_numeric_apoio_positivo,
                                     P54_recod_numeric,
                                     P55_recod_numeric,
                                     P58_recod_numeric))
TRI1 -> TRI_semP50
na.omit(all) -> all
na.omit(semP53) -> semP53
na.omit(semP50eSemP56) -> semP50eSemP56
na.omit(semP50_P56_P58) -> semP50_P56_P58
na.omit(semP50_P53_P56_P58) -> semP50_P53_P56_P58 
na.omit(semP50eSemP53) -> semP50eSemP53 
TRI_all <- mirt(all, 1,itemtype = 'graded')# 347 iter
TRI_all
summary(TRI_all)
M2(TRI_all)
anova(TRI_all, TRI_semP50)#P50 melhor

TRI_semP53 <- mirt(semP53, 1,itemtype = 'graded')# 395 iter
TRI_semP53
summary(TRI_semP53)
M2(TRI_semP53)
anova(TRI_semP53, TRI_semP50)#, com base nos índices de informação, o TRI_semP53 parece ser um modelo melhor ajustado


TRI_semP50eSemP53 <- mirt(semP50eSemP53, 1,itemtype = 'graded')# + 500 iterações

TRI_semP50_P56_P58 <- mirt(semP50_P56_P58, 1,itemtype = 'graded')# + 500 iterações

TRI_semP50_P53_P56_P58 <- mirt(semP50_P53_P56_P58, 1,itemtype = 'graded')# + 500 iterações

TRI_semP50eSemP56<- mirt(semP50eSemP56, 1,itemtype = 'graded')#85 iterações
TRI_semP50eSemP56
summary(TRI_semP50eSemP56)
M2(TRI_semP50eSemP56)
anova(TRI_semP50eSemP56, TRI_semP53)#,melhor modelo TRI_semP50eSemP56
