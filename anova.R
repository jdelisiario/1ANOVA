#-----------------------------------------
#
# ANÁLISE COMPARATIVA PSO
#
# Código feito por:
# Ciniro Aparecido Leite Nametala
# ciniro@gmail.com - IFMG Campus Bambui
#
# Adaptado por:
# Jaciara Domingos Elisiário
# jaciaraelisiario@gmail.com - IFMG Campus Bambuí
#
#------------------------------------------


rm(list=ls())
cat('\014')
library("car")
library("lmtest")
library("agricolae")
library("multcomp")

fonte <- 0.7

#CARREGAMENTO DOS DADOS INICIAIS------------------------------
entrada<-read.table(file="dadosiniciaisrosenbrock.csv",header=T,sep=",")

#GERAÇÃO DAS AMOSTRAS INICIAIS---------------------------------
tres <- entrada[1:30,]
dez <- entrada[31:60,]
vinte <-  entrada[61:90,]
trinta <-  entrada[91:120,]
dadosiniciais <- rbind(tres,dez,vinte,trinta)

#ANÁLISE PRELIMINAR--------------------------------
print("ANÁLISE PRELIMINAR DAS AMOSTRAS INICIAIS")
print("-----------------------------------------------")
print("Sumarizacao das amostras iniciais como um todo:")
print(summary(dadosiniciais))
print("Standard Deviation")
print(sd(dadosiniciais$Gbest))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Tres Dimensõeses----------------")
print(summary(tres$Gbest))
print("Standard Deviation")
print(sd(tres$Gbest))
print("-----------------------------------------------")
print("Dez dimensões----------------")
print(summary(dez$Gbest))
print("Standard Deviation")
print(sd(dez$Gbest))
print("-----------------------------------------------")
print("Vinte dimensões----------------")
print(summary(vinte$Gbest))
print("Standard Deviation")
print(sd(vinte$Gbest))
print("-----------------------------------------------")
print("Trinta Dimensões----------------")
print(summary(trinta$Gbest))
print("Standard Deviation")
print(sd(trinta$Gbest))
print("-----------------------------------------------")


boxplot(Gbest~Dimensao, 
        data = dadosiniciais,
        ylab = "Gbests",
        xlab = "Dimensoes",
        names=c("Tres","Dez","Vinte","Trinta"), cex.lab=fonte, cex.axis=fonte)

par(cex.lab=fonte)
par(cex.axis=fonte)
qqPlot(tres$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Três dimensões")
qqPlot(dez$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Dez dimensões")
qqPlot(vinte$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Vinte dimensões")
qqPlot(trinta$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Trinta dimensões")


print("-----------------------------------------------")
print("TESTE DE NORMALIDADE PARA CADA UMA DAS AMOSTRAS")
print("-----------------------------------------------")
print("Três dimensões----------------")
print(shapiro.test(tres$Gbest))
print("-----------------------------------------------")
print("Dez dimensões----------------")
print(shapiro.test(dez$Gbest))
print("-----------------------------------------------")
print("Vinte dimensões----------------")
print(shapiro.test(vinte$Gbest))
print("-----------------------------------------------")
print("Trinta dimensões----------------")
print(shapiro.test(trinta$Gbest))
print("-----------------------------------------------")

#CONFIGURAÇÃO DO EXPERIMENTO-----------------------------------
alpha <- 0.05
p <- 0.55
delta <- 0.1
a <- 4

#CALCULO DE TAMANHO AMOSTRAL-----------------------------------

vartres = var(tres$Gbest)
vardez = var(dez$Gbest)
varvinte = var(vinte$Gbest)
vartrinta = var(trinta$Gbest)

tau <- c(-delta/2, delta/2, rep(0, a-2))
vartau <- var(tau)

varmedia = (vartres+vardez+varvinte+vartrinta)/a

testepot = power.anova.test(groups = a, 
                            between.var = vartau, 
                            within.var = varmedia, 
                            sig.level = alpha,
                            power = p)

n = 300
print("-----------------------------------------------")
print("RESULTADO DO CALCULO DE TAMANHO AMOSTRAL VIA TESTE DE POTENCIA ANOVA")
print(testepot)
print("-----------------------------------------------")

#ANALISE DA AMOSTRA FINAL------------------------------------------
amostrafinal<-read.table("amostra_rosenbrock_anova.csv",header=T,sep=",")

tres <- amostrafinal[1:300,]
dez <- amostrafinal[301:600,]
vinte <-  amostrafinal[601:900,]
trinta <-  amostrafinal[901:1200,]
amostra <- rbind(tres,dez,vinte,trinta)

print("ANÁLISE PRELIMINAR DAS AMOSTRAS FINAIS")
print("-----------------------------------------------")
print("Analise das amostras finais como um todo")
print(summary(amostra))
print("Standard Deviation")
print(sd(amostra$gbests))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Três dimensões----------------")
print(summary(tres$gbests))
print("Standard Deviation")
print(sd(tres$gbests))
print("-----------------------------------------------")
print("Dez dimensões----------------")
print(summary(dez$gbests))
print("Standard Deviation")
print(sd(dez$gbests))
print("-----------------------------------------------")
print("Vinte dimensões----------------")
print(summary(vinte$gbests))
print("Standard Deviation")
print(sd(vinte$gbests))
print("-----------------------------------------------")
print("Trinta dimensões----------------")
print(summary(trinta$gbests))
print("Standard Deviation")
print(sd(trinta$gbests))
print("-----------------------------------------------")

boxplot(amostra,
        ylab = "Gbests",
        names=c("Tres","Dez","Vinte","Trinta"), cex.lab=fonte, cex.axis=fonte)

qqPlot(tres$gbests, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Tres")
qqPlot(dez$gbests, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Dez")
qqPlot(vinte$gbests, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Vinte")
qqPlot(trinta$gbests, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Trinta")

#AVERIGUACAO PRA USO DE ANOVA PARAMETRICA-----------
modelo <- aov(gbests~dimensao,data = amostra)
print("-----------------------------------------------")
print("AVERIGUACAO DE PREMISSAS PARA APLICACAO DE ANOVA PARAMETRICA")
print("-----------------------------------------------")
print(summary.aov(modelo))
print("-----------------------------------------------")

#Averiguação de normalidade
print("Premissa de normalidade:")
sha = shapiro.test(modelo$residuals)
print(sha)
print("-----------------------------------------------")

#Averiguação de homocedasticidade
print("Premissa de homocedasticidade:")
fli = fligner.test(gbests~dimensao, data = amostra)
print(fli)
print("-----------------------------------------------")

plot(x    = modelo$fitted.values,
     y    = modelo$residuals,
     xlab = "Variâncias",
     ylab = "Amostras")

#Averiguação de Independência
durbinWatsonTest(modelo)

plot(x    = seq_along(modelo$residuals),
     y    = modelo$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Ordem dos resíduos",
     ylab = "Valor dos resíduos")

#TESTE COM KRUSKAL-WALLIS---------------------------------------
#averigua os p-valores entre as amostras
valores <- c(tres$gbests, dez$gbests, vinte$gbests, trinta$gbests)
aux <- c(
  rep(1,length(tres$gbests)), 
  rep(2,length(dez$gbests)),
  rep(3,length(vinte$gbests)),
  rep(4,length(trinta$gbests)) )
grupos <- factor(aux, labels=c("tres","dez","vinte","trinta"))
wilc <- pairwise.wilcox.test(valores, grupos, paired=FALSE, p.adjust.method = "bon", conf.level=1-alpha)

print("-----------------------------------------------")
print("APLICACAO DE KRUSKAL-WALLIS NAO PARAMETRICO")
print(wilc)
print("-----------------------------------------------")

#averigua a magnitude das diferencas
print("-----------------------------------------------")
print("AVERIGUACAO DAS MAGNITUDES DAS DIFERENCAS ENTRE AMOSTRAS")
kw<-with(amostra,kruskal(gbests,dimensao,p.adj="bon",group=FALSE, main="amostra", alpha=alpha), console=TRUE)
print(kw$comparison)
print("-----------------------------------------------")


stop()
#ERRO A PARTIR DAQUI NA ESFERA
#ERRO A PARTIR DAQUI NA RASTRIGIN
#ERRO A PARTIR DAQUI NA ROSENBROCK

#TESTE COM ANOVA--------------------------------------------------
#DERIVAÇÃO DOS INTERVALOS DE CONFIANÇA----------------------------
#Comparação TODOS X TODOS
sistema_tukey <- glht(modelo, 
                      linfct = mcp(gbests = "Tukey"))
sistema_tukey_CI <- confint(sistema_tukey, 
                            level = (1-alpha))

print("-----------------------------------------------")
print("APLICACAO DE TUKEY TODOS CONTRA TODOS")
print(sistema_tukey_CI)
print("-----------------------------------------------")

#titulo = paste("Comparações com",(1-alpha)*100,"% de nível de confiança")
titulo=""
par(mar = c(5,12,4,2))
plot(sistema_tukey_CI, 
     xlab       = "Gbests",
     xlim       = c(-0.5,1),
     main=titulo)

