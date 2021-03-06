#-----------------------------------------
#
# AN�LISE COMPARATIVA PSO
#
# C�digo feito por:
# Ciniro Aparecido Leite Nametala
# ciniro@gmail.com - IFMG Campus Bambui
#
# Adaptado por:
# Jaciara Domingos Elisi�rio
# jaciaraelisiario@gmail.com - IFMG Campus Bambu�
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

#GERA��O DAS AMOSTRAS INICIAIS---------------------------------
tres <- entrada[1:30,]
dez <- entrada[31:60,]
vinte <-  entrada[61:90,]
trinta <-  entrada[91:120,]
dadosiniciais <- rbind(tres,dez,vinte,trinta)

#AN�LISE PRELIMINAR--------------------------------
print("AN�LISE PRELIMINAR DAS AMOSTRAS INICIAIS")
print("-----------------------------------------------")
print("Sumarizacao das amostras iniciais como um todo:")
print(summary(dadosiniciais))
print("Standard Deviation")
print(sd(dadosiniciais$Gbest))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Tres Dimens�eses----------------")
print(summary(tres$Gbest))
print("Standard Deviation")
print(sd(tres$Gbest))
print("-----------------------------------------------")
print("Dez dimens�es----------------")
print(summary(dez$Gbest))
print("Standard Deviation")
print(sd(dez$Gbest))
print("-----------------------------------------------")
print("Vinte dimens�es----------------")
print(summary(vinte$Gbest))
print("Standard Deviation")
print(sd(vinte$Gbest))
print("-----------------------------------------------")
print("Trinta Dimens�es----------------")
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
qqPlot(tres$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Tr�s dimens�es")
qqPlot(dez$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Dez dimens�es")
qqPlot(vinte$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Vinte dimens�es")
qqPlot(trinta$Gbest, ylab = "Gbests", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Trinta dimens�es")


print("-----------------------------------------------")
print("TESTE DE NORMALIDADE PARA CADA UMA DAS AMOSTRAS")
print("-----------------------------------------------")
print("Tr�s dimens�es----------------")
print(shapiro.test(tres$Gbest))
print("-----------------------------------------------")
print("Dez dimens�es----------------")
print(shapiro.test(dez$Gbest))
print("-----------------------------------------------")
print("Vinte dimens�es----------------")
print(shapiro.test(vinte$Gbest))
print("-----------------------------------------------")
print("Trinta dimens�es----------------")
print(shapiro.test(trinta$Gbest))
print("-----------------------------------------------")

#CONFIGURA��O DO EXPERIMENTO-----------------------------------
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

print("AN�LISE PRELIMINAR DAS AMOSTRAS FINAIS")
print("-----------------------------------------------")
print("Analise das amostras finais como um todo")
print(summary(amostra))
print("Standard Deviation")
print(sd(amostra$gbests))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Tr�s dimens�es----------------")
print(summary(tres$gbests))
print("Standard Deviation")
print(sd(tres$gbests))
print("-----------------------------------------------")
print("Dez dimens�es----------------")
print(summary(dez$gbests))
print("Standard Deviation")
print(sd(dez$gbests))
print("-----------------------------------------------")
print("Vinte dimens�es----------------")
print(summary(vinte$gbests))
print("Standard Deviation")
print(sd(vinte$gbests))
print("-----------------------------------------------")
print("Trinta dimens�es----------------")
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

#Averigua��o de normalidade
print("Premissa de normalidade:")
sha = shapiro.test(modelo$residuals)
print(sha)
print("-----------------------------------------------")

#Averigua��o de homocedasticidade
print("Premissa de homocedasticidade:")
fli = fligner.test(gbests~dimensao, data = amostra)
print(fli)
print("-----------------------------------------------")

plot(x    = modelo$fitted.values,
     y    = modelo$residuals,
     xlab = "Vari�ncias",
     ylab = "Amostras")

#Averigua��o de Independ�ncia
durbinWatsonTest(modelo)

plot(x    = seq_along(modelo$residuals),
     y    = modelo$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Ordem dos res�duos",
     ylab = "Valor dos res�duos")

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
#DERIVA��O DOS INTERVALOS DE CONFIAN�A----------------------------
#Compara��o TODOS X TODOS
sistema_tukey <- glht(modelo, 
                      linfct = mcp(gbests = "Tukey"))
sistema_tukey_CI <- confint(sistema_tukey, 
                            level = (1-alpha))

print("-----------------------------------------------")
print("APLICACAO DE TUKEY TODOS CONTRA TODOS")
print(sistema_tukey_CI)
print("-----------------------------------------------")

#titulo = paste("Compara��es com",(1-alpha)*100,"% de n�vel de confian�a")
titulo=""
par(mar = c(5,12,4,2))
plot(sistema_tukey_CI, 
     xlab       = "Gbests",
     xlim       = c(-0.5,1),
     main=titulo)

