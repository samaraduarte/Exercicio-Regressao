library(readxl)

setwd("C:/Users/Samara/Desktop/Arquivos 2020 02/01 - setimo periodo/Analise de dados e big data (pratica)/aula dia 11-11")

Dados_Vinho <- read.csv("Dados_qualidade_vinho.csv", header=TRUE, sep=";", dec=",",colClasses=c("factor","numeric","numeric","numeric","numeric","numeric","integer","integer","numeric","numeric","numeric","numeric","factor"),na.strings=c("."))

attach(Dados_Vinho)

summary(Dados_Vinho)

library(ggplot2)
ggplot(data=Dados_Vinho, mapping = aes(x = Concetracao.final.de.alcool, y = Tipo.de.vinho, Acidez.fixa, Acidez.volatil, Acido.citrico, Acucar.residual, Cloretos, Dioxido.de.enxofre.livre, Dioxido.de.enxofre.total, Densidade, pH, Sulfatos)) + geom_point() + geom_smooth(method = lm, se = FALSE)


# outra forma de fazer o grafico
scatter.smooth(Concetracao.final.de.alcool ~ Tipo.de.vinho)


modelo.linear <- lm(Y ~ X1 + X2 + X3 + … + Xn, data = data_set)
summary(modelo.linear)


modelo.alcool <- lm(Concetracao.final.de.alcool~ Tipo.de.vinho + Acidez.fixa + Acidez.volatil + Acido.citrico + Acucar.residual + Cloretos + Dioxido.de.enxofre.livre + Dioxido.de.enxofre.total + Densidade + pH + Sulfatos)

summary(modelo.alcool)



modelo.logistico <- glm(Vinho.de.boa.qualidade~ Tipo.de.vinho + Acidez.fixa + Acidez.volatil + Acido.citrico + Acucar.residual + Cloretos + Dioxido.de.enxofre.livre + Dioxido.de.enxofre.total + Densidade + pH + Sulfatos + Concetracao.final.de.alcool, family = binomial)

summary(modelo.logistico)

modelo.probs <- predict(modelo.logistico,type = "response")

modelo.pred <- ifelse(modelo.probs > 0.5, "Sim", "Nao")

table(modelo.pred, Vinho.de.boa.qualidade)

mean(modelo.pred == Vinho.de.boa.qualidade)



