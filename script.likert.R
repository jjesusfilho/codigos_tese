library(reshape)
library(likert)
library(tidyverse)
library(psych)
library(ggplot2)
library(cowplot)
library(sjPlot)
#Após baixar os arquivos para a pasta específica, fixar o diretório.
setwd('/Users/josedejesusfilho/Dropbox/Governo prisional/Questionario/Limesurvey/ls')

#Carregar os arquivos, o banco de dados terá o nome de data.
source('survey_374445_R_syntax_file.R',encoding="UTF-8")

load("~/Dropbox/Governo prisional/Questionario/Limesurvey/ls/dataset.RData")  

save.image("~/Dropbox/Governo prisional/Questionario/Limesurvey/ls/dataset.RData")

# Primeiramente, iremos plotar o total de unidades versus o gênero.

Importar

sjt.pca(dir_efa, nmbr.fctr = 7)



#tabela de endereços

ender_sap<-read.xlsx2(file.choose(),header=T,1)
# separar prisões
pris<-ender_sap[c(1:159),]

# Plotar frequencias de mulheres conforme a unidade

sjt.xtab(pris$sexo,pris$tipo, variableLabels = c("Sexo","Tipo de unidade"),
         valueLabels = list(c("Feminino","Masculino"),
                            levels(pris$tipo)),showExpected = TRUE,tdcol.expected = "red")

#### 

#"Não discordo,nem concordo" está grafado de duas maneiras distintas. Corrigir isso.
data1<-data
data1<- ddply(data1,names(data.likert), levels=c("Discordo totalmente","Discordo","Mais discordo que concordo", "Não discordo, nem concordo", "Mais concordo que discordo", "Concordo", "Concordo totalmente"))
data1<-as.data.frame(data1) # transformou-se em lista, retornar a dataframe.
names(data1)<-attributes(data)$variable.labels

#data<-data1
#rm(data1)

# Os itens likert precisam estar com sete níveis, o procedimento a seguir ajusta esse problema

meusniveis <- c("Discordo totalmente","Discordo","Mais discordo que concordo", "Não discordo, nem concordo", "Mais concordo que discordo", "Concordo", "Concordo totalmente")
tryCatch({
  lbad <- likert(datalikert)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(datalikert, class) #Verify that all the columns are indeed factors
sapply(datalikert, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(datalikert)) {
  datalikert[,i] <- factor(datalikert[,i], levels=meusniveis)
}
# o procedimento do ajuste dos níveis termina aqui.

#remover a variável id
data.likert$id<-NULL


## Trabalhando com as variáveis independentes
#Criando grupos
#Incluir as variáveis independentes
datalikert$idade<-data$G3Q00001 # idade dos diretores
datalikert$sexo<-data$G3Q00006 # sexo dos diretores
datalikert$seguranca<-data$G3Q00010 #nível de segurança da unidade
datalikert$unidades<-data$G3Q00004 #número de unidades onde já trabalhou
datalikert$tempo<-data$G3Q00002 # tempo de ingresso na administração
#Plotar os gráficos conforme os grupos
idade<-likert(datalikert[c(2,4,5)], grouping=datalikert$idade)
titulo<-"Orientação em relaçãos aos presos conforme a idade dos diretores"
plot(idade)+ggtitle(titulo)



## subdividindo as questões

disciplina<-subset(data.likert[]) # Selecionado o grupo orientação disciplinar
names(disciplina)<-c("A disciplina é a melhor forma de fazer os presos respeitarem as ordens dos agentes e diretores","Não dá para confiar nos presos","")
dislikert<-likert(disciplina)
plot(dislikert)




#Verificando o coeficiente alpha de confiabilidade
ndisciplina<-sapply(disciplina,as.numeric) # transforma toda a dataframe em numérico para cálculo do alpha
alpha(ndisciplina)

# Poder sobre os presos e agentes em relação a outros grupos. Esse grupo de questões procura saber 
poder<-subset(data1[c(30,47,49,50,74)])
names(poder)<-c("Atualmente, as facções exercem muito poder sobre a população presa", "Há muito pouco que eu posso fazer contra um agente penitenciário que causa problemas na unidade prisional","O diretor deveria ter mais autonomia para administrar a prisão","Muitos aspectos da dinâmica da unidade são decididos pela Coordenação Regional ou pelo gabinete da SAP","Há muito pouco que eu posso fazer contra um agente penitenciário que causa problemas na unidade prisional")

poderlikert<-likert(poder)
plot(poderlikert,low.color="#006691",high.color="#9A0038", text.size=4)+ggtitle("Percepção dos diretores sobre os limites de seu controle")+
  theme(
    axis.text.y=element_text(size=15, face="bold")
  )
ggsave("l_poder.png", width=20,height=9)


#Verificando o coeficiente alpha de confiabilidade
npoder<-sapply(poder,as.numeric) # transformando de factor para númerico
npoder<-as.data.frame(npoder) # retransformar em dataframe
alpha(npoder)

##Notas

#Objetivos
objetivos<-data1[c(15:20)]
names(objetivos)<-c("Reforçar o respeito às leis","Ressocializar","Impedir o cometimento de novos crimes", "Proteger a sociedade","Dar exemplo para potenciais criminosos","Punir")
plot(likert(objetivos))+
  scale_fill_manual(values=pal8(8))+
  theme(
    axis.text.y=element_text(size=15,face="bold")
  )+
  ggtitle("Qual desses objetivos você considera mais importantes para a administração prisional?")

ggsave("ggobjetivos.png",width = 12,height = 5)

#Ajusta os níveis para plotagem

tryCatch({
  lbad <- likert(objetivos)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(objetivos, class) #Verify that all the columns are indeed factors
sapply(objetivos, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(objetivos)) {
  objetivos[,i] <- factor(objetivos[,i], levels=nineisnum)
}

#Plotar
#
objetivos$sexo<-datalikert$sexo
oblikert<-likert(subset(objetivos[c(1,2,3,4,5,6)]),grouping=objetivos$sexo)
plot(oblikert)+ggtitle("Qual desses objetivos você acha mais importante para a administração prisonal?")
ggsave("objetivos.png",width = 12)

#Grupos
grupos<-data[c(21:29)]
names(grupos)<-c("Presos","Familiares de presos", "Agentes penitenciários","Pessoal Administrativo","Corpo técnico","Coordenação regional","Gabinete da SAP","Juízes","Promotores")
grulikert<-likert(grupos) 
plot(grulikert) +
scale_fill_manual(values=pal8(8))+
ggtitle("Com qual desses grupos você acha mais fácil trabalhar?")
ggsave("grupos2.png")

#Desempenho
desempenho<-data1[c(59:72)]
n_des<-sapply(desempenho, as.numeric)
names(desempenho)<-c("Gabinete do Secretário da SAP","Coordenadoria Regional","Coordenadoria de Saúde","Coordenadoria de Reintegração Social","Escola de Administração Penitenciária","Grupo de Intervenção Rápida","AEVPs","Corregedoria da SAP","Ouvidoria da SAP","Conselho Penitenciário", "Juiz da Execução","Ministério Público","Defensoria Pública","Funap")
deslikert<-likert(desempenho)
plot(deslikert)+
  ggtitle("Como você classificaria o desempenho dos seguintes órgãos ou grupos?")+
  theme(axis.text.y=element_text(size=10),
        plot.title=element_text(size=10))

ggsave("desempenho.png",width = 10, height=6)

sjt.df(desempenho, stringVariable = "Órgão ou grupo", title="Como você classificaria o desempenho dos seguintes órgãos ou grupos?", orderColumn = "mean", orderAscending = FALSE)
sjt.xtab(data1[c(59:72)],data1[106],data1[111])

library(ggthemr)

sjp.likert(sapply(n_des,factor), value.labels = "sum.outside",  geom.colors = "RdBu", showPercentageSign = TRUE, includeN = FALSE)

#Ordem
ordem<-data[c(89:99)]
names(ordem)<-c("A presença de organização interna de presos","A autoridade do agente penitenciário","A negociação com os presos diante de crises","O ingresso regular do GIR","As revistas periódicas nas celas","O serviço de inteligência interno","O receio dos presos de serem transferidos","O receio dos presos de não progredirem","Acordos com presos e o seu cumprimento","A ocupação dos presos em oficinas de trabalho e estudo","A visita de familiares nos fins de semana")
ordlikert<-likert(ordem)
plot(ordlikert,low.color="#006691",high.color="#9A0038")+
  theme(
         axis.text.y=element_text(size=16, face="bold")
          )+
  ggtitle("Quais dessas medidas, na sua opinião, contribuem para manter a ordem na unidade prisional?")

ggsave("ordem.png",width = 14, height = 7)

sjt.df(ordem,orderColumn = "mean")
# distribuição das variáveis independentes

# idade:

ggidade<-ggplot(data,aes(x=G3Q00001, fill= G3Q00001))+
geom_bar()+
labs(x="Idade",y="Anos", title="Idade dos diretores")+
  stat_bin(geom = "text",
           aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
           vjust = -.5, face="bold")+
scale_fill_discrete(name="idade")
ggidade

ggsave("ggidade.png", width = 15, height = 8)

# sexo

ggsexo<-ggplot(datalikert,aes(x=sexo, fill=sexo))+
  geom_bar()+
  labs(x="sexo",y="número de unidades prisionais", title="Sexo dos diretores das unidades prisionais")+
  #scale_y_discrete(breaks=seq(0,60,5))+
  scale_fill_manual(values=paleta7(2))+
   #theme(
  #  #axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
   # #axis.ticks.x = element_blank(),axis.text.x = element_blank(),# get rid of y ticks/text
  #  panel.grid.major = element_blank(),
   # #panel.grid.minor = element_blank(),
  #  panel.border = element_blank(),
  # panel.background = element_blank(),
   # plot.title = element_text(lineheight=.8, face="bold", vjust=2, size=rel(1.5)))  # make title bold and add space
  
ggsexo

# Tempo no serviço prisional

ggtempo<-ggplot(data,aes(x=G3Q00002, fill=G3Q00002,))+
geom_bar()+
  labs(x="Tempo de serviço prisional",y="Número de respondentes", title="Tempo em anos desde que o diretor ingressou no serviço prisional")+
  scale_y_discrete(breaks=seq(0,60,5))+
  stat_bin(geom = "text",
           aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
           vjust = -.5, face="bold")+
  scale_fill_manual(values=paleta7(5), name="Intervalos")+
  theme(
    #axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
    #axis.ticks.x = element_blank(),axis.text.x = element_blank(),# get rid of y ticks/text
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(lineheight=.8, face="bold", vjust=2, size=rel(1.5)))# make title bold and add space
ggtempo
ggsave("ggtempo.png", width = 15, height = 8)


sjt.frq(data$G3Q00002)

sjt.xtab(data$G3Q00002,data$G3Q00010)


# Nível de segurança das unidades

ggplot(data,aes(x=ordered(G3Q00010),y=..count.., fill=G3Q00010))+
geom_bar(stat="count")+
labs(x="Nível de segurança",y="Número de respondentes")+
scale_y_discrete(breaks=seq(0,60,5))+
stat_count(geom = "text",
aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
vjust = -.5)+
scale_fill_brewer(type="seq", palette=1,name="Nível de segurança")+
theme(
legend.position = "bottom",
panel.border = element_blank(),
panel.background = element_blank(),
plot.title = element_text(lineheight=.8, face="bold", vjust=2, size=rel(1.5)))# make title bold and add space






# Risco dos presos 

ggrisco<-ggplot(data,aes(x=reorder_size(G1Q00006), y=..count..,fill=G1Q00006))+
  geom_bar(stat="count")+
  labs(x="Risco oferecido",y="Número de respondentes")+
  scale_y_discrete(breaks=seq(0,60,5))+
  scale_fill_brewer(type="seq", palette=1,name="Nível de segurança",direction=-1)+
    guides(fill=FALSE)+
  stat_count(geom = "text",
             aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
             vjust = -.5)
  
ggsave("~/Desktop/TESE/figure/gg_risco.pdf",width=10,height=7)

ggrisco


ggsave("ggrisco.png", width = 16, height = 9)
sjt.frq(data1[11])

sjt.xtab(data$G1Q00006,data$G3Q00010,variableLabels = c("Risco dos presos","Segurança da unidade"), showSummary = FALSE,showLegend = FALSE)

# Convivio com faccção

data$G1Q00003<-ifelse(is.na(data$G1Q00003),"outros",data$G1Q00003)
ggfac<-ggplot(data, aes(x=G1Q00003,y=..count..,fill=G1Q00003,order=G1Q00003))+
geom_bar(width=.5,stat="count")+
labs(x="Facção",y= "Número de respondentes")+
scale_fill_brewer(type="qual",palette=4)+
guides(fill=FALSE)+
stat_count(geom = "text",
         aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
         vjust = -.8)


ggfac


ggsave("gg_fac.pdf", width = 10, height = 7)



## Situação processual:
G1Q00004

ggproc<-ggplot(data, aes(x=G1Q00004,y=..count..,fill=G1Q00004))+
  geom_bar(stat="count")+
  labs(x="Situação processual",y= "Número de respondentes")+
  scale_y_discrete(breaks=seq(0,60,5))+
  scale_fill_brewer(type="qual",palette=4)+
  guides(fill=FALSE)+
  scale_x_discrete(labels=str_wrap(levels(data$G1Q00004),20))+
  stat_count(geom = "text",
           aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
           vjust = -.8)



ggsave("gg_proc.pdf", width = 12, height = 9)



prox<-data1[c(33,34,35,38,55)]
l_prox<-likert(prox)
plot(l_prox,low.color="#006691",high.color="#9A0038", text.size=4)+
  theme(
    axis.text.y=element_text(size=16, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))+
  ggtitle("Proximidade e diálogo com os presos")
#scale_fill_manual(values=pal7(7))
ggsave("l_prox.png", width=20, height=9)


a_prox<-as.data.frame(sapply(prox,as.numeric))
attributes(prox)$variable.labels<-attributes(data)$variable.labels[c(33,34,35,38,44,55)]
alpha(a_prox, check.keys = TRUE)






# Poder próprio
proprio<-data[c(30,74,50,49,87)]
names(proprio)<-str_wrap(dic[c(30,74,50,49,87),2],10)

plot(likert(proprio),low.color="#006691",high.color="#9A0038", text.size=2)+
  
  theme(
  axis.text.y=element_text(size=12, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))
  
ggsave("l_proprio.pdf", width=12, height=7)

# Tabela
sjt.df(proprio,
altr.row.col = TRUE,
      # title="Percepção dos diretores sobre sua autonomia em relação a outros grupos", 
sort.col = "mean",
sort.asc = FALSE,
show.cmmn.row=TRUE,
string.cmmn="Alfa de Cronbach=0,47")

attributes(proprio)$variable.labels<-attributes(data)$variable.labels[c(30,74,50,49,87)]
proprio<-as.data.frame(sapply(proprio,as.numeric))
cronbach(proprio)

auto<-data1[c(30,50,47,41,57,85)]
attributes(auto)$variable.labels<-attributes(data)$variable.labels[c(30,50,47,41,57,85)]
auto1<-as.data.frame(sapply(auto,as.numeric))
cronbach(auto1)

# Proximidade e diálogo

prox<-34, 35, 38, 44, 55



# Autoimagem

imagem<-data[c(56,57,58)]
names(imagem)<-dic[c(56,57,58),2]
attributes(prox.n)$variable.labels<-attributes(data)$variable.labels[c(56,57,58)]
alpha(imagem)

plot(likert(imagem),low.color="#006691",high.color="#9A0038", text.size=4)+
  theme(
  axis.text.y=element_text(size=12, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))

ggsave("l_imagem.pdf", width = 20,height=10)



# Policiais

plot(likert(imagem,grouping = data$G3Q00002),low="#006691",high="#9A0038", text.size=4,neutral.color = "#E2E2E2")+
  ylab("Percentual")+
       #scale_fill_manual(values=pal8(7))+
       theme(
         axis.text.y=element_text(size=15, face="bold")
       )+
  ggtitle("Interesse em ser policial conforme a idade")
      
   ggsave("l_policial_tempo.pdf", width=10, height = 7)



# Visão sobre os agentes penitenciários


agentes<-data[c(73,74,75,76,77,53)]

names(agentes)<-dic[c(73,74,75,76,77,53),2]

l_agentes<-likert(agentes)
plot(l_agentes, low.color="#006691",high.color="#9A0038", text.size=4)+
  theme(
    axis.text.y=element_text(size=16, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))+
  ggtitle("Proximidade e confiança nos agentes penitenciários")
ggsave("l_agentes.pdf",width = 12,height=7)





# Condições


dir<-data[c(37,32, 36,39,40,43)]
names(dir)<-dic[c(37,32, 36,39,40,43),2]

l_dir<-likert(dir)
plot(l_dir, low.color="#006691",high.color="#9A0038", text.size=4)+
  theme(
    axis.text.y=element_text(size=16, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))

+
  ggtitle("Sobre as condições de vida e direitos dos presos")

ggsave("l_dir.png",width = 20,height=9)

sjt.df(dir, 
       alternateRowColors = TRUE,
       title="Percepção dos diretores sobre os direitos dos presos", 
       orderColumn = "mean",
       orderAscending = FALSE,
       showCommentRow=TRUE)

# Direitos dos presos


# Grupos externos


b<-lapply(data[c(80,81,82,84,85,86)], as.numeric)
b<-as.data.frame(b)
cronbach(b)

# likert
# 
externo<-data[c(80,81,82,84,85,86)]
names(externo)<-dic[c(80,81,82,84,85,86),2]

l_externo<-likert(externo)
plot(l_externo, low.color="#006691",high.color="#9A0038", text.size=4)+
  theme(
    axis.text.y=element_text(size=15, face="bold"))+
  guides(fill=guide_legend(title="Respostas"))

ggsave("l_externo.pdf", width=12, height=8)

sjt.df(data1[c(80,81,82,84,85,86)],showCommentRow = TRUE, commentString = "Alfa de Crombach=0,78", stringVariable ="Itens",orderColumn = "mean")





