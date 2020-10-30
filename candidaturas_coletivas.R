# Packages ----
rm(list=ls()); cat("\014")

packages<-c("cepespR", "readr", "data.table", "stringr", 
            "ggplot2", "tidyverse", "gridExtra")
lapply(packages, require, character.only=T)

# 2012 ----
vars1<-c("SIGLA_UE", "NOME_CANDIDATO", "NUMERO_CANDIDATO",
        "NOME_URNA_CANDIDATO", "DES_SITUACAO_CANDIDATURA", "SIGLA_PARTIDO",
        "DATA_NASCIMENTO", "CODIGO_SEXO", "DESCRICAO_GRAU_INSTRUCAO",
        "DESCRICAO_COR_RACA", "DESC_SIT_TOT_TURNO")
  
df12<-get_candidates(2012, "Vereador", 
                     columns_list = vars1, cached=T)
head(df12)

vars2<-c("SIGLA_UE", "UF", "NOME_CANDIDATO", "NUMERO_CANDIDATO", "DATA_NASCIMENTO", "NOME_MUNICIPIO", "QTDE_VOTOS")
df12_votes<-get_elections(2012, "Vereador", regional_aggregation="Municipality",
                    political_aggregation="Candidate", 
                    columns_list = vars2, cached=T)
head(df12_votes)

df12<-merge(df12, df12_votes, 
            by=c("SIGLA_UE", "NOME_CANDIDATO", "NUMERO_CANDIDATO", "DATA_NASCIMENTO"),
            all.x=T)
table(is.na(df12$QTDE_VOTOS))
df12$QTDE_VOTOS[is.na(df12$QTDE_VOTOS)]<-0

# 2016 ----
df16<-get_candidates(2016, "Vereador", 
                     columns_list = vars1, cached=T)
head(df16)

df16_votes<-get_elections(2016, "Vereador", regional_aggregation="Municipality",
                          political_aggregation="Candidate", 
                          columns_list = vars2, cached=T)
head(df16_votes)

df16<-merge(df16, df16_votes, 
            by=c("SIGLA_UE", "NOME_CANDIDATO", "NUMERO_CANDIDATO", "DATA_NASCIMENTO"),
            all.x=T)
table(is.na(df16$QTDE_VOTOS))
df16$QTDE_VOTOS[is.na(df16$QTDE_VOTOS)]<-0

# 2020 ----
setwd("/Users/guilhermerusso/Downloads")

#download.file("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2020.zip",
#              destfile = "consulta_cand_2020.zip")
#unzip("consulta_cand_2020.zip", 
#      exdir = "consulta_cand_2020")

list.files(path="./consulta_cand_2020/")
df20<-read.csv("consulta_cand_2020/consulta_cand_2020_BRASIL.csv", sep=";",
               encoding="latin1")
names(df20)

table(df20$NM_TIPO_ELEICAO)
table(df20$DS_CARGO)
df20<-df20 %>% filter(NM_TIPO_ELEICAO=="ELEIÇÃO ORDINÁRIA" & 
                        DS_CARGO=="VEREADOR")

vars2<-c("SG_UE", "NM_CANDIDATO", "NR_CANDIDATO",
        "NM_URNA_CANDIDATO", "DS_SITUACAO_CANDIDATURA", 
        "SG_PARTIDO", "DT_NASCIMENTO", "CD_GENERO", "DS_GRAU_INSTRUCAO",
        "DS_COR_RACA", "DS_SITUACAO_CANDIDATO_PLEITO", "SG_UF", "NM_UE")
df20<-df20 %>% select(vars2)
head(df20)

names(df20)<-c("SIGLA_UE", "NOME_CANDIDATO", "NUMERO_CANDIDATO",
               "NOME_URNA_CANDIDATO", "DES_SITUACAO_CANDIDATURA", "SIGLA_PARTIDO",
               "DATA_NASCIMENTO", "CODIGO_SEXO", "DESCRICAO_GRAU_INSTRUCAO",
               "DESCRICAO_COR_RACA", "DESC_SIT_TOT_TURNO", "UF", "NOME_MUNICIPIO")
df20$QTDE_VOTOS<-NA

# Merging and coding ----
df12$ano<-2012; df16$ano<-2016; df20$ano<-2020

dat<-rbind(df12, df16, df20)

dat %>% group_by(ano) %>% summarize(n())

# Creating a variable for when mentioned
dat<-dat %>% mutate(bancada=grepl("BANCADA", NOME_URNA_CANDIDATO),
                    ativista=grepl("ATIVISTA", NOME_URNA_CANDIDATO),
                    coletiva=grepl("COLETIVA", NOME_URNA_CANDIDATO),
                    coletivo=grepl("COLETIVO", NOME_URNA_CANDIDATO),
                    mandato=grepl("MANDATO", NOME_URNA_CANDIDATO),
                    mandata=grepl("MANDATA", NOME_URNA_CANDIDATO),
                    cand_colet=grepl("BANCADA|COLETIVA|COLETIVO|MANDATO|MANDATA", 
                                     NOME_URNA_CANDIDATO))

head(dat)

# Counting number of times words are used to plot ----
plot<-dat %>% group_by(ano) %>% summarize(colet=sum(cand_colet),
                                           bancada=sum(bancada),
                                           ativista=sum(ativista),
                                           coletiva=sum(coletiva),
                                           coletivo=sum(coletivo),
                                           mandato=sum(mandato),
                                           mandata=sum(mandata))
plot$ano<-as.character(plot$ano)

# Plot 1: Total number----

# Colors
cor_cepesp1= rgb(28, 47, 103, maxColorValue = 255)
cor_cepesp2= rgb(0, 150, 214, maxColorValue = 255)
cor_cepesp3= rgb(178, 178, 178, maxColorValue = 255)

ggplot(plot, aes(x=ano, y=colet, fill=ano,
            label=paste(round(colet, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="Candidaturas Coletivas") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 5, size=5)

# Plot 2: per word----
p1<-ggplot(plot, aes(x=ano, y=bancada, fill=ano,
                  label=paste(round(bancada, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="BANCADA") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

p2<-ggplot(plot, aes(x=ano, y=coletiva, fill=ano,
                     label=paste(round(coletiva, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="COLETIVA") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

p3<-ggplot(plot, aes(x=ano, y=coletivo, fill=ano,
                     label=paste(round(coletivo, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="COLETIVO") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

p4<-ggplot(plot, aes(x=ano, y=mandata, fill=ano,
                     label=paste(round(mandata, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="MANDATA") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

p5<-ggplot(plot, aes(x=ano, y=mandato, fill=ano,
                     label=paste(round(mandato, 1)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(cor_cepesp3, cor_cepesp2, cor_cepesp1)) +
  #scale_x_discrete(breaks=unique(plot1$ano), labels=c("2012", "2016", "2020")) +
  labs(y="Frequência", x="Ano",
       title="MANDATO") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

grid.arrange(p1, p2, p3, p4, p5, nrow=2)

# Plot 3: per party ----
# Counting number of times words are used to plot
tab<-dat %>% filter(ano==2020 & cand_colet) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarize(party=n())
  
source("/Users/guilhermerusso/Dropbox (Personal)/CEPESPgraphs/Scripts/Function_Palettes.R")

tab %>% mutate(SIGLA_PARTIDO = fct_reorder(SIGLA_PARTIDO, party)) %>%
         ggplot(aes(x=SIGLA_PARTIDO, y=party, fill=as.factor(party),
                 label=paste(round(party, 1)))) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_cepesp(reverse = T) +
  labs(y="Frequência", x="",
       title="Candidaturas Coletivas por Partido") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "1",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_text(color="black"),
    #axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

# Plot 4 per party and state----
tab<-dat %>% filter(ano==2020 & cand_colet & grepl("PT$|PSOL", SIGLA_PARTIDO)) %>% 
  group_by(UF, SIGLA_PARTIDO) %>% 
  summarize(party=n()) %>% 
  mutate(desc=paste(SIGLA_PARTIDO, UF))

tab %>% 
  ggplot(aes(x=reorder(desc, party), y=party, fill=as.factor(party),
             label=paste(round(party, 1)))) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_cepesp(reverse = T) +
  labs(y="Frequência", x="",
       title="Candidaturas Coletivas por Partido e UF") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "1",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 3, size=4)

# Plot 5: 2018----
df18_feddep<-get_elections(2018, "Federal Deputy", regional_aggregation="State",
                          political_aggregation="Candidate", 
                          columns_list = vars1, cached=T)
head(df18_feddep)

df18_stadep<-get_elections(2018, "State Deputy", regional_aggregation="State",
                           political_aggregation="Candidate", 
                           columns_list = vars1, cached=T)
head(df18_stadep)

df18<-rbind(df18_feddep, df18_stadep)

df18<-df18 %>% mutate(bancada=grepl("BANCADA", NOME_URNA_CANDIDATO),
                    ativista=grepl("ATIVISTA", NOME_URNA_CANDIDATO),
                    coletiva=grepl("COLETIVA", NOME_URNA_CANDIDATO),
                    coletivo=grepl("COLETIVO", NOME_URNA_CANDIDATO),
                    mandato=grepl("MANDATO", NOME_URNA_CANDIDATO),
                    mandata=grepl("MANDATA", NOME_URNA_CANDIDATO),
                    cand_colet=grepl("BANCADA|COLETIVA|COLETIVO|MANDATO|MANDATA", 
                                     NOME_URNA_CANDIDATO))

table(df18$cand_colet)
df18[df18$cand_colet,]

tab<-df18 %>% filter(cand_colet) %>% 
  mutate(elected=as.numeric(grepl("^ELEITO", DESC_SIT_TOT_TURNO))) %>% 
  group_by(SIGLA_UE, SIGLA_PARTIDO) %>% 
  summarize(party=n(), elected=sum(elected)) %>% 
  mutate(desc=paste(SIGLA_PARTIDO, SIGLA_UE))

tab %>% 
  ggplot(aes(x=reorder(desc, party), y=party, fill=as.factor(party),
             label=paste(round(party, 1)))) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_cepesp(reverse = T) +
  labs(y="Frequência", x="",
       title="Candidaturas Coletivas por Partido e UF") +
  theme_classic() + theme(
    plot.title = element_text(hjust = "1",face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = .2, size=5) +
  annotate("segment", x = 2, xend = 3.3, y = 5, yend = 5.5, colour = "black", 
           size=1, arrow=arrow()) +
  annotate("text", x=2, y=3.8, label="Monica da Bancada\n Ativista é ELEITA\n Deputada Estadual", 
           size=5, colour="black")
  
# Plot 6 per gender and skin color ----

# Counting number of times words are used to plot
tab<-dat %>% filter(cand_colet) %>% 
  mutate(CODIGO_SEXO=ifelse(CODIGO_SEXO==4, "MULHER", "HOMEM")) %>% 
  group_by(ano, CODIGO_SEXO, DESCRICAO_COR_RACA) %>% 
  summarize(n=n()) %>% 
  mutate(desc=paste(CODIGO_SEXO, DESCRICAO_COR_RACA))

tab %>% mutate(desc = fct_reorder(desc, n)) %>%
  ggplot(aes(x=desc, y=n, fill=as.factor(n),
             label=paste(round(n, 1)))) +
  facet_grid(~ano) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_cepesp(reverse = T) +
  labs(y="Frequência", x="",
       title="Candidaturas Coletivas por Sexo e Cor") +
  theme_classic() + theme(
    plot.title = element_text(hjust=4, face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = 5, size=4)

# How big are the cities these candidates are running? ----

# Number of seats per municipality
vagas<-readr::read_csv2("http://cepespdata.io/static/docs/vagas_vereadores.csv")
head(vagas)

vagas$vagas<-vagas$`2016`
sum(vagas$vagas) 

# Fixing codes for merging
vagas$SIGLA_UE<-str_pad(vagas$cod_tse, width = 5, pad = 0)
head(vagas$SIGLA_UE)
dat$SIGLA_UE<-str_pad(dat$SIGLA_UE, width = 5, pad = 0)

nrow(dat)
df<-merge(dat, vagas %>% select(SIGLA_UE, vagas),
      by="SIGLA_UE", all.x=T)
nrow(df)

# Plot 7.1: PSOL ----
tab_psol<-df %>% filter(ano==2020 & cand_colet & SIGLA_PARTIDO=="PSOL") %>% 
  group_by(vagas, SIGLA_UE, NOME_MUNICIPIO) %>% 
  summarize(n=n())

tab_psol$met<-100/tab_psol$vagas

votes2016<-df16 %>% filter(SIGLA_UE %in% tab_psol$SIGLA_UE) %>% 
  group_by(SIGLA_UE) %>% summarize(tot=sum(QTDE_VOTOS))
  
votes2016_psol<-df16 %>% filter(SIGLA_UE %in% tab_psol$SIGLA_UE & SIGLA_PARTIDO=="PSOL") %>% 
  group_by(SIGLA_UE) %>% summarize(psol=sum(QTDE_VOTOS))

votes2016<-merge(votes2016_psol, votes2016, by="SIGLA_UE")

votes2016$psol_pc<-100*votes2016$psol/votes2016$tot

tab_psol<-merge(tab_psol, votes2016 %>% select(SIGLA_UE, psol_pc), by="SIGLA_UE")
tab_psol
tab_psol$above<-tab_psol$psol_pc>tab_psol$met

ggplot(tab_psol, aes(x=vagas, y=psol_pc, color=above)) +
  geom_point(size=2) +
  scale_color_cepesp(reverse=T) +
  geom_line(aes(x=vagas, y=met), linetype="dotted") +
  labs(y="Percentual de Votos em 2016", x="Vagas (2016)",
       title="PSOL: Vagas e Votos em 2016") +
  theme_classic() + theme(
    plot.title = element_text(hjust=0.3, face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  annotate("text", x=53, y=4, label="São Paulo", 
           size=4, colour="black") +
  annotate("text", x=51, y=10, label="Rio de Janeiro", 
           size=4, colour="black") +
  annotate("text", x=21, y=6.5, label="Viamão (RS)", 
         size=4, colour="black") +
  annotate("text", x=43, y=3.5, label="Salvador", 
           size=4, colour="black") +
  annotate("text", x=43, y=0, label="Manaus", 
           size=4, colour="black")

# Plot 7.2: PT ----
tab_pt<-df %>% filter(ano==2020 & cand_colet & SIGLA_PARTIDO=="PT") %>% 
  group_by(vagas, SIGLA_UE, NOME_MUNICIPIO) %>% 
  summarize(n=n())

tab_pt$met<-100/tab_pt$vagas

votes2016<-df16 %>% filter(SIGLA_UE %in% tab_pt$SIGLA_UE) %>% 
  group_by(SIGLA_UE) %>% summarize(tot=sum(QTDE_VOTOS))

votes2016_pt<-df16 %>% filter(SIGLA_UE %in% tab_pt$SIGLA_UE & SIGLA_PARTIDO=="PT") %>% 
  group_by(SIGLA_UE) %>% summarize(pt=sum(QTDE_VOTOS))

votes2016<-merge(votes2016_pt, votes2016, by="SIGLA_UE")

votes2016$pt_pc<-100*votes2016$pt/votes2016$tot

tab_pt<-merge(tab_pt, votes2016 %>% select(SIGLA_UE, pt_pc), by="SIGLA_UE")
tab_pt
tab_pt$above<-tab_pt$pt_pc>tab_pt$met

ggplot(tab_pt, aes(x=vagas, y=pt_pc, color=above)) +
  geom_point(size=2) +
  scale_color_cepesp(reverse=T) +
  geom_line(aes(x=vagas, y=met), linetype="dotted") +
  labs(y="Percentual de Votos em 2016", x="Vagas (2016)",
       title="PT: Vagas e Votos em 2016") +
  theme_classic() + theme(
    plot.title = element_text(hjust=0.3, face = "bold", size=22), 
    #    plot.subtitle = element_text(hjust="0.5", size=10),
    legend.position = "none",
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=14),
    axis.title.x = element_text(size=16),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  annotate("text", x=53, y=15, label="São Paulo", 
           size=4, colour="black") +
  annotate("text", x=43, y=8, label="Salvador", 
           size=4, colour="black") +
  annotate("text", x=40, y=4, label="Manaus", 
           size=4, colour="black") +
  annotate("text", x=11, y=19, label="Mairi (BA)", 
           size=4, colour="black")







