# Teste Censo Escolar 08/08/2024


setwd("D:/Users/sophi/Documents/PosDoc/Educacao")

# ABRINDO O ARQUIVO

library(readxl)
library (tidyverse)
library(clipr)
library(dplyr)
library (readxl)
library (tibble)
library (geobr)
library(sf)

INEP_CE_2010_2020_div <- read_excel("D:/Users/sophi/Documents/PosDoc/Educacao/INEP_CE-_2010_2020_div.csv/INEP_CE-_2010_2020_div.xlsx")

# RENOMEANDO PRA N�o ESTRAGAR O ARQUIVO ORIGINAL- LEMBRAR DE HABILITAR nos Pacotes a fun��o dplyr

TesteCensoEscolar <- INEP_CE_2010_2020_div ## nao precisa fazer isso

#####Transformando em DataFrame para funcionar melhor

TesteCensoEscolar <- as.data.frame(TesteCensoEscolar) ##dataframe � janela de dados, � mais eficiente p manipular os dados

## Falram q tibble � melhor, transformando em tibble ?????

# as_tibble(TesteCensoEscolar) ??????????


head (TesteCensoEscolar)

#### Filtrando apenas as vari�veis que vou usar no Painel (exemplo usado o pacote do dplyr), 
### onde filter � para linhas e select � colunas _ https://www.luisotavio.pro/blog/como-filtrar-os-seus-dados-no-r/

####Escolhendo as colunas

TesteCensoEscolar <- TesteCensoEscolar %>% select(NU_ANO_CENSO,NU_IDADE,TP_SEXO,
                                          TP_NACIONALIDADE,NOME_PAIS_CE,TP_ETAPA_ENSINO,
                                          CO_UF,CO_MUNICIPIO,TP_DEPENDENCIA)
                            
head (TesteCensoEscolar)

summary(TesteCensoEscolar)

##### RENOMEANDO OS NOMES DAS COLUNAS

TesteCensoEscolar <- TesteCensoEscolar %>% rename (AnoCenso= NU_ANO_CENSO,
                                          Idade= NU_IDADE,Sexo=TP_SEXO,
                                         Nacionalidade=TP_NACIONALIDADE,
                                         PaisOrigem=NOME_PAIS_CE,EtapaEnsino=TP_ETAPA_ENSINO,
                                         UF_Escola=CO_UF,MunicipioEscola=CO_MUNICIPIO,
                                         DependenciaAdm=TP_DEPENDENCIA)

head (TesteCensoEscolar)

#####  RENOMEANDO (RECODIFICANDO) AS VARI�VEIS. NESSE EXEMPLO, 1 = MASCULINO, 2= FEMININO. PELO O QUE ENTENDI � PRECISO
#### TRANSFORMAR EM FATOR E DEPOIS MUDAR O LABEL.


###### FATOR NO R � UMA REPRESENTA��O CATEG�RICA, OU SEJA, O N�MERO N�O � NUM�RICO. PRECISO USAR O FACTOR PARA DIZER
##### EXPLICITAMENTE QUE ESSE N�MERO � UMA CATEGORIA E N�O UM VALOR NUM�RICO

TesteCensoEscolar$Sexo <- factor(x = TesteCensoEscolar$Sexo,
                          levels = c(1, 2),
                          labels = c("Masculino", "Feminino"))

##Primeiro agrupo as Etapas de Ensino para depois renome�-las 


TesteCensoEscolar <- TesteCensoEscolar %>%
  mutate(EtapaEnsino = case_when(
    EtapaEnsino %in% c(1,2) ~ "Educa��o Infantil",
    EtapaEnsino %in% c(4, 5, 6, 7) ~ "Fundamental I de 8 anos (1a � 4a s�rie)",
    EtapaEnsino %in% c(8, 9, 10, 11) ~ "Fundamental II de 8 anos (5a � 8a s�rie)",
    EtapaEnsino %in% c(14, 15, 16, 17, 18) ~ "Fundamental I (1o ao 5o ano)",
    EtapaEnsino %in% c(19, 20, 21, 41) ~ "Fundamental II (6o ao 9o ano)",
    EtapaEnsino %in% c(25, 26, 27, 28) ~ "Ensino M�dio",
    EtapaEnsino == 29 ~ "Ensino M�dio-N�o Seriada",
    EtapaEnsino %in% c(35, 36, 37, 38) ~ "Ensino M�dio Normal",
    EtapaEnsino %in% c(43, 44, 45, 46, 47, 48, 61, 62, 63, 65, 69, 70, 71, 72) ~ "EJA",
    EtapaEnsino %in% c(39, 40, 60, 67, 74) ~ "Curso T�cnico concomitante, subsequente e integrado/EJA",
    EtapaEnsino %in% c(30, 31, 32, 33, 34) ~ "Curso T�cnico integrado/E.M",
    TRUE ~ "Outra Categoria"
  ))


head (TesteCensoEscolar)

### Teste recodificando a vari�vel depend�ncia administrativa

TesteCensoEscolar$DependenciaAdm <- factor(x = TesteCensoEscolar$DependenciaAdm,
                                 levels = c(1,2,3,4),
                                 labels = c("Federal", "Estadual","Municipal",
                                            "Privada"))
head (TesteCensoEscolar)


### library(geobr) TESTE Para trocar o codigo do Mun pelo nome, atrav?s do pacote GEOBR

#### s eu crio o meu dicion?rio de dados com o c?digo e nome do munic?pio, com o GEOBR

s <- lookup_muni(code_muni = "all") #### o "s" pode ser qualquer coisa, � s� um nome qualquer.
                                    #### posso nomear de qualquer forma. o resto desse c�digo �
                                    ##### vinculado ao pacote geobr
head(s)

##merge(TesteCensoEscolar, s, by.x = 'MunicipioEscola', by.y = 'code_muni')

CensoEscolar <-TesteCensoEscolar

CensoEscolar <-merge(CensoEscolar, s, by.x = 'MunicipioEscola', by.y = 'code_muni', all.x= TRUE)

#### a especifica��o de all.x= TRUE mant�m todas as observa��es, mesmo aquelas que n�o
### tem correspond�ncia exata. Por algum motivo, quando eu dei o merge sem esse all.x=TRUE, o n�mero
### de observa��es diminuiu, por isso fiz assim

head(CensoEscolar)


### LIMPANDO A TABELA NOVA FEITA ATRAV�S DO MERGE, S� COM AS COLUNAS DE INTERESSE

CensoEscolar <- CensoEscolar %>% select(AnoCenso, Idade, Sexo, Nacionalidade,
                                                  PaisOrigem,EtapaEnsino,DependenciaAdm,name_muni,
                                                  name_state)
head(CensoEscolar)



#### Nome dos pa�ses est� desatualizado, est� errado. Exemplo. Eti�pia est� como Abissinia, Portugal, como A�ores
#### fAzendo o Join com a tabela de nome dos pa�ses atualizado

## Primeiro carregando a tabela com os nomes corretos

conversor_paises <- read_xls("D:/Users/sophi/Documents/PosDoc/Conversor_de_pa�ses_e_continentes_Geral.xls")


### Tentando fazer o merge, com a especifica��o de all.x= TRUE para manter todas as observa��es, mesmo aquelas que n�o
### tem correspond�ncia exata


CensoEscolar <- merge( CensoEscolar,conversor_paises, by.x= "PaisOrigem", 
                       by.y = "Pa�ses_bancos_origem",all.x= TRUE)



### site para compatibilizar c�dgio de munic�pio do IBGE de 6 e 7 d�gitos
##https://discourse.curso-r.com/t/compatibilizar-codigo-de-municipio-do-ibge-de-6-e-7-digitos/2124



### exportando o resultado

library(openxlsx)

write.xlsx(CensoEscolar, "D:/Users/sophi/Documents/PosDoc/Educacao/CensoEscolar_2010a2019.xlsx")


#### Filtrando s� a nacionalidade Venezuelana

CensoEscolar_Venezuelanos <- subset (CensoEscolar, PaisOrigem == "VENEZUELA")

write.xlsx(CensoEscolar_Venezuelanos, "D:/Users/sophi/Documents/PosDoc/Educacao/CensoEscolar_Venezuelanos.xlsx")










#### pacote e fun��o abaixo serve para exportar para o EXCEL! Manda rodar o write_clip (demora) e depois � s� dar
# CTRL+V direto no Excel (tamb�m demora para carregar)

library(clipr)

write_clip(TesteCensoEscolar)

####Note que por default, o separador decimal usado � o ponto. Para mudar isso, basta usar o argumento dec = ","

## write_clip(NOMETABELA, dec = ",")
