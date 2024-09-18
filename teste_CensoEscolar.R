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

# RENOMEANDO PRA Não ESTRAGAR O ARQUIVO ORIGINAL- LEMBRAR DE HABILITAR nos Pacotes a função dplyr

TesteCensoEscolar <- INEP_CE_2010_2020_div ## nao precisa fazer isso

#####Transformando em DataFrame para funcionar melhor

TesteCensoEscolar <- as.data.frame(TesteCensoEscolar) ##dataframe é janela de dados, é mais eficiente p manipular os dados

## Falram q tibble é melhor, transformando em tibble ?????

# as_tibble(TesteCensoEscolar) ??????????


head (TesteCensoEscolar)

#### Filtrando apenas as variáveis que vou usar no Painel (exemplo usado o pacote do dplyr), 
### onde filter é para linhas e select é colunas _ https://www.luisotavio.pro/blog/como-filtrar-os-seus-dados-no-r/

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

#####  RENOMEANDO (RECODIFICANDO) AS VARIÁVEIS. NESSE EXEMPLO, 1 = MASCULINO, 2= FEMININO. PELO O QUE ENTENDI É PRECISO
#### TRANSFORMAR EM FATOR E DEPOIS MUDAR O LABEL.


###### FATOR NO R É UMA REPRESENTAÇÃO CATEGÓRICA, OU SEJA, O NÚMERO NÃO É NUMÉRICO. PRECISO USAR O FACTOR PARA DIZER
##### EXPLICITAMENTE QUE ESSE NÚMERO É UMA CATEGORIA E NÃO UM VALOR NUMÉRICO

TesteCensoEscolar$Sexo <- factor(x = TesteCensoEscolar$Sexo,
                          levels = c(1, 2),
                          labels = c("Masculino", "Feminino"))

##Primeiro agrupo as Etapas de Ensino para depois renomeá-las 


TesteCensoEscolar <- TesteCensoEscolar %>%
  mutate(EtapaEnsino = case_when(
    EtapaEnsino %in% c(1,2) ~ "Educação Infantil",
    EtapaEnsino %in% c(4, 5, 6, 7) ~ "Fundamental I de 8 anos (1a à 4a série)",
    EtapaEnsino %in% c(8, 9, 10, 11) ~ "Fundamental II de 8 anos (5a à 8a série)",
    EtapaEnsino %in% c(14, 15, 16, 17, 18) ~ "Fundamental I (1o ao 5o ano)",
    EtapaEnsino %in% c(19, 20, 21, 41) ~ "Fundamental II (6o ao 9o ano)",
    EtapaEnsino %in% c(25, 26, 27, 28) ~ "Ensino Médio",
    EtapaEnsino == 29 ~ "Ensino Médio-Não Seriada",
    EtapaEnsino %in% c(35, 36, 37, 38) ~ "Ensino Médio Normal",
    EtapaEnsino %in% c(43, 44, 45, 46, 47, 48, 61, 62, 63, 65, 69, 70, 71, 72) ~ "EJA",
    EtapaEnsino %in% c(39, 40, 60, 67, 74) ~ "Curso Técnico concomitante, subsequente e integrado/EJA",
    EtapaEnsino %in% c(30, 31, 32, 33, 34) ~ "Curso Técnico integrado/E.M",
    TRUE ~ "Outra Categoria"
  ))


head (TesteCensoEscolar)

### Teste recodificando a variável dependência administrativa

TesteCensoEscolar$DependenciaAdm <- factor(x = TesteCensoEscolar$DependenciaAdm,
                                 levels = c(1,2,3,4),
                                 labels = c("Federal", "Estadual","Municipal",
                                            "Privada"))
head (TesteCensoEscolar)


### library(geobr) TESTE Para trocar o codigo do Mun pelo nome, atrav?s do pacote GEOBR

#### s eu crio o meu dicion?rio de dados com o c?digo e nome do munic?pio, com o GEOBR

s <- lookup_muni(code_muni = "all") #### o "s" pode ser qualquer coisa, é só um nome qualquer.
                                    #### posso nomear de qualquer forma. o resto desse código é
                                    ##### vinculado ao pacote geobr
head(s)

##merge(TesteCensoEscolar, s, by.x = 'MunicipioEscola', by.y = 'code_muni')

CensoEscolar <-TesteCensoEscolar

CensoEscolar <-merge(CensoEscolar, s, by.x = 'MunicipioEscola', by.y = 'code_muni', all.x= TRUE)

#### a especificação de all.x= TRUE mantém todas as observações, mesmo aquelas que não
### tem correspondência exata. Por algum motivo, quando eu dei o merge sem esse all.x=TRUE, o número
### de observações diminuiu, por isso fiz assim

head(CensoEscolar)


### LIMPANDO A TABELA NOVA FEITA ATRAVÉS DO MERGE, SÓ COM AS COLUNAS DE INTERESSE

CensoEscolar <- CensoEscolar %>% select(AnoCenso, Idade, Sexo, Nacionalidade,
                                                  PaisOrigem,EtapaEnsino,DependenciaAdm,name_muni,
                                                  name_state)
head(CensoEscolar)



#### Nome dos países está desatualizado, está errado. Exemplo. Etiópia está como Abissinia, Portugal, como Açores
#### fAzendo o Join com a tabela de nome dos países atualizado

## Primeiro carregando a tabela com os nomes corretos

conversor_paises <- read_xls("D:/Users/sophi/Documents/PosDoc/Conversor_de_países_e_continentes_Geral.xls")


### Tentando fazer o merge, com a especificação de all.x= TRUE para manter todas as observações, mesmo aquelas que não
### tem correspondência exata


CensoEscolar <- merge( CensoEscolar,conversor_paises, by.x= "PaisOrigem", 
                       by.y = "Países_bancos_origem",all.x= TRUE)



### site para compatibilizar códgio de município do IBGE de 6 e 7 dígitos
##https://discourse.curso-r.com/t/compatibilizar-codigo-de-municipio-do-ibge-de-6-e-7-digitos/2124



### exportando o resultado

library(openxlsx)

write.xlsx(CensoEscolar, "D:/Users/sophi/Documents/PosDoc/Educacao/CensoEscolar_2010a2019.xlsx")


#### Filtrando só a nacionalidade Venezuelana

CensoEscolar_Venezuelanos <- subset (CensoEscolar, PaisOrigem == "VENEZUELA")

write.xlsx(CensoEscolar_Venezuelanos, "D:/Users/sophi/Documents/PosDoc/Educacao/CensoEscolar_Venezuelanos.xlsx")










#### pacote e função abaixo serve para exportar para o EXCEL! Manda rodar o write_clip (demora) e depois é só dar
# CTRL+V direto no Excel (também demora para carregar)

library(clipr)

write_clip(TesteCensoEscolar)

####Note que por default, o separador decimal usado é o ponto. Para mudar isso, basta usar o argumento dec = ","

## write_clip(NOMETABELA, dec = ",")
