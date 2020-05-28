# dicas para colocar o app no google cloud.
# https://www.simoncoulombe.com/2018/05/protected_free_shiny/
  

# 1. DOCUMENTAÇÃO DO SCRIPT----
 
# Este arquivo contém scripts para fazer o pré-processamento dos dados previdenciários
# disponibilizados pela SPREV no site
# http://www.previdencia.gov.br/dados-abertos/estatisticas-e-informacoes-dos-rpps/
# e deixá-los prontos para serem consumidos pelo app "Painel Previdenciario" no formato Rds.

# Tudo aqui deve estar descrito no documento Análise de Dados Previdenciários - ADPrev

# Dados disponibilizados pela SPREV na página de dados abertos:

# * Base de dados do CRP. Arquivo "VALIDADE-CRP_atualizacao_de_fev_2020_extracao_em_17.03.2020.xlsx"
# * Cadastro dos Entes. Arquivo ""


# Alem dos dados disponibilizados nos dados abertos, os seguintes arquivos são
# necessários:

# * Relação dos fundos vedados. Arquivo "xxx" 
# * Enquadramentodos fundos de investimentos. Arquivo "xxx"
# * Limites Resolução CMN 3922/10. Arquivo "xxx"
# * Art. 15 da Resolução CMN 3922
#


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 2. CARREGAR PACOTES----
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)


#library(tabulizer)
#library(recipes)
#library(pins)
#library(data.cubes)
#library(memoise)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 3. SETAR LOCALIZAÇÃO DAS BASES DE DADOS----
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# As bases de dados cruas ficarão armazenadas em um único local. Tando o painel
# previdenciário, quanto o documento ADPrev devem consumir os dados armazendados 
# neste local para evitar uso de versões distintas dos dados. 

options(scipen = 999)
setwd("C:\\Users\\Marcos\\OneDrive\\TribunalRJ\\DadosSPrev\\Bases Dados SPrev\\ABR2020")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 5. RELAÇÃO DOS ENTES COM CNPJ----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Este arquivo passará a ser utilizado como referência para os nomes dos entes e
# para colocar o CNPJ nas bases que não tem.

cadastro_entes <- read_csv("Regime-Previdenciario-Atual-dos-Entes-Federativose_abril_2020.csv",
                           locale = locale(encoding = "latin1")) %>%
                        rename(cnpj = CNPJ, 
                               uf   = UF,
                               ente = ENTE,
                               regime = REGIME) %>%
                        filter(regime == "RPPS")  

# Salvar os dados.
saveRDS(cadastro_entes, "cadastro_entes_DtRef_ABR2020.Rds")




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 5. CRP---- 
#
# Script atualizado em 22-05-2020 para trabalhar com os dados do CRP
# disponibilizados pela SPrev em ABR 2020.
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# NOTA: A base de dados oriunda da SPrev precisa ter os seguintes campos (com os nomes indicados):

# 'UF' - Sigla da unidade da federação
# 'ENTE' - Nome do ente da federação (municípios, estados e distrito federal)
# 'NR CRP' - Código numérico de identificação do Certificado de Regularidade Previdenciária
# 'DATA DE EMISSÃO' - Data de emissão do CRP
# 'DATA DE VALIDADE' - Data de validade do CRP. O CRP tem validade de 6 meses.
# 'CRP JUDICIAL' - Indica de o CRP do ente foi obtido pela via judicial
# 'SITUAÇÃO DO CRP' - Indica se o CRP está vigente (e portanto válido) ou está vencido.

# Importar a base de CRP
crp <- read_csv("VALIDADE-CRP_atualizacao_de_abril_2020_extracao_em_16.05.2020.csv",
                locale = locale(encoding = "latin1"))


# Teste 1. Verificar se a base possui os campos acima com os nomes indicados



# Alterar nomes das variáveis
crp <- crp %>%
        rename(uf = UF,
               ente = ENTE,
               num_crp = `NR CRP`,
               dt_emissao = `DATA DE EMISSÃO`,
               dt_validade = `DATA DE VALIDADE`,
               judicial = `CRP JUDICIAL`,
               tipo_regime = `TIPO DE REGIME`,
               situacao = `SITUAÇÃO DO CRP`)

# Converter datas para o formato de datas
crp <- crp %>% 
        mutate(dt_emissao = dmy(dt_emissao),
               dt_validade = dmy(dt_validade))

          
crp <- crp %>% 
          filter(tipo_regime == "RPPS")



# Teste 2: Existe duplicidade de CRP na base?
#any(duplicated(crp$num_crp)) 

# Tem dupliplicidade... identificar...
#crp_duplicado <- crp %>% 
#                    group_by(num_crp) %>% 
#                    filter(n()>1)

# Teste: Na data de extração o CRP é considerado Válido ou Vencido?
#crp %>% filter(dt_validade == ymd("2020-05-16"))



# Exclui duplicidades
crp <- crp %>% 
  distinct(num_crp, .keep_all = TRUE)



# Teste 3: Os nomes dos Entes na base do CRP estão iguais aos
#          nomes dos entes na base de Cadastro do RPPS?
cadastro_entes$ente[!is.element( cadastro_entes$ente, unique(crp$ente))]


# Insere na base a data de extração... IMPORTANTE!!!! Isso é usado no código...
comment(crp) <- "2020-05-16"


# Salva os dados
saveRDS(crp, file="crp_DtRef_ABR2020.Rds") 



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 6. DAIR----
# * Anos de 2018, 2019 e 2020 
#
# Script modificado em Maioo de 2020 para importar
# os dados disponibilizados pela SPREV em ABR 2020.
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dair <- read_delim("Carteira 2013_a_2020_Atualizacao_de_abr_2020_extração_em_18.05.20202020-05-18.csv",
                 delim = ",", locale = locale(encoding = "latin1",
                                              decimal_mark = ",",
                                              grouping_mark = "")) 

# +++++Realizar o pré processamento++++++ 

# Alterar os nomes das colunas
names(dair) <- c("cnpj", "uf", "ente", "competencia", "segmento",
                   "tipo_ativo", "limite_resol_cmn", "ident_ativo",
                   "nm_ativo", "qtd_quotas", "vlr_atual_ativo", "vlr_total_atual",
                   "perc_recursos_rpps", "pl_fundo", "perc_pl_fundo")

# Filtrar para pegar apenas os dados posteriores a 31/12/2017 (2018, 2019 e 2020)

# Trabalhar os valores do campo competêncai...

# Tirar da base os valores da forma "X/YYYY"
dair <- dair %>% 
          filter(!str_detect(competencia, "/"))

# Converter a variável "competencia" para o formato de data
dair <- dair %>%
  mutate(competencia = dmy(str_c("01", competencia)))

# Pegar apenas os registros de 2018, 2019 e 2020
dair <- dair %>% 
          filter(competencia >= dmy("01012018"))


# Preencher valores do campo "tipo_ativo" para as disponibilidades. 
dair$tipo_ativo <- ifelse(dair$segmento == "Disponibilidades Financeiras", "Disp", dair$tipo_ativo)


# Remover sinais de pontuação do CNPJ dos fundos de investimento 
dair$ident_ativo <- gsub("[[:punct:]]", "", dair$ident_ativo)

# Exportar a base de dados
saveRDS(dair, file="dair_DtRef_ABR2020.Rds")



## TESTES...
# Testar para ver se os nomes no dair conferem com os nomes no cadastro
unique(dair$ente)[!is.element(unique(dair$ente), cadastro$ente)] # 9 entes...
cadastro$ente[!is.element(cadastro$ente, unique(dair$ente))]




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 7. FUNDOS VEDADOS----
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Definição de uma função para limpeza dos dados (encoding e remoção de '\r')

convcol <- function(x){
  x <- iconv(as.character(x), from="utf-8", to="latin1")
  x <- gsub("\\r", " ", as.character(x))
  x
}

# Extração e tratamento dos dados

fundos_vedados <- extract_tables("FUNDOS-VEDADOS-CARTEIRA-DOS-FUNDOS-21122018.pdf")
nomes_colunas <- fundos_vedados[[1]][1,]
fundos_vedados <- lapply(fundos_vedados, function(x) x[-1,])
fundos_vedados <- do.call(rbind, fundos_vedados)
colnames(fundos_vedados) <- nomes_colunas

fundos_vedados <- apply(fundos_vedados, 2, convcol) # limpeza da base com a função "convcol()"
fundos_vedados <- as.data.frame(fundos_vedados)

names(fundos_vedados) <- toupper(iconv(names(fundos_vedados), from="utf-8", to="latin1"))
names(fundos_vedados) <- gsub("\\r", " ", names(fundos_vedados))

# Exportar os dados
saveRDS(fundos_vedados, file="fundos_vedados.Rds")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 8. ENQUADRAMENTO DOS FUNDOS DE INVESTIMENTOS----
#    Script alterado em 23/03/2020 para pegar o arquivo
#    mais recente de enquadramento dos fundos.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
enquadramento_sprev <- read_excel("./dair/Planilha-Consolidada031220.xlsx")

enquadramento_sprev <- enquadramento_sprev %>% 
                            select(nm_fundo    = FUNDO,
                                   cnpj        = CNPJ,
                                   cnpj_admin  = ADM,
                                   cnpj_gestor = GESTOR,
                                   enquad_sprev  = `CLASSIFICAÇÃO 3922`)

# Excluindo as duplicidades - Ver nota abaixo 
enquadramento_sprev <- enquadramento_sprev %>%
                            distinct(cnpj, .keep_all = TRUE)

# Exportar os dados
saveRDS(enquadramento_sprev, file = "enquadramento_sprev_12-03-2020.Rds")


# NOTA: Existe um fundo que está duplicado na base...
#
# Fundo duplicado: 10.922.432/0001-03 - ICATU VANGUARDA INFLAÇÃO CURTA FUNDO DE INVESTIMENTO RENDA FIXA LONGO PRAZO
# Artigo 7º, Inciso IV, 'a'
# Artigo 7º, Inciso I, 'b'


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 9. LIMITES RESOLUÇÃO CMN 3922/10----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
limites_cmn <- read_excel("limites_cmn.xlsx",  sheet="limites_Res4604")
limites_cmn$classificacao <- toupper(gsub('[[:punct:]]| |o', '', limites_cmn$ativo))

# Exportar os dados
saveRDS(limites_cmn, file="limites_cmn.Rds")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 10. DIPR----
# Script ajustado em 20.04.20 para importar as bases de dados de Fev2020

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Todo: incluir o CNPJ dos municípios na base de dados.
#       Verificar se os nomes dos entes batem com a base mestre
#       ("DADOS-GERAIS-Regime-Previdenciario-dos-Entes-Federativos.csv")

arquivos_dipr <- c("./dipr/Relatorio-de-Informacoes-Previdenciarias-2019_fev20.xlsx",
                   "./dipr/Relatorio-de-Informacoes-Previdenciarias-2018_fev20.xlsx")

dipr <- map_dfr(arquivos_dipr,
                read_excel,
                sheet = "diprResultadoPrevidenciario",
                skip = 4,
                col_names = FALSE)

names(dipr) <- c("ente", "uf", "competencia", "plano_segreg", "dt_info", "bc_pat_serv", 
                 "bc_pat_serv_ug", "bc_pat_apo", "bc_pat_pen", "bc_serv", "bc_apo",
                 "bc_pen", "ct_pat_serv", "ct_pat_serv_ug", "ct_pat_apo", "ct_pat_pen",
                 "ct_serv", "ct_apo", "ct_pen", "deduc_pat", "deduc_pat_outros", "deduc_seg",
                 "deduc_seg_outros", "aportes_amortiz", "aportes_insufin", "aportes_despadm",
                 "aportes_pagtesouro", "aportes_outros", "parcelamentos", "bcug_pat", "bcug_serv",
                 "bcug_serv_afast", "bcug_apos", "bcug_pen", "ctug_pat", "ctug_serv", "ctug_serv_afast",
                 "ctug_apos", "ctug_pen", "rb_serv", "rb_apos", "rb_pen", "nb_serv", "nb_apos", "nb_pen",
                 "nb_dep", "ing_contr", "ing_aportes", "ing_parc", "ing_acres_leg", "ing_contr_cedlic",
                 "ing_comprev", "ing_aplicfin", "ing_ativos", "ing_outros", "desp_apo", "desp_pen_morte",
                 "desp_auxdoenca", "desp_salmater", "desp_salfam", "desp_auxreclu", "desp_decjud",
                 "desp_beneftesouro", "desp_despadm", "desp_invest", "desp_rest", "desp_comprev", 
                 "desp_outras", "total_receita", "total_despesa", "resutado_final", "bcmil_pat_ativ",
                 "bcmil_ativos", "bcmil_reserva", "bcmil_pen", "ctmil_pat", "ctmil_ativos", "ctmil_reserva",
                 "ctmil_pen", "rbmil", "nbmil_ativos", "nbmil_reserva", "nbmil_pen", "nbmil_dep",
                 "ingmil_contr", "ingmil_aportes", "ingmil_outras", "despmil_reseva", "despmil_penmorte",
                 "despmil_outras", "resultado_final")



# Corrigir os nomes dos Entes no na base de DIPR para coincidir com os nomes do cadastro
# arquivo "DADOS-GERAIS-Regime-Previdenciario-dos-Entes-Federativos.csv"
dipr$ente <- gsub("^Prefeitura Municipal d[eoa] ", "", dipr$ente)


# Teste 1. Todos os nomes estão coincidindo?
cadastro_entes <- read_csv2("./crp/DADOS-GERAIS-Regime-Previdenciario-dos-Entes-Federativos.csv")
nomes_dipr <- dipr %>% 
                select(ente) %>%
                distinct() %>%
                pull()

nomes_cadastro <- cadastro_entes %>%
                    select(`Ente Federativo`) %>%
                    pull()

# Resultado: character(0) - Todos estão batendo... Mostrar quem não coincidir...
nomes_dipr[!is.element(nomes_dipr, nomes_cadastro)]


# Função para converter para formato numerico
convnum <- function(x){
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  as.numeric(x)
}

# convertendo os dados...(usar map_dbl() do pacotte purrr)
dipr <- as_tibble(map_at(.x = dipr,
                         .at = names(dipr)[-(1:5)],
                         .f = convnum))

# Converter a variável competencia???


# Exportando os dados
saveRDS(dipr, file = "dipr_DtRef_FEV2020.Rds")



# Exportar dipr apenas do RJ. Não consegui subir a base completa para RStudioCloud
# Obs. Base de dados utilizada para elaborar o relatório de acompanhamento
# de 2019.
dipr_rj <- filter(dipr, uf == "RJ")
saveRDS(dipr_rj, file = "dipr_rj_DtRef_FEV2020.Rds")





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 10.1 ALIQUOTAS DE CONTRIBUIÇÃO----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

aliquotas <- read_csv("Aliquotas-de-contribuicao-praticadas_abril_2020.csv",
                      locale = locale(encoding = "latin1"))

aliquotas <- aliquotas %>% 
                rename(cnpj_ente = CNPJ,
                       ente = Ente,
                       uf = UF,
                       plano_segregacao = `Plano de Segregação`,
                       sujeito_passivo = `Sujeito Passivo`,
                       aliquota = Aliquota,
                       inic_vigencia = `Início de Vigência`,
                       fim_vigencia = `Fim de Vigência`)

saveRDS(aliquotas, file = "aliquotas_contribuicao_DtRef_ABR2020.Rds")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 11. DRAA----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# a. Valores dos compromissos----

draa_compromissos <- read_tsv("./draa/Valores_Compromissos_Atualizacao_de_fev_2020_extração_em_17.03.2020.csv",
                              locale=locale(encoding = "latin1"))


draa_compromissos <- draa_compromissos %>% 
  rename(cnpj               = CNPJ,
         uf                 = UF,
         ente               = Ente,
         exercicio          = Exercicio,
         dt_envio           = `Data de Envio`,
         sit_draa           = `Situacao do DRAA`,
         tipo_plano         = `Tipo de Plano`,
         tipo_massa         = `Tipo de Massa`,
         codigo             = `Codigo Dem.  Result. Atuarial`,
         descr              = Descricao,
         categoria          = Categoria,
         vlr_geracao_atual  = `Valor da Geracao Atual`,
         vlr_geracao_futura = `Valor da Geracao Futura`)

draa_compromissos <- draa_compromissos %>% 
  mutate(vlr_geracao_atual  = as.numeric(vlr_geracao_atual),
         vlr_geracao_futura = as.numeric(vlr_geracao_futura))


draa_compromissos_2019 <- draa_compromissos %>% 
  filter(exercicio == 2019)

# Salvar os dados
saveRDS(draa_compromissos_2019, file="draa_compromissos_2019.Rds")


# b.----

# c.----




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 12. PARCELAMENTOS----
#     Script alterado em 24/03/2020 para importar dados
#     disponibilizado em Mar/2020

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
parcel <- read_excel("./parcelamentos/Acordos-de-parcelamentos-01-01-1996-a-04-02-2020.xlsx", skip = 3, col_names = FALSE)

names(parcel) <- c("ente", "uf", "tipo", "situacao", "num_acordo", "rubrica",
                   "lei", "dt_consol", "dt_assin", "dt_venc_1a", "compet_ini",
                   "compet_fin", "vlr_consolid", "qtd_parc", "vlr_parc_ini",
                   "sd_estim_satualiz", "sd_estim_catualiz", "comp_indice",
                   "comp_tipo_juros", "comp_txjuros", "comp_multa", "parcvinc_indice",
                   "parcvinc_tipojuros", "parcvinc_txjuros", "parcvinc_multa",
                   "parcatraso_indice", "parcatraso_tipojuros",
                   "parcatraso_txjuros", "parcatraso_multa")

convNum <- function(x){
  as.numeric(gsub(",", ".", gsub("\\.", "", x)))
}

colunas_converter <- c("vlr_consolid", "sd_estim_satualiz", "sd_estim_catualiz", "vlr_parc_ini")

parcel <- parcel %>% 
            mutate_at(.vars = colunas_converter, .funs = convNum)

# Exportar os dados
saveRDS(parcel, file="parcelamentos_DtRef_FEV2020.Rds")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 13. FLUXOS ATUARIAIS----
#     Gera uma lista de dataframes contendo os dados do fluxo atuarial

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 14. MSC----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 15. SEGREGAÇÃO DE MASSAS----

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
segregacao <- read_excel("./draa/Segregação de Massa_Atualizacao_de_fev_2020_extração_em_17.03.2020.xlsx")
saveRDS(segregacao, file="segregacao_DtGer_17-03-2020.Rds")




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# 16. ISP----  
#    Código antigo...
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
isp <- read_excel("RESULTADO-ISP-2018.01.xlsx")
isp <- isp[, -c(31, 32, 33, 34)]
names(isp) <- c("ENTE",    "UF", "REGIAO", "GRUPO", "ESCRITURA",	"APLICFIN",
                "CONTRIB", "COBSERVPUB", "CONCBENEF",	"EQUILATUAR",	"PARTSEG",	"PARCELTEMP",
                "REGRAS",	 "UTILRECPREV",	"CONF",	"CLASSIF_CONF",	"ENDIVID",	"SOLVFIN",
                "RAZAOAI", "COMPROMATUAR",	"SOLVATUAR",	"EQUIL",	"CLASSIF_EQUIL",
                "DRAA",    "DIPR",	"DPIN",	"DAIR",	"TRANSP",	"CLASSIF_TRANSP",	"ISP201801")

# Exportar os dados
saveRDS(isp, file="isp.Rds")







# 17. EQUALIZAÇÃO DE NOMES NAS BASES DE DADOS----
#     Considerando que nem todas as bases de dados disponibilizadas pela SPrev 
#     possuem o CNPJ dos Entes para que seja possível realizar o merge das bases
#     isso deverá ser feito, em alguns casos, usando os nomes dos Entes.






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 18. FALHAS IDENTIFICADAS NOS DADOS----
# Pontos verificados na análise de dados - informar à SPREV


# 1. A base de CRP tem duplicações e para encontrá-las podemos usar o seguinte código:[Ok. Já comunicado.]

# Encontra duplicidades na base de dados... 
dups_crp <-crp %>% 
    group_by(`NR CRP`) %>% 
    filter(n()>1)

# Adiciona uma coluna nova com a contagem de casos.
crp %>% 
  add_count(`NR CRP`) %>%
  filter( n > 1) %>%
  distinct(`NR CRP`, .keep_all = T) %>% 
  arrange(desc(n))

# Exportar os dados.
write_excel_csv2(dups_crp, path = "CPR_Duplicados.csv")


# 2. A base de CRP tem Entes com RPPS faltando...[ok. já comunicado à sprev]
inf_rpps$ta_na_base_crp <- ifelse(inf_rpps$`Ente Federativo` %in% crp$ENTE, 1, 0)

inf_rpps %>% 
  filter(ta_na_base_crp == 0)





## Testes - FUNDOS VEDADOS DAIR ABR 2020 

fundos_vedados <- readRDS("fundos_vedados.Rds")
fundos_vedados$`CNPJ DO FUNDO` <- gsub("[[:punct:]]", "", fundos_vedados$`CNPJ DO FUNDO`)
funved <- unique(fundos_vedados$`CNPJ DO FUNDO`)

lumax <- dair %>%
  mutate(flag = ifelse(ident_ativo %in% funved, 1, 0)) %>% 
  filter(flag == 1)




