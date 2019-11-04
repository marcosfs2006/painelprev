#https://www.infofundos.com/rest-api
# www.infofundos.com/api
#https://maisretorno.com/melhores-fundos-investimentos
#https://www.investidor.gov.br/publicacao/ListaCadernos.html
#https://www.infomoney.com.br/colunistas/maxima-performance/nova-ferramenta-gratuita-de-fundos-de-investimentos/



#===============================================================================
#
# Este arquivo contém scripts para fazer o download dos dados da SPrev
# e realizar o pré-processento e deixar pronto para o consumo do app
# tudo aqui deve estar descrito no documento Análise de Dados Previdenciários - ADPrev
#
#===============================================================================
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(tabulizer)
#library(recipes)
#library(pins)
#library(data.cubes)

setwd("C:\\Users\\Marcos\\OneDrive\\TribunalRJ\\DadosSPrev\\Bases Dados SPrev\\27-08-2019")

#===============================================================================
#
# ISP
#===============================================================================
isp <- read_excel("RESULTADO-ISP-2018.01.xlsx")
isp <- isp[, -c(31, 32, 33, 34)]
names(isp) <- c("ENTE",    "UF", "REGIAO", "GRUPO", "ESCRITURA",	"APLICFIN",
                "CONTRIB", "COBSERVPUB", "CONCBENEF",	"EQUILATUAR",	"PARTSEG",	"PARCELTEMP",
                "REGRAS",	 "UTILRECPREV",	"CONF",	"CLASSIF_CONF",	"ENDIVID",	"SOLVFIN",
                "RAZAOAI", "COMPROMATUAR",	"SOLVATUAR",	"EQUIL",	"CLASSIF_EQUIL",
                "DRAA",    "DIPR",	"DPIN",	"DAIR",	"TRANSP",	"CLASSIF_TRANSP",	"ISP201801")

# Exportar os dados
saveRDS(isp, file="isp.Rds")


#===============================================================================
#
# CRP
#===============================================================================
crp <- read_csv2("CRP Emitido.csv")
crp <- crp %>% filter(REGIME == "RPPS")
crp <- crp %>%
  mutate(`DATA DE EMISSÃO` = dmy(`DATA DE EMISSÃO`),
         `DATA DE VALIDADE`= dmy(`DATA DE VALIDADE`)) %>%
  group_by(UF, ENTE) %>%
  filter(`DATA DE EMISSÃO` == max(`DATA DE EMISSÃO`))

# Exclusão de duplicidades. Verificar se ainda existem.
crp <- crp %>%
  filter(!duplicated(`NR CRP`))

# Cálculo da quantidade de dias (Não. Essa conta deve ocorrer nomento da consulta no painel)
#crp <- crp %>%
#  mutate(QtdDias = as.numeric(difftime(as.POSIXct(Sys.Date()), `DATA DE VALIDADE`, units = "days")))

# Exportar os dados
saveRDS(crp, file="crp_2019-09.Rds")


#===============================================================================
#
# DAIR
#===============================================================================

# IMPORTAR OS DADOS BRUTOS

arquivos <- c("DAIR-2018-Carteira.csv", "DAIR-2019-Carteira.csv")
dair <- lapply(arquivos, read.csv2, stringsAsFactors = FALSE, na.strings = c("NULL", "", " "))
dair <- do.call('rbind', dair)


#dair <- c("DAIR-2018-Carteira.csv", "DAIR-2019-Carteira.csv") %>%
#            walk(read_csv2, na=c("NULL", "", " ")) %>%
#            bind_rows()


# REALIZAR O PRÉ-PROCESSAMENTO
# Alterar os nomes das colunas
names(dair) <- c("cnpj", "uf", "ente", "competencia", "segmento",
                   "tipo_ativo", "limite_resol_cmn", "ident_ativo",
                   "nm_ativo", "qtd_quotas", "vlr_atual_ativo", "vlr_total_atual",
                   "perc_recursos_rpps", "pl_fundo", "perc_pl_fundo")

# Realizar a limpeza dos dados
dair <- dair %>%
  mutate(ente        = iconv(ente,       from = "utf-8", to="latin1"),
         segmento    = iconv(segmento,   from = "utf-8", to="latin1"),
         tipo_ativo  = iconv(tipo_ativo, from = "utf-8", to="latin1"),
         nm_ativo    = iconv(nm_ativo,   from = "utf-8", to="latin1"),
         ident_ativo = trimws(ident_ativo))

# Preencher valores do campo "tipo_ativo" para as disponibilidades. 
dair$tipo_ativo <- ifelse(dair$segmento == "Disponibilidades Financeiras", "Disp", dair$tipo_ativo)

# Converter a variável "competencia" para o formato de data
#dair$competencia <- as.Date(paste("01", dair$competencia, sep="/"), "%d/%m/%Y") # formato: 08/2019

dair <- dair %>%
          mutate(competencia = ifelse(is.na(competencia),
                                      competencia,
                                      dmy(paste("01", sprintf("%06.0f", competencia), sep=""))))

dair$competencia <- as.Date(dair$competencia, origin = "1970-01-01")


# Remover sinais de pontuação do CNPJ dos fundos de investimento 
dair$ident_ativo <- gsub("[[:punct:]]", "", dair$ident_ativo)

# Exportar a base de dados
saveRDS(dair, file="dair.Rds")

#===============================================================================
#
# FUNDOS VEDADOS
#===============================================================================
# Definição de uma função para limpeza dos dados (encoding e remoção de '\r')
convcol <- function(x){
  x <- iconv(as.character(x), from="utf-8", to="latin1")
  x <- gsub("\\r", " ", as.character(x))
  x
}

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



#===============================================================================
#
# ENQUADRAMENTO DOS FUNDOS DE INVESTIMENTOS
#===============================================================================
enquadramento_sprev <- read_excel("Planilha-de-Enquadramento-dos-Fundos-com-aplicacoes-dos-RPPS-27092019.xlsx", skip=1)
names(enquadramento_sprev) <- c("cnpj", "nm_fundo", "seguimento", "tipo_ativo", "cnpj_admin", "cnpj_gestor")
enquadramento_sprev <- enquadramento_sprev %>%
                            mutate(enquad_sprev = str_extract(tipo_ativo, "Art.*$"))

enquadramento_sprev$cnpj <- sprintf("%014.0f", enquadramento_sprev$cnpj)

# Exportar os dados
saveRDS(enquadramento_sprev, file = "enquadramento_sprev.Rds")

#===============================================================================
#
# LIMITES RESOLUÇÃO CMN 3922/10
#===============================================================================
limites_cmn <- read_excel("limites_cmn.xlsx",  sheet="limites_Res4604")
limites_cmn$classificacao <- toupper(gsub('[[:punct:]]| |o', '', limites_cmn$ativo))

# Exportar os dados
saveRDS(limites_cmn, file="limites_cmn.Rds")







#===============================================================================
#
# DIPR
#===============================================================================
# Nota: O arquivo DIPR-2017-20190827.xlsx contém os dados de 2018 apesar
#       do nome do arquivos referir-se a 2017
arquivos_dipr <- c("DIPR-2017-20190827.xlsx", "DIPR-2019-20190827.xlsx")
dipr <- lapply(arquivos_dipr,
               read_excel,
               sheet = "diprResultadoPrevidenciario",
               skip = 4, col_names = FALSE)
dipr <- do.call('rbind', dipr)

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

# Limpar alguns dados
dipr$ente <- gsub("^Governo d[oea] |Prefeitura Municipal d[eoa] ", "", dipr$ente)


# Função para converter para formato numerico
convnum <- function(x){
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  as.numeric(x)
}

# convertendo os dados...
dipr[,-c(1:5)] <- sapply(dipr[,-c(1:5)], convnum)

# Exportando os dados
saveRDS(dipr, file = "dipr.Rds")




#===============================================================================
#
# DRAA
#===============================================================================




#===============================================================================
#
# PARCELAMENTOS
#===============================================================================
parcel <- read_excel("PARCELAMENTOS_27082019.xlsx", skip = 3, col_names = FALSE)
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
parcel[, colunas_converter] <- sapply(parcel[, colunas_converter], convNum)

# Limpar os nomes dos entes:
parcel$ente <- gsub("^Governo d[oea] ", "", parcel$ente)

# Exportar os dados
saveRDS(parcel, file="parcelamentos.Rds")

