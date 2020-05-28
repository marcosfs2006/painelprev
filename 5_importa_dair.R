# Importa a base do DAIR e bases de dados relacionadas----

dair  <- readRDS("dados/dair_DtRef_FEV2020.Rds") %>% 
              mutate(enquad_rpps_pad = str_extract(tipo_ativo, "Art.*$"),
                     enquad_rpps_pad = toupper(gsub("[[:punct:]]| |º", "", enquad_rpps_pad))) 

dair_fundos <- dair %>% 
              filter(competencia >= as.Date("2018-04-01"),  # Pega dados posteriores a Abr/2018
                     grepl("\\d{14}", ident_ativo))        # Pega só fundos


fundos_vedados      <- readRDS("dados/fundos_vedados.Rds") 

names(fundos_vedados) <- c("cnpj_fundo", "nm_fundo", "classe1", "classe2", "tx_adm",
                           "administrador", "gestor", "carencia", "conversao_cotas",
                           "disp_rec_resgatado", "tx_saida", "motivo")

fundos_vedados <- fundos_vedados %>% 
                          mutate(cnpj_fundo = gsub("[[:punct:]]", "", cnpj_fundo)) %>% 
                          map_df(as.character)

enquadramento_sprev <- readRDS("dados/enquadramento_sprev_12-03-2020.Rds") %>% 
                          mutate(cnpj = str_remove_all(cnpj, "[[:punct:]]"),
                                 enquad_sprev_pad = str_replace_all(enquad_sprev, c("Artigo" = "Art", "Inciso" = "")),
                                 enquad_sprev_pad = toupper(gsub("[[:punct:]]| |º", "", enquad_sprev_pad)),
                                 cnpj_admin = str_extract(gsub("[[:punct:]]", "", cnpj_admin),  "^\\d{8}"),
                                 cnpj_gestor= str_extract(gsub("[[:punct:]]", "", cnpj_gestor), "^\\d{8}"))



limites_cmn  <- readRDS("dados/limites_cmn.Rds") %>% 
                   mutate(classificacao = toupper(gsub('[[:punct:]]| |o', '', ativo)))


art15 <- read_delim("dados/Art15_ResolucaoCMN.txt", delim="|", trim_ws = TRUE) %>%
              mutate(cnpj = gsub("[[:punct:]]", "", cnpj))







#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# PREPARA BASES AUXILIARES PARA USO NOS SUBMÓDULOS DO MÓDULO DAIR
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## [ATIVOS GARANTIDORES]----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Total dos Ativos Garantidores por mês - Para cada Ente

ativoGarantBruto <- dair %>%
  group_by(uf, ente, competencia) %>%   # Mudar para cnpj... mas antes converter para character...
  summarize(vlrAtivoTotal = sum(vlr_total_atual, na.rm=TRUE)) 

# Valor dos Ativos Garantidores Líquidos por mês - Para cada Ente

ativoGarantLiquido <- dair %>%
  filter(competencia >= ymd("2018-04-01"), grepl("Art.*$", tipo_ativo)) %>% # colocar uma coluna com flag...
  group_by(uf, ente, competencia) %>%
  summarize(vlrAtivoTotal = sum(vlr_total_atual, na.rm=TRUE)) 

# Junta as duas bases em uma só...

ativoGarantBruto$tipo   <- "Ativo Bruto"
ativoGarantLiquido$tipo <- "Ativo Liquido"

ativos_garantidores <- rbind(ativoGarantBruto,
                             ativoGarantLiquido)

rm(ativoGarantBruto, ativoGarantLiquido)



## [FUNDOS VEDADOS]----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Podemos retornar só as colunas necessárias??? ('ente', 'ident_ativo', 'nm_ativo')

invest_fv <- dair_fundos %>% 
  mutate(aplica_fundo_vedado = ifelse(ident_ativo %in% fundos_vedados$cnpj_fundo, 1, 0)) %>%
  filter(aplica_fundo_vedado == 1) 



## [ENQUADRAMENTO]----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Aumentou a quantidade de registros quando fez o join... Investigar...

divergencias_enquadramento <- dair_fundos %>%
                  #filter(competencia >= as.Date("2018-04-01"),  # Pega dados posteriores a Abr/2018
                  #       grepl("\\d{14}", ident_ativo)) %>%     # Pega só fundos
                  #mutate(enquad_rpps_pad = str_extract(tipo_ativo, "Art.*$"),
                  #       enquad_rpps_pad = toupper(gsub("[[:punct:]]| |º", "", enquad_rpps_pad))) %>%
                  left_join(select(enquadramento_sprev,
                                    cnpj,
                                    nm_fundo,
                                    enquad_sprev,
                                    enquad_sprev_pad),
                            by=c("ident_ativo" = "cnpj")) %>% 
                  mutate(divergencia = ifelse(enquad_rpps_pad == enquad_sprev_pad, 0, 1)) %>% 
                  filter(divergencia == 1)



## [COMPOSIÇÃO DA CARTEIRA]---- 
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dair_limites <- dair %>% 
  filter(competencia >= ymd("2018-04-01")) %>%
  mutate(classificacao = str_extract(tipo_ativo, "Art.*$"))

dair_limites <- dair_limites %>%
  mutate(classificacao =  ifelse(tipo_ativo == "Disp", "Disp",
                          ifelse(tipo_ativo %in% c("Prédio Comercial", "Outros - Imóveis",
                                                   "Terreno", "Casa", "Loja", "Prédio Residencial",
                                                   "Apartamento"), "Imoveis",
                          ifelse(tipo_ativo %in% c("Outros Bens, Direitos e Ativos",
                          "Títulos de Renda Fixa",
                          "Valores Mobiliários",
                          "Fundos de Investimento não previstos em Resolução CMN"), "Outros",
                          classificacao))))


dair_limites <- dair_limites %>%
  mutate(classificacao = ifelse(tipo_ativo == "FI 100% títulos TN", "Art. 7º, I, b",
                         ifelse(tipo_ativo == "FI de Renda Fixa", "Art. 7º, IV, b",
                         ifelse(tipo_ativo == "FI em Ações", "Art. 8º, II, a",
                         ifelse(tipo_ativo == "FI Multimercado - Aberto", "Art. 8º, III",
                         ifelse(tipo_ativo == "Fundo Investimento - Sufixo Investimento no Exterior", "Art. 9ºA, II", classificacao)))))
  )

dair_limites <- dair_limites %>%
  mutate(classificacao = toupper(gsub('[[:punct:]]| |º', '', classificacao)))

# Agrega os ativos
dair_limites_agreg <- dair_limites %>%
  group_by(cnpj, ente, uf, competencia, classificacao) %>%
  summarise(VlrTotalAtivo = sum(vlr_total_atual, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(cnpj, ente, uf, competencia) %>%
  mutate(pctVlrTotalAtivo = round(VlrTotalAtivo / sum(VlrTotalAtivo, na.rm=TRUE), 2))




## [LIMITES RES. 3922/10]----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dair_limites_agreg_liq <- dair_limites %>%
  mutate(classificacao = ifelse(classificacao %in% c("ART7IIIA", "ART7IIIB"), "ART7III#",
                                ifelse(classificacao %in% c("ART7IVA", "ART7IVB"),   "ART7IV#", classificacao))) %>%
  group_by(cnpj, ente, uf, competencia, classificacao) %>%
  mutate(VlrTotalAtivoLiq = ifelse(classificacao %in% c("DISP", "IMOVEIS", "OUTROS"), 0, vlr_total_atual)) %>%
  summarise(VlrTotalAtivoLiq = sum(VlrTotalAtivoLiq, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(cnpj, ente, uf, competencia) %>%
  mutate(pctVlrTotalAtivoLiq = round(VlrTotalAtivoLiq / sum(VlrTotalAtivoLiq, na.rm=TRUE) * 100, 2)) %>%
  filter(!classificacao %in% c("DISP", "IMOVEIS", "OUTROS"))

dair_limites_agreg_liq <- dair_limites_agreg_liq %>%
  left_join(limites_cmn[, c("classificacao", "limite", "limite_pl")],
            by="classificacao")

dair_limites_agreg_liq <- dair_limites_agreg_liq %>%
  mutate(limite  = ifelse(classificacao == "ART7III#", 60,
                          ifelse(classificacao == "ART7IV#",  40, limite)))



## [VEDAÇÃO ART. 15 RES. 3922/10]----
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Elaborar uma tabela dos fundos que não atendem ao art. 15

enquadramento_sprev <- enquadramento_sprev %>% 
                          mutate(atende_art15_adm = ifelse(cnpj_admin %in% art15$cnpj,  1, 0),
                                 atende_art15_ges = ifelse(cnpj_gestor %in% art15$cnpj, 1, 0))


nao_atende_art15 <- dair_fundos %>%
                      filter(competencia >= as.Date("2018-12-01")) %>%
                      left_join(enquadramento_sprev, by = c("ident_ativo" = "cnpj")) %>% 
                      filter( !(atende_art15_adm == 1 | atende_art15_ges == 1))




