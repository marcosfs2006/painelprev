# Importação e tratamento dos dados do CRP

crp <- readRDS("dados/crp_DtRef_FEV2020.Rds") # tem cnpj
dt_extracao <- comment(crp)

# Cria uma nova variável contendo a quantidade de dias decorridos desde a data de validade.
crp <- crp %>%
  mutate(qtd_dias = as.numeric(difftime(ymd(dt_extracao), dt_validade, units = "days"))) # Na data de extração.

# Na base de dados, se a data de validade coincide com a data de extração, a situação do 
# do CRP é considerada VENCIDO. Acho que isso não tá correto e o código a seguir corrige isso.
crp <- crp %>% 
  mutate(situacao = ifelse(dt_validade == ymd(dt_extracao), "VÁLIDO", situacao))

# Faz o escalonamento de dias,
crp <- crp %>%
  mutate(categoria = cut(qtd_dias,
                         breaks =c(-Inf, 0, 30, 60, 90, 180, 365, 5*365, Inf) ,
                         labels = c("A VENCER",
                                    "VENCIDO: 1 a 30 DIAS",
                                    "VENCIDO: 31 a 60 DIAS",
                                    "VENCIDO: 61 a 90 DIAS",
                                    "VENCIDO: 91 a 180 DIAS",
                                    "VENCIDO: 181 a 365 DIAS",
                                    "VENCIDO: 1 A 5 ANOS",
                                    "VENCIDO: MAIS DE 5 ANOS"),
                         ordered_result = TRUE))

# Formatação de datas necessária para que as tabelas feitas com a função DT::datatable()
# não saiam com as indicações de hora, minuto e segundo. (corrigir isso... as datas devem ficar no foramato de data)
crp <- crp %>% 
  mutate( dt_emissao  = as.character(format(dt_emissao, "%d/%m/%Y")), 
          dt_validade = as.character(format(dt_validade, "%d/%m/%Y")))


