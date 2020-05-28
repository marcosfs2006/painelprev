
# Definição das contas que compõem o Balanço Atuarial do plano preividenciário
contas_interesse <- c(500101, 500201, 500301, 500401, 500501, 509001,
                      130201, 210000, 110000, 220000, 120000, 130101)

# Importa os dados de interesse e aplica alguns filtros
draa_compromissos_2019 <- readRDS("dados/draa_compromissos_2019.Rds") %>% 
                              filter(codigo %in% contas_interesse,
                                     tipo_massa == "Civil")  


draa_entes <- draa_compromissos_2019 %>%
  filter(!tipo_plano %in% c("Financeiro", "Mantidos pelo Tesouro"),
         tipo_massa == "Civil") %>% 
  select(uf, ente, exercicio, dt_envio) %>% 
  distinct()







