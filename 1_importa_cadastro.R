# Importação da listagem dos RPPS Brasileiros

cadastro <- readRDS("dados/cadastro_entes_DtRef_FEV2020.Rds")

# Conferir se os nomes estão batendo...
# 1. Cadastro com DAIR []
# 2. Cadastro com DIPR [ok]

# 3. Cadastro com CRP [ok]             Não é necessário coincidir porque nada depende dos nomes dos Entes. 
# all(is.element(crp$ente, cadastro$ente)) # TRUE
# all(is.element(cadastro$ente, crp$ente)) # FALSE

# 4. Cadastro com PARCELAMENTOS [ok]
# 5. Cadastro com XXX

