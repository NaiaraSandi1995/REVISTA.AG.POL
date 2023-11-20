# Obter os coeficientes do modelo
coeficients <- coef(Mod2023.1)

# Calcular os valores exponenciados dos coeficientes
exp_coefs <- exp(coeficients)

# Criar a tabela com os coeficientes exponenciados
model_table <- cbind(coeficients, exp_coefs)

# Renomear as colunas
colnames(model_table) <- c("Coeficientes", "Odds Ratios")

# Imprimir a tabela
model_table
tab_model(model_table)
