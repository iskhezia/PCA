# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(ggrepel)
library(readxl)
library(paletteer)

#Diretorio
setwd("/Users/kheziamoura/Documents/TCC")

# Ler o dataset
data <- read_excel("2024_indexofeconomicfreedom_data_v2.xlsx")

paises_pt <- c(
  "Afeganistão", "Albânia", "Argélia", "Angola", "Argentina", "Armênia",
  "Austrália", "Áustria", "Azerbaijão", "Bahamas", "Bahrein", "Bangladesh",
  "Barbados", "Belarus", "Bélgica", "Belize", "Benin", "Butão",
  "Bolívia", "Bósnia e Herzegovina", "Botsuana", "Brasil", "Brunei", "Bulgária",
  "Burkina Faso", "Mianmar", "Burundi", "Cabo Verde", "Camboja", "Camarões",
  "Canadá", "República Centro-Africana", "Chade", "Chile", "China", "Colômbia",
  "Comores", "República Democrática do Congo", "República do Congo", "Costa Rica",
  "Costa do Marfim", "Croácia", "Cuba", "Chipre", "República Tcheca",
  "Dinamarca", "Djibuti", "Dominica", "República Dominicana", "Equador",
  "Egito", "El Salvador", "Guiné Equatorial", "Eritreia", "Estônia",
  "Eswatini", "Etiópia", "Fiji", "Finlândia", "França",
  "Gabão", "Gâmbia", "Geórgia", "Alemanha", "Gana", "Grécia",
  "Guatemala", "Guiné", "Guiné-Bissau", "Guiana", "Haiti", "Honduras",
  "Hungria", "Islândia", "Índia", "Indonésia", "Irã", "Iraque",
  "Irlanda", "Israel", "Itália", "Jamaica", "Japão", "Jordânia",
  "Cazaquistão", "Quênia", "Kiribati", "Coreia do Norte", "Coreia do Sul", "Kosovo",
  "Kuwait", "Quirguistão", "Laos", "Letônia", "Líbano", "Lesoto",
  "Libéria", "Líbia", "Liechtenstein", "Lituânia", "Luxemburgo", "Madagascar",
  "Malaui", "Malásia", "Maldivas", "Mali", "Malta", "Mauritânia",
  "Maurício", "México", "Micronésia", "Moldávia", "Mongólia", "Montenegro",
  "Marrocos", "Moçambique", "Namíbia", "Nepal", "Países Baixos", "Nova Zelândia",
  "Nicarágua", "Níger", "Nigéria", "Macedônia do Norte", "Noruega", "Omã",
  "Paquistão", "Panamá", "Papua-Nova Guiné", "Paraguai", "Peru", "Filipinas",
  "Polônia", "Portugal", "Catar", "Romênia", "Rússia", "Ruanda",
  "Samoa", "São Tomé e Príncipe", "Arábia Saudita", "Senegal", "Sérvia", "Seicheles",
  "Serra Leoa", "Singapura", "Eslováquia", "Eslovênia", "Ilhas Salomão", "Somália",
  "África do Sul", "Espanha", "Sri Lanka", "Santa Lúcia", "São Vicente e Granadinas", "Sudão",
  "Suriname", "Suécia", "Suíça", "Síria", "Taiwan", "Tadjiquistão",
  "Tanzânia", "Tailândia", "Timor-Leste", "Togo", "Tonga", "Trinidad e Tobago",
  "Tunísia", "Turquia", "Turcomenistão", "Uganda", "Ucrânia", "Emirados Árabes Unidos",
  "Reino Unido", "Estados Unidos", "Uruguai", "Uzbequistão", "Vanuatu", "Venezuela",
  "Vietnã", "Iêmen", "Zâmbia", "Zimbábue")

#Colocando os países em português
data$Country <- paises_pt

#Criando a coluna de classificação
data <- data %>%
  mutate(Classificação = case_when(
    `Overall Score` >= 80.0 ~ "Muito alta",
    `Overall Score` >= 70.1 & `Overall Score` <= 79.5 ~ "Alta",
    `Overall Score` >= 60.1 & `Overall Score` <= 71.0 ~ "Moderada",
    `Overall Score` >= 50.2 & `Overall Score` <= 59.8 ~ "Baixa",
    `Overall Score` >= 2.9 & `Overall Score` <= 49.9 ~ "Muito baixa",
    TRUE ~ NA_character_  # Se nenhuma condição for atendida, retorna NA
  )) %>%
  mutate(Classificação = factor(
    Classificação,
    levels = c("Muito alta", "Alta", "Moderada", "Baixa", "Muito baixa"),
    ordered = TRUE
  ))

#Vendo as variaveis
names(data)

# Selecionar as variáveis relevantes e remover NAs
variables <- c("Property", "Government", "Judicial","Tax Burden",
               "Government Spending","Fiscal Health","Business F.","Labor F.", 
               "Monetary F.","Trade F.","Investment F.","Financial F.")


df_pca <- data %>%
  select(Country, Region, Classificação, all_of(variables)) %>%
  na.omit()

df_pca <- df_pca %>% rename("DP" = "Property", "IG" = "Government", "EJ" = "Judicial",
                            "CT" = "Tax Burden", "GG" = "Government Spending", "SF" = "Fiscal Health",
                            "LE" = "Business F.", "LT" = "Labor F.", "LM" = "Monetary F.",
                            "LC" = "Trade F.", "LI" = "Investment F.", "LF" = "Financial F.")

variables <-  c("DP", "IG","EJ", "CT", "GG", "SF", "LE", "LT","LM", "LC", "LI", "LF")

# Padronizar as variáveis - Não é necessário pois todas as variáveis estão na mesma escala

##################### Aplicando a PCA Tradicional #############################
pca <- prcomp(df_pca[variables], center = TRUE, scale. = TRUE)

pca_excel <- pca$rotation
#write.xlsx(pca_excel,file="pca_liberdadeEconomica.xlsx")

# Calcular a variância explicada
explained_variance <- summary(pca)$importance[2, ]
explained_variance_percentage <- explained_variance * 100
autovalores <- pca$sdev^2
sum(autovalores)

# Calcular a variância acumulada
explained_variance_cumulative <- cumsum(explained_variance_percentage)

#screeplot
library(factoextra)
fviz_eig(pca)

fviz_eig(pca, addlabels=TRUE, hjust = -0.3,
         barfill="white", barcolor ="darkblue",
         linecolor ="red") + ylim(0,60) + 
  theme_minimal()+
  labs(y = "Variância Explicada (%)")+
  labs(x = "Componentes")

#Vendo a correlação entre as componentes e as variaveis originais
# Extrair os scores das componentes principais
scores <- pca$x

# Calcular a correlação entre as variáveis originais e os componentes principais
(correlations <- cor(df_pca[variables], scores))
#write.xlsx(correlations,file="correlations_liberdadeEconomica.xlsx")

# Transformar os componentes principais em um dataframe
pca_df <- as.data.frame(pca$x)

# Adicionar nomes dos países e indicadores regionais ao dataframe de PCA
pca_df$Country  <- df_pca$Country 
pca_df$Region <- df_pca$Region
pca_df$Classificação <- df_pca$Classificação

# Lista de países influentes 
influential_countries<- c(
  "Estados Unidos", "China", "Rússia", "Índia", "França", "Alemanha",
  "Reino Unido", "Japão", "Arábia Saudita", "Itália", "Israel",
  "Canadá", "Brasil", "Austrália", "Espanha", "Coreia do Sul",
  "Países Baixos", "Turquia", "Suíça", "Taiwan", "México",
  "Polônia", "Argentina", "Bélgica", "Suécia", "Irlanda", "Tailândia",
  "Noruega", "Singapura", "Áustria", "Nigéria", "Emirados Árabes Unidos",
  "Vietnã", "Malásia", "Filipinas", "Bangladesh", "Dinamarca",
  "África do Sul", "Colômbia", "Egito", "Chile", "Haiti", "Finlândia", "Indonésia",
  "Coreia do Norte", "Cuba", "Venezuela")

# Filtrar o dataframe para os países influentes
influential_pca_df <- pca_df %>%
  filter(Country  %in% influential_countries)

# Refletir os valores de PC1 e PC2
influential_pca_df <- influential_pca_df %>%
  mutate(PC1 = -PC1, PC2 = -PC2)

# Obter loadings das variáveis
loadings <- as.data.frame(pca$rotation)
loadings$Variable <- rownames(loadings)

# Grafico baseado na classificação
ggplot() +
  geom_text(data = influential_pca_df, aes(x = PC1, y = PC2, color = factor(Classificação, levels = c("Muito alta", "Alta", "Moderada", "Baixa", "Muito baixa")), label = Country), size = 3, , key_glyph = "rect") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo X
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo Y
  labs(
    x = paste0("Componente Principal 1 (", round(explained_variance_percentage[1], 2), "%)"),
    y = paste0("Componente Principal 2 (", round(explained_variance_percentage[2], 2), "%)"),
    color = "Classificação") +
  scale_color_manual(values = c("darkgreen", "chartreuse4", "gold2", "orange2", "red3")) +
  theme_minimal() +
  theme(legend.position = "right", legend.key = )

ggplot() +
    geom_text(data = influential_pca_df, aes(x = PC1, y = PC2, color = factor(Classificação, levels = c("Muito alta", "Alta", "Moderada", "Baixa", "Muito baixa")), label = Country), size = 3, key_glyph = "rect") +
    #geom_point(data = influential_pca_df, aes(x = PC1, y = PC2, color = factor(Classificação, levels = c("Muito alta", "Alta", "Moderada", "Baixa", "Muito baixa"))), size = 3, shape = 16, alpha = 0) +  # Camada adicional para bolinhas na legenda
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo X
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo Y
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = -PC1 * 7, yend = -PC2 * 7), arrow = arrow(length = unit(0.3, "cm")), color = "pink", size = 1) +  # Vetores das variáveis# Destaque do eixo Y
  geom_text(data = loadings, aes(x = -PC1 * 7, y = -PC2 * 7, label = Variable), color = "black", size = 3, vjust = 1, key_glyph = "rect") +  # Rótulos dos vetores
  labs(
      x = paste0("Componente Principal 1 (", round(explained_variance_percentage[1], 2), "%)"),
      y = paste0("Componente Principal 2 (", round(explained_variance_percentage[2], 2), "%)"),
      color = "Classificação") +
    scale_color_manual(values = c("darkgreen", "chartreuse4", "gold2", "orange2", "red3"),
                       guide = guide_legend(override.aes = list(shape = 16, size = 4))) +  # Define bolinhas para a legenda
    theme_minimal() +
    theme(legend.position = "right")


############################ PCA LOGÍSTICA ####################################
############# TRANSFORMANDO E APLICANDO A PCA LOGÍSTICA #######################

library(logisticPCA)
library(mlbench)
library(rARPACK)

# Transformando o conjunto para binário. Nota de corte = 60
df_binario <- df_pca  %>%
  mutate(
    `DP` = ifelse(`DP` > 60, 1, 0),
    `IG` = ifelse(`IG` > 60, 1, 0),
    `EJ` = ifelse(`EJ` > 60, 1, 0),
    `CT` = ifelse(`CT` > 60, 1, 0),
    `GG` = ifelse(`GG` > 60, 1, 0),
    `SF` = ifelse(`SF` > 60, 1, 0),
    `LE` = ifelse(`LE` > 60, 1, 0),
    `LT` = ifelse(`LT` > 60, 1, 0),
    `LM` = ifelse(`LM` > 60, 1, 0),
    `LC` = ifelse(`LC` > 60, 1, 0),
    `LI` = ifelse(`LI` > 60, 1, 0),
    `LF` = ifelse(`LF` > 60, 1, 0)
  )


############# PCA LOGISTICA CONFORME A CLASSIFICAÇÃO ###########
#Pegando as variáveis binárias
liberdade_lpca <- (as.matrix(df_binario[, 4:15]))
rownames(liberdade_lpca) = df_binario$Classificação #Colocando o nome das linhas conforme classificação

#Aplicando a PCA
logpca_cv = cv.lpca(liberdade_lpca, ks = 2, ms = 1:15) #assume melhor quando m = 12
logpca_model = logisticPCA(liberdade_lpca, k = 2, m = which.min(logpca_cv))
plot(logpca_model, type = "trace")

party = rownames(liberdade_lpca)
party = factor(party, levels = c("Muito alta", "Alta", "Moderada", "Baixa", "Muito baixa"))
plot(logpca_model, type = "scores") +
  geom_point(aes(colour = party)) + 
  ggtitle("Logistic PCA")+
  scale_color_manual(values = c("darkgreen", "chartreuse4", "gold2", "orange2", "red3"))



############# PCA LOGISTICA CONFORME O NOME DOS PAÍSES ###########

Liberdade_lpca <- (as.matrix(df_binario[, 4:15]))
rownames(liberdade_lpca) = df_binario$Country

logpca_model = logisticPCA(liberdade_lpca, k = 2, m = which.min(logpca_cv))
plot(logpca_model, type = "trace")


party = rownames(liberdade_lpca)
plot(logpca_model, type = "scores") + 
  geom_point(aes(colour = party)) + 
  ggtitle("Logistic PCA") +
  labs(x = "Componente Principal 1", y = "Componente Principal 2") +
  theme(legend.position = "none")


# Obter os scores da PCA
scores <- as.data.frame(logpca_model$PCs)

# Adicionar o nome dos países
scores$Country <- rownames(liberdade_lpca)

# Filtrar apenas os países influentes
influential_scores <- scores %>%
  filter(Country %in% influential_countries)

influential_scores <- left_join(influential_scores, df_binario[,1:3], by = "Country")

# Plotar os resultados apenas para os países influentes
ggplot(influential_scores, aes(x = -V1, y = -V2, colour = Classificação)) +
  geom_text(aes(label = Country), vjust = -0.5, hjust = -0.5, size = 3, key_glyph = "rect") +  # Adiciona os nomes dos países
  labs(x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "chartreuse4", "gold2", "orange2", "red3"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo X
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

loadings <- as.data.frame(logpca_model$U)
loadings$Variable <- rownames(loadings)
loadings$Variable <- variables

#Com vetores
ggplot(influential_scores, aes(x = -V1, y = -V2, colour = Classificação)) +
  geom_text(aes(label = Country), vjust = -0.5, hjust = -0.5, size = 3, key_glyph = "rect") +  # Adiciona os nomes dos países
  labs(x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal() +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = -V1*15, yend = -V2*15), arrow = arrow(length = unit(0.3, "cm")), color = "pink", size = 1) + 
  geom_text(data = loadings, aes(x = -V1 * 15, y = -V2 * 15, label = Variable), color = "black", size = 3, vjust = 1, key_glyph = "rect") +  # Rótulos dos vetores
  scale_color_manual(values = c("darkgreen", "chartreuse4", "gold2", "orange2", "red3"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Destaque do eixo X
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")



