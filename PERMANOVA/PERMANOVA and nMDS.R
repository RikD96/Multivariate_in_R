##################################################################
#                                                                #
# Desenvolvedor: HENRIQUE PAULO SILVA DE MELO                    #
#                                                                #
# Contato:                                                       #
#                                                                #
##################################################################

#----------------------------------------------------------------#
# Reiniciando sessão R                                                                       
# ---------------------------------------------------------------#
rm(list=ls())
graphics.off()
options(warn=0)
.rs.restartR()

#----------------------------------------------------------------#

# Carregar bibliotecas necessárias
library(vegan)
library(ggplot2)

# Carregar dados
getwd() # mostrar diretorio
setwd("/media/rik_d/Dados/OneDrive/Documentos/Data Science/Multivariate in R/PERMANOVA") # define o diretorio
dir()

dados <- read.csv("Grupo_Sazonalidade.csv")


# Salvar a coluna da variável
variavel <- dados$Sazonalidade

# Remover a coluna da variável dos dados
dados$Sazonalidade <- NULL

# Padronizar a composição de espécies com o método de Hellinger
#- species.hel <- decostand(x = dados, method = "hellinger")

# Criar matriz de distância com o método Bray-Curtis
sps.dis <- vegdist(x = dados, method = "bray")

# Verificar a correlação entre as variáveis
correlacao <- cor(dados)

# Imprimir a matriz de correlação
print(correlacao)

# Criar design com a variável salva
design <- data.frame(grupo = variavel)

# Executar Permanova
resultado <- adonis2(sps.dis ~ design$grupo)

# Imprimir resultado da Permanova
print(resultado)

# Executar BETADISPER para verificar a homogeneidade das variâncias
betadisper_resultado <- betadisper(sps.dis, design$grupo)

# Imprimir resultado do BETADISPER
print(betadisper_resultado)

# Executar nMDS
nmds_resultado <- metaMDS(sps.dis)

# Imprimir resultado do nMDS
print(nmds_resultado)


nmds_resultado <- metaMDS(sps.dis)

# Converter os resultados do nMDS em um dataframe para uso com ggplot2
nmds_df <- data.frame(MDS1 = nmds_resultado$points[,1], MDS2 = nmds_resultado$points[,2], Grupo = variavel)

# Criar gráfico nMDS usando ggplot2 com polígonos para cada grupo
ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = Grupo)) +
  geom_polygon(aes(fill = Grupo), alpha = 0.3, data = nmds_df, stat='identity') +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Análise nMDS", x = "MDS1", y = "MDS2") +
  theme(legend.title = element_blank())
