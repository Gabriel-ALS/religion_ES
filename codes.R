library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(viridis)

###Organizando os dados------------

#importando
dados <- read_excel("Tabela1_Religiao2.xlsx")


dados_long <- dados %>%
  pivot_longer(
    cols = -c(Município, Religião),  # Manter estas colunas como identificadores
    names_to = "Grupo_Demografico",   # Nome da nova coluna que conterá os nomes das colunas atuais
    values_to = "Quantidade"          # Nome da nova coluna que conterá os valores
  )

# Visualizar o resultado
head(dados_long)

#Separando em categorias
dados_long_separado <- dados_long %>%
  separate(
    Grupo_Demografico, 
    into = c("Genero", "Raca_Cor"), 
    sep = "_"
  )

dados_long_separado <- clean_names(dados_long_separado)

# Visualizar o resultado
head(dados_long_separado)

#Zerando os dados em que não tinham informação
dados_long_separado <- dados_long_separado %>%
  mutate(quantidade = ifelse(quantidade == "-", 0, quantidade),
         quantidade = as.numeric(quantidade))

# Agora calculamos as porcentagens
total_geral <- sum(dados_long_separado$quantidade, na.rm = TRUE)

dados_porcentagem <- dados_long_separado %>%
  group_by(religiao, genero) %>%
  summarise(total = sum(quantidade, na.rm = TRUE), .groups = 'drop') %>%
  mutate(porcentagem = (total / total_geral) * 100)


#agrupando por diocese
dados_com_grupo <- dados_todos_juntos %>%
  mutate(
    grupo_eclesiastico = case_when(
      municipio %in% c("Vitória", "Vila Velha", "Serra", "Fundão", "Cariacica", 
                       "Viana", "Guarapari", "Anchieta", "Alfredo Chaves", 
                       "Afonso Cláudio", "Marechal Floriano", "Domingos Martins", 
                       "Santa Leopoldina", "Santa Maria de Jetibá", "Brejetuba") ~ "Arquidiocese de Vitória",
      
      municipio %in% c("Cachoeiro de Itapemirim", "Itapemirim", "Marataízes", 
                       "Presidente Kennedy", "Muqui", "Iúna", "Ibatiba", 
                       "Muniz Freire", "Alegre", "Apiacá", "Atílio Vivácqua", 
                       "Bom Jesus do Norte", "Castelo", "Divino de São Lourenço", 
                       "Dores do Rio Preto", "Guaçuí", "Iconha", "Jerônimo Monteiro", 
                       "Mimoso do Sul", "Piúma", "Rio Novo do Sul", 
                       "São José do Calçado", "Vargem Alta", "Ibitirama", 
                       "Irupi", "Conceição do Castelo", "V.N. Imigrante") ~ "Diocese de Cachoeiro de Itapemirim",
      
      municipio %in% c("Aracruz", "Baixo Guandu", "Colatina", 
                       "Governador Lindenberg", "Ibiraçu", "Itaguaçu", 
                       "Itarana", "João Neiva", "Laranja da Terra", "Linhares", 
                       "Marilândia", "Pancas", "Rio Bananal", "Santa Teresa", 
                       "São Domingos do Norte", "São Roque do Canaã", "Sooretama") ~ "Diocese de Colatina",
      
      municipio %in% c("Água Doce do Norte", "Águia Branca", "Alto Rio Novo", 
                       "Barra de São Francisco", "Boa Esperança", "Conceição da Barra", 
                       "Ecoporanga", "Jaguaré", "Mantenópolis", "Montanha", 
                       "Mucurici", "Ponto Belo", "Nova Venécia", "Vila Pavão", 
                       "Pedro Canário", "Pinheiros", "S.G. Palha", 
                       "Vila Valério", "São Mateus") ~ "Diocese de São Mateus",
      
      TRUE ~ "Não Classificado"
    )
  )

# Verificando o resultado
dados_com_grupo %>%
  select(municipio, grupo_eclesiastico) %>%
  distinct() %>%
  arrange(grupo_eclesiastico, municipio) %>%
  print(n = 78)  # Mostra todas as cidades e seus grupos



###Gráficos cidades----------

#Para encontrar os valores totais
dados_agregados <- dados_long_separado %>%
  group_by(religiao, genero) %>%
  summarise(quantidade_total = sum(quantidade, na.rm = TRUE), .groups = 'drop')

ggplot(dados_agregados, 
       aes(x = religiao, y = quantidade_total, fill = genero)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Distribuição por Religião e Gênero",
       x = "Religião", y = "Quantidade Total", fill = "Gênero") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Tabela do total de cada religião
dados_long_separado %>%
  group_by(religiao, genero) %>%
  summarise(total = sum(quantidade), .groups = 'drop') %>%
  arrange(desc(total))

#Porcentagens de cada Religião
barplot_porcentagem <- ggplot(dados_porcentagem, aes(x = religiao, y = porcentagem, fill = genero)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", porcentagem)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = "Distribuição por Religião e Gênero (em % do total)",
       x = "Religião", y = "Porcentagem (%)", fill = "Gênero") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, max(dados_porcentagem$porcentagem) * 1.1))

barplot_porcentagem
ggsave(
  filename = "barplot_porcentagem.png",  # Nome do arquivo (pode ser .png, .jpg, .pdf, etc.)
  plot = barplot_porcentagem,            # Objeto do gráfico
  width = 8,                     # Largura (polegadas)
  height = 6,                    # Altura (polegadas)
  dpi = 300                      # Resolução (DPI)
)

barplot_porcentagem


#Grafico de porcentagens empilhado

dados_porcentagem <- dados_porcentagem %>%
  mutate(religiao = stringr::str_wrap(religiao, width = 10))

ggplot(dados_porcentagem, aes(x = religiao, y = porcentagem, fill = genero)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", porcentagem)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  labs(title = "Distribuição por Religião e Gênero",
       subtitle = "Em porcentagem do total geral",
       x = NULL, y = "Porcentagem (%)") +
  scale_fill_viridis_d(option = "E")  # Inverte a ordem na legenda
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  )



# Primeiro agregue os dados por cidade e religião
dados_todos_juntos <- dados_long_separado %>%
  group_by(municipio, religiao) %>%
  summarise(total = sum(quantidade, na.rm = TRUE), .groups = 'drop')

dados_todos_juntos <- dados_todos_juntos %>%
  mutate(municipio = str_remove(municipio, " \\(ES\\)"))

dados_todos_juntos <- dados_todos_juntos %>%
  mutate(municipio = case_when(
    municipio == "São Gabriel da Palha" ~ "S.G. Palha",
    municipio == "Venda Nova do Imigrante" ~ "V.N. Imigrante",
    TRUE ~ municipio
  ))

# Gráfico com todas as variaveis 
barplot_todos <- ggplot(dados_todos_juntos, aes(x = religiao, y = total, fill = religiao)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ municipio, scales = "free_y") +  # Escala livre no eixo Y
  labs(title = "Distribuição Religiosa por Município",
       x = "Religião", y = "Quantidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove a legenda para economizar espaço

barplot_todos

ggsave(
  filename = "barplot_todos.png",  # Nome do arquivo (pode ser .png, .jpg, .pdf, etc.)
  plot = barplot_todos,            # Objeto do gráfico
  width = 8,                     # Largura (polegadas)
  height = 6,                    # Altura (polegadas)
  dpi = 300                      # Resolução (DPI)
)



ggplot(dados_todos_juntos, aes(x = religiao, y = total, fill = religiao)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ municipio, scales = "free_y") +
  labs(title = "Distribuição Religiosa por Município") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),        # Remove títulos dos eixos
    axis.text = element_blank(),         # Remove textos dos eixos
    axis.ticks = element_blank(),       # Remove marcadores dos eixos
    panel.grid = element_blank(),       # Remove linhas de grade
    legend.position = "none",           # Remove legenda
    strip.background = element_blank(), # Remove fundo dos títulos de faceta
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12), # Centraliza título
    panel.spacing = unit(0.5, "lines")  # Reduz espaço entre facets
  ) +
  scale_y_continuous(expand = c(0, 0))  # Remove margem extra no eixo Y

ggplot(dados_todos_juntos, aes(x = religiao, y = total, fill = religiao)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ municipio, scales = "free_y") +
  labs(title = "Distribuição Religiosa por Município") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",  # Move legenda para baixo
    legend.key.size = unit(0.3, "cm"),  # Reduz tamanho dos ícones
    legend.text = element_text(size = 7),  # Reduz tamanho do texto
    legend.title = element_blank(),  # Remove título da legenda
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.spacing = unit(0.5, "lines")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2))  # Legenda em 2 linhas


#grafico 2 com todas as cidades
ggplot(dados_todos_juntos, aes(x = religiao, y = total, fill = religiao)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ municipio, scales = "free_y") +
  labs(title = "Distribuição Religiosa por Município") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 7),
    legend.title = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.spacing = unit(0.5, "lines")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_viridis_d(option = "C")  # "C" = viridis escuro (azul/verde)

# Calcular proporções por município
dados_densidade <- dados_todos_juntos %>%
  group_by(municipio) %>%
  mutate(proporcao = total / sum(total)) %>%
  ungroup()


#heatmap municipios porcentagem
write.csv(dados_densidade, "dados_densidade.csv", row.names = FALSE)

dados_densidade <- dados_densidade %>%
  group_by(municipio) %>%
  mutate(total_cidade = sum(total),
         porcentagem_correta = total / total_cidade * 100) %>%
  ungroup()

head(dados_densidade %>% select(municipio, religiao, total, proporcao, porcentagem_correta))

# Filtrar apenas os dados da religião católica
dados_catolicos <- dados_densidade %>% 
  filter(religiao == "Católica Apostólica Romana")

# Criar o heatmap
heatmap <- ggplot(dados_catolicos, aes(x = 1, y = reorder(municipio, porcentagem_correta), 
                            fill = porcentagem_correta)) +
  geom_tile(color = "white", height = 1, width = 1) +  # altura menor = mais espaço
  geom_text(aes(label = paste0(round(porcentagem_correta, 1), "%")), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_viridis(option = "C", direction = -1,
                     name = "Porcentagem de Católicos") +
  labs(
    title = "Distribuição de Católicos por Município",
    subtitle = "Porcentagem da população que declara ser Católica Apostólica Romana",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 7, margin = margin(r = 6)),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 0.02)  # valor um pouco maior afasta mais ainda

ggsave("heatmap_catolicos.png", plot = heatmap, width = 18, height = 10, dpi = 300)

###Gráficos Dioceses--------------

dados_com_grupo <- dados_com_grupo %>%
  group_by(grupo_eclesiastico) %>%
  mutate(total_grupo = sum(total)) %>%
  ungroup() %>%
  mutate(porcentagem = (total / total_grupo) * 100)

# 2. Gráfico com facetas (um painel por grupo)
ggplot(dados_com_grupo, aes(x = religiao, y = porcentagem, fill = religiao)) +
  geom_bar(stat = "identity") +
  facet_wrap(~grupo_eclesiastico, scales = "free_x") +  # Painéis independentes
  labs(
    title = "Distribuição Religiosa por Grupo Eclesiástico",
    x = "Religião",
    y = "Porcentagem (%)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rótulos verticais
    legend.position = "none",  # Remove a legenda para evitar repetição
    strip.text = element_text(size = 10, face = "bold")  # Estilo dos títulos dos painéis
  ) + scale_fill_viridis_d(option = "E")


#boxplot
dados_vitoria <- dados_com_grupo %>% 
  filter(grupo_eclesiastico == "Arquidiocese de Vitória")

# Criar o boxplot
ggplot(dados_vitoria, aes(x = religiao, y = total, fill = religiao)) +
  geom_boxplot() +
  labs(title = "Distribuição de Fiéis por Religião - Arquidiocese de Vitória",
       x = "Religião",
       y = "Total de Fiéis (valores absolutos)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma)  # Formato numérico legível





