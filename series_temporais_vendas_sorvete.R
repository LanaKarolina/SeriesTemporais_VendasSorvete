library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats) # trabalhando com variáveis do tipo fator

# libs de series temporais
library(TSA)
library(forecast)
library(lmtest)
library(urca)
library(tsibble)

##### Leitura dos dados
dados_originais = fread("~/Library/CloudStorage/OneDrive-Pessoal/Pessoal/DataSets/Dataset_de_Sorvetes.csv")
dados = dados_originais
names(dados) = c("data", "sabor", "estabelecimento", "preco", "tamanho", "vendas");head(dados)

##### Analise exploratória
summary(dados$data)
table(dados$sabor)
table(dados$estabelecimento)
summary(dados$preco)
table(dados$tamanho)
summary(dados$vendas)

dados$data = as.Date(dados$data)
dados

## 1. Quais são as características da venda de 80 unidades de sorvete?

dados %>% filter(vendas == max(vendas)) # 80 casquinhas foram vendidas a 3,61 

## 2. Criando nova variável: preco total

dados = dados %>% mutate(preco_total = preco*vendas); head(dados)

summary(dados$preco_total)

# 2.1 Quais são as características do valor total de 1.081,73?

dados %>% filter(preco_total == max(preco_total))

#### Analise grafica

### 1. Sabor e Vendas totais

plot_sabor = dados %>% group_by(sabor) %>% summarise('vendas_total' = sum(vendas)) %>% 
  slice_max(order_by = vendas_total, n = 5) %>%
  mutate(sabor = fct_reorder(sabor, -vendas_total)) %>%
  ggplot(aes(y = vendas_total, x = sabor, fill = sabor)) + 
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = comma(vendas_total, big.mark = ".", decimal.mark = ",")), 
    hjust = -0.1, size = 3.5, fill = "white", color = "black") +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  theme_minimal()

### 1.1 Usando plotly

library(plotly)

# Definindo os dados

plot_sabor = dados %>%
  group_by(sabor) %>%
  summarise(vendas_total = sum(vendas)) %>%
  slice_max(order_by = vendas_total, n = 5) %>%
  mutate(sabor = fct_reorder(sabor, -vendas_total))

# Definindo as cores

cores_sabores = c(
  "Chocolate" = "#8B4513",     # marrom
  "Morango" = "#FF69B4",       # rosa
  "Creme" = "#FFFACD",         # amarelo claro
  "Limão" = "#98FB98",         # verde claro
  "Uva" = "#8A2BE2"             # violeta
)

# Gerando o gráfico em plotly
plot_ly(
  data = plot_sabor,
  x = ~vendas_total,
  y = ~sabor,
  type = 'bar',
  orientation = 'h',
  text = ~comma(vendas_total, big.mark = ".", decimal.mark = ","),
  textposition = 'auto',
  marker = list(color = ~cores_sabores[sabor],
    line = list(color = 'rgba(0,0,0,0.5)', width = 1.5))
) %>%
  layout(
    title = list(text = "Sabores por Vendas Totais",
      y = 0.95), # ajustando a posição vertical do título (1 = topo, 0 = base)
    xaxis = list(title = "Vendas Totais", tickformat = ",d"),
    yaxis = list(title = "Sabor", categoryorder = "total ascending"),
    margin = list(l = 100, t = 40) # aumenta o espaço do topo
  )


#### 2. Sabor e preço médio/ticket medio

### ggplot
plot_sabor2 = dados %>%
  group_by(sabor) %>%
  summarise(
    receita_total = sum(preco_total),
    unidades_vendidas = sum(vendas),
    ticket_medio = round(receita_total / unidades_vendidas,2)) %>%
  slice_max(order_by = ticket_medio, n = 5) %>%
  mutate(sabor = fct_reorder(sabor, ticket_medio))

plot_sabor2$cor = cores_sabores[as.character(plot_sabor2$sabor)]; plot_sabor2

plot_sabor2_1 = plot_sabor2 %>%
  ggplot(aes(y = ticket_medio, x = sabor, fill = sabor)) + 
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = paste0('R$ ',comma(ticket_medio, big.mark = ".", decimal.mark = ","))), 
    hjust = -0.1, size = 3.5, fill = "white", color = "black") +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = cores_sabores) +
  labs(y = 'Ticket Médio', x = 'Sabor', title = 'Ticket Médio dos Sabores') +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5))

### plotly

plot_ly(
  data = plot_sabor2,
  x = ~ticket_medio,
  y = ~sabor,
  type = 'bar',
  orientation = 'h',
  text = ~paste0("R$ ", comma(ticket_medio, big.mark = ".", decimal.mark = ",")),
  textposition = 'auto',
  marker = list(color = ~cor,
    line = list(color = 'rgba(0,0,0,0.5)', width = 1.5))
) %>%
  layout(
    title = list(
      text = "Ticket Médio dos Sabores",
      y = 0.95
    ),
    xaxis = list(title = "Ticket Médio", tickprefix = "R$ ", tickformat = ".2f"),
    yaxis = list(title = "Sabor", categoryorder = "total ascending"),
    margin = list(l = 100, t = 40)
    )

#### 3. Vendas e Ticket Medio por Sabor

sabor_comb = dados %>% group_by(sabor) %>%
  summarise(vendas_total = sum(vendas),
    receita_total = sum(preco_total),
    ticket_medio = receita_total / vendas_total
    ) %>%
  mutate(
    sabor = fct_reorder(sabor, vendas_total),
    vendas_norm = vendas_total / max(vendas_total), #normalizado para 0-1
    ticket_norm = ticket_medio / max(ticket_medio)  #normalizado para 0-1
  ); sabor_comb

### ggplot2

plot_comb = sabor_comb %>% ggplot(aes(x = sabor)) +
  geom_col(aes(y = vendas_norm, fill = sabor), width = 0.6, show.legend = FALSE) +
  geom_line(aes(y = ticket_norm, group = 1), color = 'black', size = 1.2) +
  geom_point(aes(y = ticket_norm), color = 'black', size = 3) + 
  scale_fill_manual(values = cores_sabores) +
  scale_y_continuous(
    name = 'Vendas (normalizado)',
    # criando o eixo secundário
    sec.axis = sec_axis(~ . * max(sabor_comb$ticket_medio), # transformando o ticket_norm para ticket_medio (escala original)
      name = 'Ticket Médio (R$)',
      labels = function(x) paste0('R$ ', formatC(x, format = 'f', digits = 2,
        big.mark = '.', decimal.mark = ',')))
  ) +
  labs(
    title = "Vendas Totais vs Ticket Médio por Sabor",
    x = "Sabor"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

### plotly

plot_ly() %>%
  add_bars(
    data = sabor_comb,
    x = ~sabor,
    y = ~vendas_total,
    name = "Vendas Totais",
    marker = list(color = ~cores_sabores[as.character(sabor)])
  ) %>%
  add_lines(
    data = sabor_comb,
    x = ~sabor,
    y = ~ticket_medio,
    yaxis = "y2",
    name = "Ticket Médio",
    line = list(color = "black", width = 2),
    mode = "lines+markers",
    marker = list(color = "black", size = 8),
    text = ~paste0("R$ ", formatC(ticket_medio, format = "f", digits = 2,
      big.mark = ".", decimal.mark = ",")),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(text = "Vendas Totais vs Ticket Médio por Sabor", y = 0.95),
    xaxis = list(title = "Sabor"),
    yaxis = list(
      title = "Vendas Totais",
      range = c(0, max(sabor_comb$vendas_total) * 1.1)
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = list(
        text = "Ticket Médio (R$)",
        standoff = 20
      ),
      tickprefix = "R$ ",
      tickformat = ".2f",
      tickfont = list(color = "black"),
      titlefont = list(color = "black"),
      range = c(0, max(sabor_comb$ticket_medio) * 1.2),
      showgrid = FALSE #,standoff = 20
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = 1.15,
      xanchor = "center"
    ),
    margin = list(t = 100, r = 100)
  )

#### 4. Vendas e Ticket Medio por Estabelecimento

estabelecimento_comb = dados %>% group_by(estabelecimento) %>%
  summarise(vendas_total = sum(vendas),
    receita_total = sum(preco_total),
    ticket_medio = receita_total / vendas_total
  ) %>%
  mutate(
    estabelecimento = fct_reorder(estabelecimento, vendas_total),
    vendas_norm = vendas_total / max(vendas_total), #normalizado para 0-1
    ticket_norm = ticket_medio / max(ticket_medio)  #normalizado para 0-1
  ); estabelecimento_comb

### ggplot2

estabelecimento_comb %>% ggplot(aes(x = estabelecimento)) +
  geom_col(aes(y = vendas_norm, fill = estabelecimento), width = 0.6, show.legend = FALSE) +
  geom_line(aes(y = ticket_norm, group = 1), color = 'black', size = 1.2) +
  geom_point(aes(y = ticket_norm), color = 'black', size = 3) + 
  #scale_fill_manual(values = cores_sabores) +
  scale_y_continuous(
    name = 'Vendas (normalizado)',
    # criando o eixo secundário
    sec.axis = sec_axis(~ . * max(estabelecimento_comb$ticket_medio), 
      name = 'Ticket Médio (R$)',
      labels = function(x) paste0('R$ ', formatC(x, format = 'f', digits = 2,
        big.mark = '.', decimal.mark = ',')))
  ) +
  labs(
    title = "Vendas Totais vs Ticket Médio por Estabelecimento",
    x = "Estabelecimento"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

### plotly

plot_ly() %>%
  add_bars(
    data = estabelecimento_comb,
    x = ~estabelecimento,
    y = ~vendas_total,
    color = ~estabelecimento,
    name = "Vendas Totais",
    showlegend = FALSE
  ) %>%
  add_lines(
    data = estabelecimento_comb,
    x = ~estabelecimento,
    y = ~ticket_medio,
    yaxis = "y2",
    name = "Ticket Médio",
    line = list(color = "black", width = 2),
    mode = "lines+markers",
    marker = list(color = "black", size = 8),
    text = ~paste0("R$ ", formatC(ticket_medio, format = "f", digits = 2,
      big.mark = ".", decimal.mark = ",")),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(text = "Vendas Totais vs Ticket Médio por Estabelecimento", y = 0.95),
    xaxis = list(title = "Estabelecimento"),
    yaxis = list(
      title = "Vendas Totais",
      range = c(0, max(estabelecimento_comb$vendas_total) * 1.1)
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = list(
        text = "Ticket Médio (R$)",
        standoff = 20
      ),
      tickprefix = "R$ ",
      tickformat = ".2f",
      tickfont = list(color = "black"),
      titlefont = list(color = "black"),
      range = c(0, max(estabelecimento_comb$ticket_medio) * 1.2),
      showgrid = FALSE #,standoff = 20
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = 1.15,
      xanchor = "center"
    ),
    margin = list(t = 40, r = 100)
  )

#### 4. Análise cruzada entre estabelecimento e tamanho

estab_tam = dados %>%
  group_by(estabelecimento, tamanho) %>%
  summarise(
    vendas_total = sum(vendas),
    receita_total = sum(preco_total),
    ticket_medio = receita_total / vendas_total,
    .groups = "drop"
  ) %>%
  group_by(tamanho) %>%
  mutate(
    pct_vendas_coluna = vendas_total / sum(vendas_total),
    pct_receita_coluna = receita_total / sum(receita_total),
    tamanho = fct_relevel(tamanho, "Casquinha", "250g", "500g", "1kg")
  ) %>%
  ungroup();estab_tam

# Gráfico 1: Heatmap de volume de vendas
plot_vendas = ggplot(estab_tam, aes(x = tamanho, y = estabelecimento, fill = vendas_total)) +
  geom_tile(color = "white") +
  geom_text(aes(label = vendas_total), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#d0e1f9", high = "#08306b", name = "Vendas Totais") +
  labs(title = "Volume de Vendas por Estabelecimento e Tamanho",
    x = "Tamanho do Sorvete", y = "Estabelecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Gráfico 2: Heatmap de ticket médio
plot_ticket = ggplot(estab_tam, aes(x = tamanho, y = estabelecimento, fill = ticket_medio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0("R$ ", formatC(ticket_medio, digits = 2, format = "f", decimal.mark = ",", big.mark = "."))), 
    color = "black", size = 3.5) +
  scale_fill_gradient(low = "#ffe6e6", high = "#990000", name = "Ticket Médio (R$)") +
  labs(title = "Ticket Médio por Estabelecimento e Tamanho",
    x = "Tamanho do Sorvete", y = "Estabelecimento") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#### 5. Analise cruzada entre preço total e vendas por tamanho

plot_dispersao = ggplot(dados, aes(x = vendas, y = preco_total)) +
  geom_point(alpha = 0.4, color = "#1f77b4") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "Relação entre Vendas e Receita Total",
    x = "Vendas (unidades)",
    y = "Receita Total (R$)"
  ) +
  facet_wrap(~ tamanho) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#### 6. Dispersão das vendas por sabor dentro de cada tamanho

plot_dispersao_facet = ggplot(dados, aes(x = vendas, y = preco_total, color = sabor)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ tamanho) +
  labs(
    title = "Dispersão de Vendas vs Receita Total por Sabor e Tamanho",
    x = "Vendas (unidades)",
    y = "Receita Total (R$)",
    color = "Sabor"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

##### Testes de hipóteses

### 1. Ticket Médio entre os Estabelecimentos

# H0: as médias de ticket médio são iguais entre os estabelecimentos
# H1: pelo menos uma média é diferente

## preparando os dados
dados_teste1 = dados %>% group_by(data, estabelecimento) %>%
  summarise(vendas_total = sum(vendas), receita_total = sum(preco_total)) %>% 
  mutate(ticket_medio = receita_total / vendas_total); head(dados_teste1)

## anova

aov1 = aov(ticket_medio ~ estabelecimento, data = dados_teste1)
summary(aov1) # pelo menos um estabelecimento não difere de outros

# verificando se os residuos pertencem a distribuição normal
shapiro.test(residuals(aov1))

# verificando se as variâncias são homogêneas
car::leveneTest(ticket_medio ~ as.factor(estabelecimento), data = dados_teste1) # variâncias heterogeneas

## anova de Welch

oneway.test(ticket_medio ~ estabelecimento, data = dados_teste1, var.equal = FALSE)

## teste de kruskal-wallis (não paramétrico)

kruskal.test(ticket_medio ~ estabelecimento, data = dados_teste1)

### 2. Ticket Médio entre os Sabores

# H0: as médias de ticket médio são iguais entre os sabores
# H1: pelo menos uma média é diferente

## preparando os dados
dados_teste2 = dados %>% group_by(data, sabor) %>%
  summarise(vendas_total = sum(vendas), receita_total = sum(preco_total)) %>% 
  mutate(ticket_medio = receita_total / vendas_total); head(dados_teste2)

## anova

aov2 = aov(ticket_medio ~ sabor, data = dados_teste2)
summary(aov2) # os sabores são diferentes

# verificando se os residuos pertencem a distribuição normal
shapiro.test(residuals(aov2)) # não pertencem a uma distribuição normal

# verificando se as variâncias são homogêneas
car::leveneTest(ticket_medio ~ as.factor(sabor), data = dados_teste2) # variâncias heterogeneas

## anova de Welch

oneway.test(ticket_medio ~ sabor, data = dados_teste2, var.equal = FALSE)

## teste de kruskal-wallis (não paramétrico)

kruskal.test(ticket_medio ~ sabor, data = dados_teste2)

## analisando quais sabores se diferenciam entre si (não paramétrico)
#install.packages("FSA")
library(FSA)

duntest_sabor = dunnTest(ticket_medio ~ as.factor(sabor), data = dados_teste2, method = "bonferroni")

#-- O sabor Uva é o que se diferencia dos demais sabores, exceto o sabor Limão! --#

## analise gráfica

# 1.boxplot com os p-values

library(ggsignif)

ggplot(dados_teste2, aes(x = sabor, y = ticket_medio, fill = sabor)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.6) +
  geom_signif(
    comparisons = list(
      c("Uva", "Chocolate"),
      c("Uva", "Creme"),
      c("Uva", "Morango")
    ),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01
  ) +
  labs(
    title = "Comparação do Ticket Médio entre Sabores",
    y = "Ticket Médio", x = "Sabor"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 2. tabela de letras com multcompView (resumo por grupo)

library(multcompView)

# Executa o teste de Dunn
dunn = dunnTest(ticket_medio ~ as.factor(sabor), data = dados_teste2, method = "bonferroni")

# Extrai os resultados em data.frame
dunn_df = dunn$res

# Reorganiza para formato necessário: nomes dos pares como rownames e p-values ajustados
pvals <- dunn_df %>%
  select(Comparison, P.adj) %>%
  tibble::deframe()  # transforma em named vector

# Limpa os nomes das comparações (removendo espaços)
library(stringr)

names(pvals) <- str_replace_all(names(pvals), " ", "")

# Gera as letras com base nos p-valores ajustados
letras <- multcompLetters(pvals, threshold = 0.05)

# Monta data frame com sabores e suas letras

letras_df <- tibble::enframe(letras$Letters, name = "sabor", value = "grupo") %>%
  mutate(sabor = str_trim(sabor)) %>%
  distinct()

print(letras_df)

## unindo a visualização do boxplot com a das letras

# definindo um df para adicionar as letras de cada sabor
y_pos = dados_teste2 %>% group_by(sabor) %>% summarise(maximo = max(ticket_medio)) # definindo os maximos
anotacoes = left_join(letras_df, y_pos, by = 'sabor')

# gerando o grafico
plot_box_grupos = ggplot(dados_teste2, aes(x = sabor, y = ticket_medio, fill = sabor)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.6) +
  geom_signif(
    comparisons = list(
      c("Uva", "Chocolate"),
      c("Uva", "Creme"),
      c("Uva", "Morango")
    ),
    map_signif_level = TRUE,
    textsize = 4,
    tip_length = 0.01
  ) +
  labs(
    title = "Comparação do Ticket Médio entre Sabores",
    y = "Ticket Médio", x = "Sabor"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_text(
    data = anotacoes,
    aes(x = sabor, y = maximo*1.05, label = grupo),  
    inherit.aes = FALSE,
    vjust = -0.5,
    fontface = "bold"
  )

##### Analise de correlação

#### 1. vendas x preco_total

cor(dados$vendas, dados$preco_total, method = "pearson") # 50% de correlação

#### 2. sabor x tamanho

library(rstatix) # craner_v() avaliar correlação entre categorica x categorica
table(dados$sabor, dados$tamanho) %>% cramer_v() # 0 = sem associação

#### 3. sabor x preco_total

library(DescTools) # EtaSq(aov()) avalia correlação entre categorica e numerica
EtaSq(aov(preco_total ~ sabor, data = dados), type = 1) # acima de 0.14 = forte associação

#### 4. tamanho x preco_total
EtaSq(aov(preco_total ~ tamanho, data = dados), type = 1) # acima de 0.14 = forte associação

#### 4. estabelecimento x preco_total
EtaSq(aov(preco_total ~ estabelecimento, data = dados), type = 1) # baixa correlação

##### Regressão linear

#### 1. preco_total como variável explicativa

modelo_1 = lm(preco_total ~ vendas + tamanho + sabor + estabelecimento, data = dados)
summary(modelo_1)

### 1.1 retirando 'estabelecimento' por não ser significativa ao modelo

modelo_2 = lm(preco_total ~ vendas + tamanho + sabor, data = dados)
summary(modelo_2)

### 1.2 comparando os modelos
anova(modelo_1, modelo_2) # p-valor alto siginifica que o modelo_2 é tão nom quanto o completo
AIC(modelo_1, modelo_2) # modelo_2 possui uma diferença mínima, porém é o melhor

##### Analisando Multicolinariedade

#-- VIF: Variance Inflation Factor: 1 / (1 - Rˆ2) -> mede o quanto a variância de um coeficiente é inflada devido 
#-- colinariedade com outras variáveis

#-- <=1: sem colinariedade
#-- >1 e <= 5: colinariedade moderada (aceitável)
#-- >5 e <10: colinariedade alta
#-- >=10: colinariedade severa

library(car)

vif(modelo_2) # baixíssima colinariedade!

##### Análise de Séries temporais

#### 1. Preparando os dados

dados_semanais = dados %>% group_by(data) %>% 
  summarise(vendas_total = sum(vendas), 
    receita_total = sum(preco_total)) %>%
  mutate(ticket_medio = receita_total / vendas_total); head(dados_semanais)

#### 2. Convertendo para objeto de série temporal

dados_ts = as_tsibble(dados_semanais, index = data); dados_ts

#### 3. Visualizando graficamente

dados_ts %>% ggplot(aes(x = data, y = vendas_total)) +
  geom_line(color = '#1f77b4') + 
  labs(title = 'Série Temporal de Vendas Semanais', x = 'Data', y = 'Vendas Totais') +
  theme_minimal()

### 3.1 Decompondo a série: Tendência, Sazonalidade e Ruído

library(feasts)
library(fabletools)

class(dados_ts)

decomposicao = dados_ts %>% model(STL(vendas_total ~ season(window = 'periodic'))) %>% components();decomposicao

autoplot(decomposicao) + labs(title = "Decomposição STL da Série de Vendas Semanais")

## explorando a sazonalidade anual

gg_season(dados_ts, vendas_total)

#### 4. Analisando ACF e PACF

#-- ACF: Autocorrelation Funcion (Função de Autocorrelação)
#-- mede a correlação entre a série e ela mesma com diferentes defasagens (lags)
#-- Ex: ACF com lag = 1 -> correlação entre y_t e y_(t-1) # entre hoje e (periodo - 1)
#--     ACF com lag = 2 -> correlação entre y_t e t_(t-2) # entre hoje e (periodo - 2)
#-- serve para diagnosticar se a série é não estacionária (valores altos e persistentes em lags iniciais)

#-- PACF: Partial Autocorrelation Function (Autocorrelação Parcial)
#-- mede a correlação entre y_t e y_(t-k), removendo o efeito intermediário dos lags anteriores
#-- Ex: PACF com lag = 3 -> correlação entre y_t e y_(t-3), removendo o efeito dos lags 1 e 2
#-- serve para identificar o número de termos AR (AutoRegressivos) no modelo ARIMA
#-- e também para distinguir dependência direta vs. indireta em lags maiores

#-- Em modelagem ARIMA o componente AR(p) é o número de lags significativos no PACF,
#-- já o componente MA(q) é o número de lags significativos no ACF

## Gráfico de ACF
acf_plot = dados_ts %>%
  ACF(vendas_total) %>%
  autoplot() +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  labs(
    title = "ACF - Autocorrelação de Vendas Totais Semanais",
    x = "Lag (semanas)",
    y = "Autocorrelação"
  ) +
  theme_minimal()

## Gráfico de PACF
pacf_plot = dados_ts %>%
  PACF(vendas_total) %>%
  autoplot() +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  labs(
    title = "PACF - Autocorrelação Parcial de Vendas Totais Semanais",
    x = "Lag (semanas)",
    y = "Autocorrelação Parcial"
  ) +
  theme_minimal()

#### 5. Estacionariedade

### 5.1 Teste Dickey-Fuller Aumentado (ADF)

#-- H0: a série tem raiz unitária -> não estacionária
#-- p-value <= 0.05: série estacionária
#-- p-value > 0.05: precisa de diferenciação 

library(tseries)

adf.test(dados_ts$vendas_total) # a série é estacionária (no modelo ARIMA, d=0)

##### 6. Modelo ARIMA
#-- modelo sugerido pelo ChatGPT 4: ARIMA(1,0,1)

library(fable)

modelo_arima_1 = dados_ts %>% model(ARIMA(vendas_total ~ pdq(1, 0, 1)))
report(modelo_arima_1)

#### 6.1 Comparando com o auto.arima

modelo_arima_2 = dados_ts %>% model(ARIMA(vendas_total))
report(modelo_arima_2)

##### 7. Comparando vários modelos de uma só vez

library(fable)
library(fabletools)
library(feasts)
library(fable.prophet)

dados_ts$data = as.Date(dados_ts$data)
dados_ts = as_tsibble(dados_ts);dados_ts

# Ajustar vários modelos de uma vez
modelos_comparacao <- dados_ts %>%
  model(
    arima = ARIMA(vendas_total),
    ets = ETS(vendas_total),
    holt = ETS(vendas_total ~ error("A") + trend("A") + season("N")),
    rw_drift = RW(vendas_total ~ drift())
  )

# Ver relatório de cada modelo
report(modelos_comparacao)

#-- o melhor modelo continua sendo o modelo_arima_2: ARIMA(2,0,0) !

##### 8. Analise de Resíduos do Modelo Escolhido: modelo_arima_2

#-- Os modelos devem deixar apenas aleatoriedade pura. Chamamos isso de ruído branco.
#-- Ou seja, Media aprox 0, variância constante, sem autocorrelação, sem padrões (tendencia, sazonalidade, etc)

library(fabletools)

gg_tsresiduals(modelo_arima_2)

#### 8.1 Avaliando se os resíduos são homocedásticos

#-- Teste de Breusch-Pagan: lmtest::bptest()

### 8.1.1 Tratando os dados para estarem no padrão lm

library(broom)

residuos_df = augment(modelo_arima_2);head(residuos_df)

### 8.1.2 Rodando o teste

#-- H0: os resíduos são homocedásticos
#-- H1: os resíduos são heterocedásticos

library(lmtest)

bptest(.resid ~ .fitted, data = residuos_df) # os resíduos são homocedásticos!

##### 9. Fazendo as previsões

#### 9.1 Gerar previsão para 12 semanas futuras

### 9.1.1 Rodando o modelo com o novo formato de data

modelo_arima_2 = dados_ts %>% model(ARIMA(vendas_total))

### 9.1.2 Gerando a previsão

previsao_12s = forecast(modelo_arima_2, h = 12, level = 95);previsao_12s

### 9.1.3 Desempacotando o intervalo de confiança -> está info para o campo 'vendas_total'

previsao_12s_lims = previsao_12s %>% hilo(level = 95) %>% as_tibble() %>%
  mutate(
    lower_95 = `95%`$lower,
    upper_95 = `95%`$upper
  ); previsao_12s_lims

ggplot() +
  geom_line(data = dados_ts, aes(x = data, y = vendas_total), color = "black") +
  geom_line(data = previsao_12s_lims, aes(x = data, y = .mean), color = "blue") +
  geom_line(data = previsao_12s_lims, aes(x = data, y = lower_95), linetype = "dashed", color = "gray40") +
  geom_line(data = previsao_12s_lims, aes(x = data, y = upper_95), linetype = "dashed", color = "gray40") +
  labs(
    title = "Previsão de Vendas Totais - 12 Semanas (com Intervalo 95%)",
    x = "Data", y = "Vendas Totais"
  ) +
  theme_minimal()

autoplot(previsao_12s, dados_ts) +
  scale_fill_manual(values = c("95" = scales::alpha("blue", 0.2))) +  # cor mais suave
  labs(
    title = "Previsão de Vendas Totais - 12 Semanas (com Intervalo 95%)",
    x = "Data",
    y = "Vendas Totais"
  ) +
  theme_minimal()

#### 9.2 Gerando previsões para 25 semanas (quase 1 semestre)

# Gerar previsão de 25 semanas
previsao_25s = forecast(modelo_arima_2, h = 25, level = 95); previsao_25s

autoplot(previsao_25s, dados_ts) +
  scale_fill_manual(values = c("95" = scales::alpha("blue", 0.2))) +
  labs(
    title = "Previsão de Vendas Totais - 25 Semanas (com Intervalo 95%)",
    x = "Data",
    y = "Vendas Totais"
  ) +
  theme_minimal()

#-- Vemos que a previsão possui valores constantes, o que indica que o modelo não levou em consideração sazonalidades 
#-- mensais ou ciclos anuais -> os dados que pedi para o chat gpt criar não possuem sazonalidade solicitada

##### 10. Previsão por subgrupos (sabor, tamanho ou estabelecimento)

#### 10.1 Arima por sabor

### 10.1.1 Preparação dos dados

dados_sabor = dados %>% mutate(data = as.Date(data)) %>% group_by(data, sabor) %>%
  summarise(vendas_total = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = data, key = sabor); dados_sabor

### 10.1.2 Definição do modelo ARIMA

modelo_por_sabor =  dados_sabor %>% model(ARIMA(vendas_total))
report(modelo_por_sabor)

### 10.1.3 Previsão

previsao_por_sabor = modelo_por_sabor %>% forecast(h = 25);previsao_por_sabor

# gerando o gráfico
autoplot(previsao_por_sabor, dados_sabor) +
  labs(
    title = "Previsão de Vendas Totais por Sabor - 25 Semanas",
    x = "Data", y = "Vendas Totais"
  ) +
  theme_minimal()

### 10.1.4 Acurácia

library(fabletools)

# Calcular acurácia para cada tamanho
acuracia_por_sabor = accuracy(modelo_por_sabor); acuracia_por_sabor

#### 10.2 Arima por tamanho

### 10.2.1 Preparação dos dados

dados_tamanho = dados %>% mutate(data = as.Date(data)) %>% group_by(data, tamanho) %>%
  summarise(vendas_total = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = data, key = tamanho); dados_tamanho

### 10.2.2 Definição do modelo ARIMA

modelo_por_tamanho =  dados_tamanho %>% model(ARIMA(vendas_total))
report(modelo_por_tamanho)

### 10.2.3 Previsão

previsao_por_tamanho = modelo_por_tamanho %>% forecast(h = 25);previsao_por_tamanho

# gerando o gráfico
autoplot(previsao_por_tamanho, dados_tamanho) +
  labs(
    title = "Previsão de Vendas Totais por Tamanho - 25 Semanas",
    x = "Data", y = "Vendas Totais"
  ) +
  theme_minimal()

### 10.2.4 Acurácia

# Calcular acurácia para cada tamanho
acuracia_por_tamanho = accuracy(modelo_por_tamanho); acuracia_por_tamanho

#### 10.3 Arima por estabelecimento

### 10.3.1 Preparação dos dados

dados_estabelecimento = dados %>% mutate(data = as.Date(data)) %>% group_by(data, estabelecimento) %>%
  summarise(vendas_total = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = data, key = estabelecimento); dados_estabelecimento

### 10.3.2 Definição do modelo ARIMA

modelo_por_estabelecimento =  dados_estabelecimento %>% model(ARIMA(vendas_total))
report(modelo_por_estabelecimento)

### 10.3.3 Previsão

previsao_por_estabelecimento = modelo_por_estabelecimento %>% forecast(h = 25);previsao_por_estabelecimento

# gerando o gráfico
autoplot(previsao_por_estabelecimento, dados_estabelecimento) +
  labs(
    title = "Previsão de Vendas Totais por Tipo de Estabelecimento - 25 Semanas",
    x = "Data", y = "Vendas Totais"
  ) +
  theme_minimal()

### 10.3.4 Acurácia

# Calcular acurácia para cada tamanho
acuracia_por_estabelecimento = accuracy(modelo_por_estabelecimento); acuracia_por_estabelecimento

#### 10.4 Arima Sabor e Tamanho (Análise cruzada)

### 10.4.1 Preparação dos dados

dados_sabor_tam_ts = dados %>% group_by(data, sabor, tamanho) %>%
  summarise(vendas_total = sum(vendas), .groups = "drop") %>%
  as_tsibble(index = data, key = c(sabor, tamanho));dados_sabor_tam_ts

### 10.4.2 Definição do modelo Arima

modelo_por_sabor_tam = dados_sabor_tam_ts %>% model(ARIMA(vendas_total)) 
report(modelo_por_sabor_tam)

### 10.4.3 Previsão

previsao_por_sabor_tam = modelo_por_sabor_tam %>% forecast(h = 25); previsao_por_sabor_tam

# gerando o gráfico
autoplot(previsao_por_sabor_tam, dados_sabor_tam_ts) +
  labs(
    title = "Previsão de Vendas Totais por Sabor e Tamanho - 25 Semanas",
    x = "Data", y = "Vendas Totais"
  ) +
  facet_wrap(sabor ~ tamanho) +
  theme_minimal()

### 10.4.4 Acurácia

acuracia_por_sabor_tam = accuracy(modelo_por_sabor_tam);acuracia_por_sabor_tam
