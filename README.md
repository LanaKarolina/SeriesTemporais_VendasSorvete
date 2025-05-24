
# 🍦 Análise de Séries Temporais - Vendas de Sorvete

Este repositório contém uma análise de séries temporais aplicada à base de dados fictícia de vendas de sorvete. O objetivo do projeto é identificar padrões sazonais, tendências e realizar previsões com base em dados mensais.

---

## 🔍 Objetivo

- Visualizar a série histórica de vendas
- Avaliar sazonalidade e tendência
- Aplicar modelos estatísticos simples
- Validar previsões com gráficos e métricas

---

## 📂 Estrutura do Projeto

```
├── SeriesTemporais_Vendas_Sorvete.Rmd    # RMarkdown com a análise
├── docs/
│   └── SeriesTemporais_Vendas_Sorvete.html  # Relatório renderizado (GitHub Pages)
└── SeriesTemporais_VendasSorvete.Rproj     # Arquivo do projeto RStudio
```

---

## 🌐 Relatório Online

O relatório renderizado está disponível em:

👉 [Clique aqui para visualizar](https://lanakarolina.github.io/SeriesTemporais_VendasSorvete/SeriesTemporais_Vendas_Sorvete.html)

---

## 🚀 Como reproduzir localmente

1. Clone o repositório:
```bash
git clone https://github.com/LanaKarolina/SeriesTemporais_VendasSorvete.git
```

2. Abra o projeto `.Rproj` no RStudio.

3. Instale os pacotes necessários (caso ainda não tenha):
```r
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")
```

4. Renderize o relatório:
```r
rmarkdown::render("SeriesTemporais_Vendas_Sorvete.Rmd", output_dir = "docs")
```

---

## 🧊 Observações

- Os dados são fictícios e simulam uma tendência sazonal comum a produtos como sorvetes.
- A publicação do relatório usa GitHub Pages com o arquivo `.html` na pasta `docs/`.

---

Feito com 💙 e muito gelo por Lana Karolina 🧁
