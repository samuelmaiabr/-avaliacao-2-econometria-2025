# Avaliação 2 – Econometria I (2025)

Este repositório contém o script principal e os arquivos auxiliares utilizados na **Avaliação 2** da disciplina de *Econometria I* (Prof.ª Ana Hermeto — Cedeplar/UFMG, 1º semestre/2025).

O exercício explora os seguintes temas: **endogeneidade, variáveis instrumentais (2SLS), dados em painel, efeitos fixos e aleatórios, primeiras diferenças, lags e robustez de especificação**. Tratei deles a partir de uma análise da relação entre **complexidade econômica (ECI)** e **desigualdade de renda**.

---

## Dados

As análises empíricas combinam duas bases de acesso público:

| Fonte                                          | Arquivo                       | Link                                                    | Descrição                                                                       |
| ---------------------------------------------- | ----------------------------- | ------------------------------------------------------- | ------------------------------------------------------------------------------- |
| **World Inequality Database (WID)**            | `wid-data-2024-02-Global.dta` | [wid.world/data](https://wid.world/data/)               | Séries de desigualdade para centenas de países (Gini, shares etc.)              |
| **Atlas of Economic Complexity (Harvard CID)** | `OEC_SITC2_1962_2022.csv`     | [atlas.cid.harvard.edu](https://atlas.cid.harvard.edu/) | Exportações por produto/país (SITC-2) e índices de complexidade econômica (ECI) |

> Para executar o script de forma replicável, salve os dois arquivos acima no diretório `data/raw/`.

---

## Script de Replicação

O arquivo principal do projeto é `Maia-2025-Av-2-Econometrics.R`, que contém comentários detalhados e está dividido em seções que correspondem à numeração da Seção 4 da Avaliação.

O script executa, em ordem, as seguintes tarefas:

* Preparação do painel decadal e da amostra analítica
* Estatísticas descritivas e figuras (Seção 4.1)
* Estimação de modelos Pooled, FE, RE, FD + testes de Hausman e Pesaran (Seção 4.2)
* Testes de endogeneidade e tentativa de 2SLS (Seção 4.3)
* Testes de robustez e heterogeneidade: OCDE vs. Não‐OCDE, defasagens e especificações alternativas (Seção 4.4)

Todos os gráficos e tabelas `.tex` são salvos automaticamente na pasta `output/`.

---

## Reprodutibilidade

A replicação pode ser feita de duas formas:

### 1. Ambiente manual

* Baixe os dados brutos nas fontes indicadas e coloque em `data/raw/`;
* Instale os pacotes listados em `session-info.txt`;
* Execute o script `Maia-2025-Av-2-Econometrics.R`.

### 2. Ambiente automático com `renv` (recomendado)

Este repositório usa `renv` para garantir reprodutibilidade computacional. Para restaurar o ambiente exato:

```r
# No diretório do projeto
renv::restore()
```

---

## Organização do Repositório

```plaintext
Avaliacao-2-Econometria/
┣  data/
┃ ┣  raw/         # Dados originais (WID, Atlas)
┃ ┗  processed/   # Painéis e bases combinadas
┣  output/
┃ ┣  figures/     # Gráficos gerados
┃ ┗  tables/      # Tabelas finais (.tex, .csv)
┣  Maia-2025-Av-2-Econometrics.R   # Script principal de replicação
┣  renv.lock       # Arquivo de dependências para reprodutibilidade
┣  session-info.txt # Informações da sessão R (versões e pacotes)
┗  README.md
```

---

## 📬 Contato

Dúvidas sobre replicação ou execução do código?
Envie e-mail para **samuelmaiabr (at) gmail.com** com o assunto *"ECN I – Avaliação 2"*.

---

\* A replicação é compatível com **R ≥ 4.3**. O arquivo `renv.lock` permite restaurar automaticamente o ambiente. O arquivo `session-info.txt` registra, em texto plano, as versões do R, sistema operacional e pacotes carregados.
