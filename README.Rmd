---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# PNADc.table

The goal of PNADc.table is to make PNADc tables in an easy and interesting way

## Installation

You can install the development version of pnadcr from [GitHub](https://github.com/migconforto/PNADc.table) with:
      

```{r}
install.packages("devtools")
devtools::install_github("migconforto/PNADc.table")
```

## Example

```{r example}
library(pnadcr)
pnadc_download(2019,1) #downloading the 2019 PNADc first quartile

pnadc_mean(~V403312, ~UF, 2019, 1) #Creating a table of mean monthly gross income by UF (Federation Unit) from the 2019.1 PNADc survey

```

Now let's make a bar graph of the last table

```{r example}
pnadc_plot(~V403312, ~UF, 2019, 1, calculation = "mean", classifier = "V403312", type = 1)
```
