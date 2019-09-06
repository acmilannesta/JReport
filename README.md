# Journal style reporting (R-package "JReport")

## Description
This package helps to create journal style descriptive table (so called "Table 1") and model output table (so called "Table 2"). 


## Install
`devtools::install_github('acmilannesta/JReport')`

## Usage
`Table1(data, numcol = NULL, catcol = NULL, exp_var, output = NULL, overall = TRUE)`
`Table2(model, data, catcols = NULL, esdigits = 2, output = NULL, pdigits = 2, eps = 0.001)`

## Arguments
data: A dataframe including the exposure variable.

numcol: A vector of numerical column names in character. Default to NULL.

catcol: A vector of categorical column names in character. Default to NULL.

exp_var: String of main exposure variable name

output: String of path to store the output word file. E.g., 'Table1.rtf' or 'Table1.doc'

overall: Whether to add a column for overall subjects. Default to TRUE

model: Object output from lm, glm or coxph

esdigits: Controlling the effect size digits. Default to 2.

pdigits: Controlling the significant p-value digits. Default to 2.

eps: P-value tolerane. Those less than eps are formatted as "< [eps]". Default to 0.001.

## Return Value
If output is not specified, a dataframe will be returned. Otherwise, a rtf file will be saved in the specified path.

## Details
Table1
For numerical/continuous columns, the results will be mean(SD). P-value is computed from Kruskal-Wallis Rank Sum test statistic.
<br/>For categorical columns, the results will be n(%). P-value is computed from Chi-square test statistic.

Table2
Currently the function only supports lm, glm and coxph objects. 
<br/>The output include effect size (95% CI) along with p-values. 

## Examples
```
df = data.frame(
  a = sample(1:100, 100, TRUE),
  b = sample(c('Y', 'N', 'UNK'), 100, TRUE, prob=c(0.5, 0.3, 0.2)),
  c = sample(1:100, 100, TRUE),
  d = sample(c('Exposed', 'Unexposed'), 100, TRUE, prob=c(0.6, 0.4)))

Table1(df, c('a', 'c'), 'b', 'd')
```

|Name |Overall (n=100) |Exposed (n=60) |Unexposed (n=40)  |   P_val
|:-------------|:-------------:|:-----:|:-----------:|:------------:|
|b: N      |   28 (28)       | 18 (30)        |  10 (25)    | 0.804|
|b: UNK    |   27 (27)    |    15 (25)        |  12 (30)||
|b: Y      |   45 (45)       | 27 (45)       |   18 (45)||
|a        |    54.2 (28.6)  |  52.2 (28.3)   |   57.2 (29.2) |0.408|
|c         |   50.1 (31)   | 50.1 (29.7)    |    50.1 (33.3) |0.935|


```
data(mtcars)
mtcars$am = factor(mtcars$am)
log = glm(vs==1~mpg+am, family='binomial', data=mtcars)

Table2(log, mtcars, 'am')
```
|Variable   |  ES_CI        |      P_val
  |:-------------|:-------------:|:-----:|
(Intercept)  |  0 (0, 0.03)  |      0.006
mpg     |       1.98 (1.2, 3.24)  | 0.007
am||
  &nbsp;&nbsp;0    |        Ref         |       Ref
  &nbsp;&nbsp;1     |       0.05 (0, 1.14)    | 0.060

