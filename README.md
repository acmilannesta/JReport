# Table1

## Description
This function helps to create journal style "Table 1" for numerical/continuous and categorical columns. 
For numerical/continuous columns, the results will be mean(SD).
For categorical columns, the results will be n(%) 
The output will be saved in word (rft) file.

## Install
`devtools::install_github('acmilannesta/Table1')`

## Usage
Table1(data, numcol = NULL, catcol = NULL, exp_var, output = NULL, overall = TRUE)

## Arguments
data	
A dataframe including the exposure variable.

numcol	
A vector of numerical column names in character. Default to NULL.

catcol	
A vector of categorical column names in character. Default to NULL.

exp_var	
String of main exposure variable name

output	
String of path to store the output word file. E.g., 'Table1.rtf' or 'Table1.doc'

overall	
Whether to add a column for overall subjects. Default to TRUE


## Return Value
If output is not specified, a dataframe will be returned. Otherwise, a rtf file will be saved in the specified path.

## Examples
```
df = data.frame(
  a = sample(1:100, 100, TRUE),
  b = sample(c('Y', 'N', 'UNK'), 100, TRUE, prob=c(0.5, 0.3, 0.2)),
  c = sample(1:100, 100, TRUE),
  d = sample(0:1, 100, TRUE, prob=c(0.6, 0.4)))

Table1(df, c('a', 'c'), 'b', 'd')
```



|name |Overall (n=100) |Exposed (n=60) |Unexposed (n=40)  |   P_val
| ------------- |:-------------:| -----:|-----------:|:------------:|
|b: N      |   28 (28)       | 18 (30)        |  10 (25)    | 0.804|
|b: UNK    |   27 (27)    |    15 (25)        |  12 (30)||
|b: Y      |   45 (45)       | 27 (45)       |   18 (45)||
|a        |    54.2 (28.6)  |  52.2 (28.3)   |   57.2 (29.2) |0.408|
|c         |   50.1 (31)   | 50.1 (29.7)    |    50.1 (33.3) |0.935|
