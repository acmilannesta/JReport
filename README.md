# Table1

## Description
This function helps to create journal style "Table 1" for numerical/continuous and categorical columns. 
For numerical/continuous columns, the results will be mean(SD).
For categorical columns, the results will be n(%) 
The output will be saved in word (rft) file.

## Install
`devtools::install_github('acmilannesta/Table1')`

## Usage
Table1(data, numcol = NULL, catcol = NULL, exp_var, output = NULL)

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

| Name        | Exposed           | Unexposed  | P_val
| ------------- |:-------------:| -----:|-----------:|
| b: N  | 20 (34.5)  | 17 (40.5) | 0.163 |
| b: UNK  | 16 (27.6)  |  5 (11.9) ||
|   b: Y  | 22 (37.9)  | 20 (47.6) ||
|      a  | 55.5 (30) | 51.9 (27.2) |0.487|
|      c | 49.2 (31.3) | 53.2 (23.5) |0.459|
