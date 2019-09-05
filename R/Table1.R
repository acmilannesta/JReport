
#' Table1 function
#'
#' This function helps to create journal style "Table 1" for numerical/continuous and categorical columns.
#' For numerical/continuous columns, the results will be mean(SD)
#' For categorical columns, the results will be n(%)
#' The output will be saved in word file.
#' @param data A dataframe including the exposure variable.
#' @param numcol A vector of numerical column names in character. Default to NULL.
#' @param catcol A vector of categorical column names in character. Default to NULL.
#' @param exp_var String of main exposure variable name
#' @param output String of path to store the output word file. For example, 'Table1.doc' or 'Table1.rtf'
#' @return  If output is not specified, a dataframe will be returned. Otherwise, a rtf file will be saved in the specified path.
#' @export
#' @examples
#' df = data.frame(
#' a = sample(1:100, 100, TRUE),
#' b = sample(c('Y', 'N', 'UNK'), 100, TRUE, prob=c(0.5, 0.3, 0.2)),
#' c = sample(1:100, 100, TRUE),
#' d = sample(0:1, 100, TRUE, prob=c(0.6, 0.4)))
#'
#' Table1(df, c('a', 'c'), 'b', 'd')
#'     name     Exposed   Unexposed P_val
#' 1   b: N   20 (34.5)   17 (40.5) 0.163
#' 2 b: UNK   16 (27.6)    5 (11.9)
#' 3   b: Y   22 (37.9)   20 (47.6)
#' 4      a   55.5 (30) 51.9 (27.2) 0.487
#' 5      c 49.2 (31.3) 53.2 (23.5) 0.459

Table1 = function(data, numcol = NULL, catcol = NULL, exp_var, output = NULL){
  pkg = c('dplyr', 'rtf')
  for (p in pkg){
    if (!p %in% rownames(installed.packages())) install.packages(p)
    library(p, character.only = T)
  }
  
  data[[exp_var]] = as.factor(data[[exp_var]])
  for(level in levels(data[[exp_var]])){
    data1 = data %>% filter(!!sym(exp_var)==level)
    df = data.frame()
    for(col in catcol){
      a = table(droplevels(data1[col]))
      b = round(prop.table(a)*100, 1)
      p = format.pval(chisq.test(table(data[[col]], data[[exp_var]]))$p.value, 3, 0.001)
      tmp = data.frame(a) %>%
        left_join(data.frame(b) %>% rename(Pct=Freq), by='Var1') %>%
        rename(name=Var1) %>%
        mutate(es_cl = paste0(Freq, ' (', Pct, ')'),
               name = paste0(!!col, ': ', name)) %>%
        select(-c(Freq, Pct))
      if(level==tail(levels(data[[exp_var]]), n=1)){
        tmp = tmp %>% mutate(P_val=ifelse(row_number()==1, p, ''))
      }
      df = rbind(df, tmp)
    }
    for(col in numcol){
      es = colMeans(data1[col], na.rm = T)
      sd = sqrt(var(data1[col], na.rm = T))
      p = format.pval(kruskal.test(as.formula(paste0(col, '~', exp_var)), data=data)$p.value, 3, 0.001)
      tmp = data.frame(name = col,
                       es_cl = paste0(round(es, 1), ' (', round(sd, 1), ')'))
      if(level==tail(levels(data[[exp_var]]), n=1)){
        tmp = tmp %>% mutate(P_val=ifelse(row_number()==1, p, ''))
      }
      df = rbind(df, tmp)
    }
    if(level==head(levels(data[[exp_var]]), n=1)){
      final = df %>% rename(!!level:=es_cl)
    }
    else{
      final = final %>% left_join(df %>% rename(!!level:=es_cl), by='name')
    }
  }
  if(is.null(output)) return(final)
  else {
    rtffile = RTF(output)
    addTable(rtffile, final, col.justify = c('L', rep('C', length(levels(data[[exp_var]]))+1)), header.col.justify = 'C')
    done(rtffile)
  }
}
