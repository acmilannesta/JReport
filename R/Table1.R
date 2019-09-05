
#' Table1 function
#'
#' This function helps to create journal style "Table 1" for numerical/continuous and categorical columns.
#' For numerical/continuous columns, the results will be mean(SD). P-value is computed from Kruskal-Wallis Rank Sum test statistic.
#' For categorical columns, the results will be n(%). P-value is computed from chi-square test statistic
#' The output will be saved in word file.
#' @title Create journal style characteristic table
#' @description Create journal style characteristic table
#' @param data A dataframe including the exposure variable.
#' @param numcol A vector of numerical column names in character. Default to NULL.
#' @param catcol A vector of categorical column names in character. Default to NULL.
#' @param exp_var String of main exposure variable name
#' @param output String of path to store the output word file. For example, 'Table1.doc' or 'Table1.rtf'
#' @param overall Whether to add a column for overall subjects. Default to TRUE
#' @return  If output is not specified, a dataframe will be returned. Otherwise, a rtf file will be saved in the specified path.
#' @export
#' @examples
#' df = data.frame(
#' a = sample(1:100, 100, TRUE),
#' b = sample(c('Y', 'N', 'UNK'), 100, TRUE, prob=c(0.5, 0.3, 0.2)),
#' c = sample(1:100, 100, TRUE),
#' d = sample(c('Exposed', 'Unexposed'), 100, TRUE, prob=c(0.6, 0.4)))
#'
#' Table1(df, c('a', 'c'), 'b', 'd', overall=TRUE)
#' name Overall (n=100) Exposed (n=60) Unexposed (n=40)     P_val
#' b: N         28 (28)        18 (30)          10 (25)     0.804
#' b: UNK       27 (27)        15 (25)          12 (30)
#' b: Y         45 (45)        27 (45)          18 (45)
#' a            54.2 (28.6)    52.2 (28.3)      57.2 (29.2) 0.408
#' c            50.1 (31)    50.1 (29.7)        50.1 (33.3) 0.935


Table1 = function(data, numcol = NULL, catcol = NULL, exp_var, output = NULL, overall=TRUE){
  for (p in c('dplyr', 'rtf')){
    if (!p %in% rownames(installed.packages())) install.packages(p)
    library(p, character.only = T)
  }

  data[[exp_var]] = as.factor(data[[exp_var]])
  levels = levels(data[[exp_var]])
  if(overall) levels = c('Overall', levels(data[[exp_var]]))
  for(level in levels){
    if(level=='Overall') data1 = data
    else data1 = data %>% filter(!!sym(exp_var)==level)
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
      if(level==tail(levels, n=1)){
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
      if(level==tail(levels, n=1)){
        tmp = tmp %>% mutate(P_val=ifelse(row_number()==1, p, ''))
      }
      df = rbind(df, tmp)
    }
    df = df %>% rename(!!paste0(level, ' (n=', nrow(data1), ')'):=es_cl)
    if(level==head(levels, n=1)){
      final = df
    }
    else{
      final = final %>% left_join(df, by='name')
    }
  }
  if(is.null(output)) return(final)
  else {
    rtffile = RTF(output)
    addTable(rtffile, final, col.justify = c('L', rep('C', length(levels)+1)), header.col.justify = 'C')
    done(rtffile)
  }
}

