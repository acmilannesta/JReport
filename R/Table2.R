

#' Table2 function
#'
#' This function helps to create journal style "Table 2" (model output).
#' Currently the function only supports lm, glm and coxph objects.
#' \cr The output include effect size (95\% CI) along with p-values.
#' \cr The output will be saved in word file.
#' @title Create journal style model output table.
#' @description Create journal style model output table.
#' @param model Object output from lm, glm or coxph
#' @param data A dataframe including the exposure variable.
#' @param catcol A vector of factrozied categorical column names in character.  Default to NULL.
#' @param esdigits Controlling the effect size digits. Default to 2.
#' @param pdigits Controlling the significant p-value digits. Default to 2.
#' @param eps P-value tolerane. Those less than eps are formatted as "< [eps]". Default to 0.001
#' @param output String of path to store the output word file. For example, 'Table2.doc' or 'Table2.rtf'
#' @return  If output is not specified, a dataframe will be returned. Otherwise, a rtf file will be saved in the specified path.
#' @export
#' @examples
#'
#' data(mtcars)
#'
#' log = glm(vs==1~mpg+am, family='binomial', data=mtcars)
#'
#'   Variable     ES_CI              P_val
#' (Intercept)    0 (0, 0.03)        0.006
#' mpg            1.98 (1.2, 3.24)   0.007
#' am
#'   0            Ref                Ref
#'   1            0.05 (0, 1.14)     0.060


Table2 = function(model, data, catcols = NULL, esdigits = 2, output = NULL, 
          pdigits = 2, eps = 0.001) 
{
  for (p in c("dplyr", "rtf")) {
    if (!p %in% rownames(installed.packages())) 
      install.packages(p)
    library(p, character.only = T)
  }
  if('coxph' %in% class(model)) secol = 'se(coef)'
  else secol='Std. Error'
  out = round(data.frame(ES = exp(summary(model)$coefficients[, 1]), 
                         LCL = exp(summary(model)$coefficients[, 1] - 1.96 * summary(model)$coefficients[, secol]), 
                         UCL = exp(summary(model)$coefficients[, 1] + 1.96 * summary(model)$coefficients[, secol])), esdigits) %>% 
    mutate(Variable = rownames(.), ES_CI = paste0(ES, " (", LCL, ", ", UCL, ")"))
  p = summary(model)$coefficients[, 'Pr(>|z|)']
  out = out %>% right_join(data.frame(P_val = p) %>% mutate(Variable = rownames(.), 
                                                            P_val = format.pval(P_val, digits = pdigits, eps = eps)), 
                           "Variable") %>% select(Variable, ES_CI, P_val)
  for (x in catcols) {
    out = out %>% mutate(Variable = gsub(paste0("^",x), "", Variable)) %>% 
      add_row(Variable = x, ES_CI = "", P_val = "", .before = which(.$Variable == levels(data[[x]])[2])) %>% 
      add_row(Variable = levels(data[[x]])[1],  ES_CI = "Ref", P_val = "Ref", .before = which(.$Variable == levels(data[[x]])[2]))
  }
  if (is.null(output)) 
    return(out)
  else {
    rtffile = RTF(output)
    addTable(rtffile, out, col.justify = c("L", rep("C", 
                                                    2)), header.col.justify = "C")
    done(rtffile)
  }
}
