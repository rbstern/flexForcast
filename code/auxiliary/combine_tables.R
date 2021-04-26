library(tidyverse)
library(fs)
library(kableExtra)

convert_to_mean_se <- function(dados_todos)
{
  # dados_todos new to contains "setting" and "statistic". 
  # Each value of "statistic" must be "mean" or "se"
  methods <- colnames(dados_todos) %>% 
    .[!. %in% c("setting","statistic")]
  
  dados_todos <- 
    dados_todos %>% 
    pivot_wider(names_from=statistic,
                values_from=methods[methods!="quantile"])
  tabela_final <- tibble(setting=dados_todos$setting)
  
  if("quantile"%in%methods)
    tabela_final$quantile <- dados_todos$quantile
  
  for(ii in methods[methods!="quantile"])
  {
    tabela_final$new <- apply(dados_todos,1,function(x)
    {
      return(paste0(round(as.numeric(x[paste0(ii,"_mean")]),decimal)," (",
                    round(as.numeric(x[paste0(ii,"_se")]),decimal),")"))
    })
    
    colnames(tabela_final)[ncol(tabela_final)] <- ii
  }
  
  return(tabela_final)
}

read_cde_loss <- function(arqs)
{
  dados_todos <- tibble(setting=arqs %>% 
                          path_file() %>% 
                          path_ext_remove() #%>% 
                        #gsub("\\_.*","",.)
  ) %>% 
    mutate(content=purrr::map(arqs, 
                              readRDS) %>% 
             lapply(function(xx){
               return(data.frame(statistic=cbind(c("mean","se")),
                                 rbind(xx$cdeloss$mean,xx$cdeloss$se)))
             })) %>% 
    unnest()
}


read_quantile_loss <- function(arqs)
{
  dados_todos <- tibble(setting=arqs %>% 
                          path_file() %>% 
                          path_ext_remove() #%>% 
                        #gsub("\\_.*","",.)
  ) %>% 
    mutate(content=purrr::map(arqs, 
                              readRDS) %>% 
             lapply(function(xx){
               return(data.frame(statistic=cbind(c("mean","se")),
                                 rbind(xx$pbloss$mean,xx$pbloss$se)))
             })) %>% 
    unnest()

}

all_tables <- function(arqs,n)
{
  arqs_subset <- arqs %>% 
    str_subset(.,as.character(n))
  data_cde <- arqs_subset %>% 
    read_cde_loss() %>% 
    convert_to_mean_se()
  data_quantile <- arqs_subset %>% 
    read_quantile_loss() %>% 
    convert_to_mean_se()
  
  
  
  kbl(data_quantile, booktabs = T,"latex", align = "c", linesep = '') %>%
    collapse_rows(1:2, row_group_label_position = 'stack')%>% 
    kable_styling(font_size = 7) %>% 
    print()
  
  kbl(data_cde, booktabs = T,"latex", align = "c", linesep = '') %>%
    #collapse_rows(1:2, row_group_label_position = 'stack')%>% 
    kable_styling(font_size = 7)%>% 
    print()
  
  
}

decimal <- 3

folder_files <- "../results/processed/"
arqs <- list.files(folder_files,full.names=TRUE)
all_tables(arqs,1000)
all_tables(arqs,5000)

