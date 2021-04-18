library(tidyverse)
library(fs)


convert_to_mean_se <- function(dados_todos,methods)
{
  # dados_todos new to contains "setting" and "statistic". 
  # Each value of "statistic" must be "mean" or "se"
  methods <- colnames(dados_todos) %>% 
    .[!. %in% c("setting","statistic")]
  
  dados_todos <- 
    dados_todos %>% 
    pivot_wider(names_from=statistic,
                values_from=methods)
  
  
  tabela_final <- tibble(setting=dados_todos$setting)
  for(ii in methods)
  {
    tabela_final$new <- apply(dados_todos,1,function(x)
    {
      return(paste0(round(as.numeric(x[paste0(ii,"_mean")]),decimal)," (+/- ",
                    round(as.numeric(x[paste0(ii,"_se")]),decimal),")"))
    })
    
    colnames(tabela_final)[ncol(tabela_final)] <- ii
  }
  
  return(tabela_final)
}




decimal <- 3

folder_files <- "../results/processed/"
arqs <- list.files(folder_files,full.names=TRUE)

n <- 1000
arqs_subset <- arqs %>% 
  str_subset(.,as.character(n))


dados_todos <- tibble(setting=arqs_subset %>% 
                        path_file() %>% 
                        path_ext_remove() %>% 
                        gsub("\\_.*","",.)) %>% 
  mutate(content=purrr::map(arqs_subset, 
                            readRDS) %>% 
           lapply(function(xx){
             return(data.frame(statistic=cbind(c("mean","se")),
                               rbind(xx$cdeloss$mean,xx$cdeloss$se)))
           })) %>% 
  unnest()

convert_to_mean_se(dados_todos)
