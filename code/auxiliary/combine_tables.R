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
               return(data.frame(statistic=c(rep("mean",nrow(xx$pbloss$mean)),
                                             rep("se",nrow(xx$pbloss$se))),
                                 rbind(xx$pbloss$mean,xx$pbloss$se)))
             })) %>% 
    unnest()
}

all_tables <- function(arqs,n,which_quantiles,which_settings,methods_remove)
{
  arqs_subset <- arqs %>% 
    str_subset(.,as.character(n))
  
  names_files <- basename(arqs_subset)
  
  if(!is.null(which_settings))
  {
    arqs_subset <- arqs_subset[names_files %>% 
                                 str_detect(.,paste(which_settings, collapse="|"))]
  }
  
  data_cde <- arqs_subset %>% 
    read_cde_loss() %>% 
    convert_to_mean_se()
  data_quantile <- arqs_subset %>% 
    read_quantile_loss() %>% 
    convert_to_mean_se() 
  
  if(!is.null(which_quantiles))
  {
    data_quantile <- data_quantile %>% 
      filter(quantile%in%which_quantiles)
  }
  
  if(!is.null(methods_remove))
  {
    data_quantile <- data_quantile %>% 
      select(!contains(methods_remove))
    
    data_cde <- data_cde %>% 
      select(!contains(methods_remove))
  }
  
  
  
  kbl(data_quantile, booktabs = T,"latex", align = "c", linesep = '') %>%
    collapse_rows(1:2, row_group_label_position = 'stack')%>% 
    kable_styling(font_size = 7) %>% 
    print()
  
  kbl(data_cde, booktabs = T,"latex", align = "c", linesep = '') %>%
    #collapse_rows(1:2, row_group_label_position = 'stack')%>% 
    kable_styling(font_size = 7)%>% 
    print()
  
  
}



all_plots <- function(arqs,which_quantiles,which_settings,methods_remove)
{
  arqs_subset <- arqs 
  
  names_files <- basename(arqs_subset)
  
  if(!is.null(which_settings))
  {
    arqs_subset <- arqs_subset[names_files %>% 
                                 str_detect(.,paste(which_settings, collapse="|"))]
  }
  
  data_cde <- arqs_subset %>% 
    read_cde_loss() %>% 
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d{4}"))) %>% 
    mutate(setting=str_replace_all(setting, "[:digit:]", ""))
  
  
  data_quantile <- arqs_subset %>% 
    read_quantile_loss()  %>% 
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d{4}")))%>% 
    mutate(setting=str_replace_all(setting, "[:digit:]", ""))
  
  if(!is.null(which_quantiles))
  {
    data_quantile <- data_quantile %>% 
      filter(quantile%in%which_quantiles)
  }
  
  if(!is.null(methods_remove))
  {
    data_quantile <- data_quantile %>% 
      select(!contains(methods_remove))
    
    data_cde <- data_cde %>% 
      select(!contains(methods_remove))
  }
  
  data_cde <- data_cde %>% 
    pivot_longer(NNKCDE:FLEX_RF,names_to = "method",
                 values_to = "CDE",names_repair = "unique") %>% 
    pivot_wider(names_from = statistic,values_from = CDE) %>% 
    mutate(quantile="CDE")
  
  data_quantile <- data_quantile %>% 
    pivot_longer(QAR:FLEX_RF,names_to = "method",
                 values_to = "Pinball",names_repair = "unique") %>% 
    pivot_wider(names_from = statistic,values_from = Pinball) %>% 
    mutate(quantile=as.character(quantile))
  
  data_all <- full_join(data_cde,data_quantile)
  
  ggplot(data_all)+
    geom_line(aes(x=n,y=mean,color=method))+
    facet_wrap(setting~quantile, scales="free",ncol = 4)
  
}

decimal <- 3
which_quantiles <- c(0.5,0.8,0.95)
which_settings<- c("^AR_1","^ARMA_11",
                   "^ARMAJUMP","^ARMATJUMP",
                   "^JUMPDIFFUSION","^SINE")
methods_remove <- "XGB"
folder_files <- "../results/processed/"
arqs <- list.files(folder_files,full.names=TRUE)



all_tables(arqs,1000,which_quantiles,which_settings,methods_remove)

all_tables(arqs,5000,which_quantiles,which_settings,methods_remove)

