library(tidyverse)
library(fs)
library(kableExtra)
library(cowplot)
library(grid) 
library(gtable)
library(gridExtra)
library(grid)

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
  
  return(ggplot(data_all)+
           geom_line(aes(x=n,y=mean,color=method))+
           facet_wrap(setting~quantile, scales="free",ncol = 4)+
           theme_bw()+
           scale_color_brewer(name = "Method",palette="Dark2"))
  
}

plots_paper <- function(arqs,which_quantiles,which_settings,methods_remove)
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
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d+"))) %>% 
    #mutate(setting=str_replace_all(setting, "[:digit:]", ""))
    mutate(setting=str_replace_all(setting, "\\_.*", ""))
  
  
  data_quantile <- arqs_subset %>% 
    read_quantile_loss()  %>% 
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d+")))%>% 
    #mutate(setting=str_replace_all(setting, "[:digit:]", ""))
    mutate(setting=str_replace_all(setting, "\\_.*", ""))
  
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
  
  data_all$setting <- recode(data_all$setting, 
                             SINE_AR_obs = "NONLINEAR MEAN", 
                             AR__obs = "AR",
                             AR_NONLINEAR_VAR_obs = "NONLINEAR VARIANCE",
                             ARMA__obs="ARMA",
                             ARMAJUMP_obs="ARMA JUMP",
                             ARMATJUMP_obs="ARMA JUMP T",
                             JUMPDIFFUSION_obs="JUMP DIFFUSION")
  
  data_all$quantile <- recode(data_all$quantile,
                              "0.5"="Pinball loss (50%)",
                              "0.8"="Pinball loss (80%)",
                              "0.95"="Pinball loss (95%)",
                              "CDE"="CDE loss")
  
  settings <- unique(data_all$setting)
  plots <- list()
  grobs <- list()
  for(this_setting in seq_along(settings))
  {
    plots[[this_setting]] <- ggplot(data_all %>% 
                                      filter(setting==settings[this_setting]))+
      geom_line(aes(x=n,y=mean,color=method))+
      facet_wrap(~quantile, scales="free",ncol = 4)+
      theme_bw(base_size = 12)+
      scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
      #scale_color_brewer(name = "",palette="Dark2")+ 
      theme(legend.position="none",axis.text.x = element_text(size=6))+
      scale_x_continuous(breaks=unique(data_all$n))+
      xlab("Time Series Length")+
      ylab("Loss Function")
    
    grobs[[this_setting]] <- ggplotGrob(plots[[this_setting]]) # Generate a ggplot2 plot grob
    grobs[[this_setting]] <- gtable_add_rows(grobs[[this_setting]], 
                                             unit(0.6, 'cm'), 2) # add new rows in specified position
    
    grobs[[this_setting]] <- gtable_add_grob(grobs[[this_setting]],
                                             list(rectGrob(gp = gpar(col = NA, fill = gray(0.7))),
                                                  textGrob(settings[this_setting], gp = gpar(col = "black",cex=0.9))),
                                             t=2, l=4, b=3, r=18, name = paste(runif(2))) #add grobs into the table
    
  }
  legend <- get_legend(
    # create some space to the left of the legend
    plots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 6)) +
      theme(legend.position = "top")
  )
  
  top_row <- plot_grid(
    legend,
    nrow = 1
  )
  
  bottom_row <- plot_grid(
    plotlist=grobs,ncol=2
  )
  
  plot_grid(top_row,bottom_row,ncol=1,rel_heights = c(0.2,4))
  
  ggsave("../figures/loss_values.png",height = 7,width = 14)
  
}

plots_paper_individual <- function(arqs,which_quantiles,which_settings,methods_remove,se=TRUE)
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
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d+"))) %>% 
    #mutate(setting=str_replace_all(setting, "[:digit:]", ""))
    mutate(setting=str_replace_all(setting, "\\_.*", ""))
  
  
  data_quantile <- arqs_subset %>% 
    read_quantile_loss()  %>% 
    mutate(n=as.numeric(stringi::stri_extract_last_regex(setting, "\\d+")))%>% 
    #mutate(setting=str_replace_all(setting, "[:digit:]", ""))
    mutate(setting=str_replace_all(setting, "\\_.*", ""))
  
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
  
  data_cde$setting <- recode(data_cde$setting, 
                             SINElagged = "NONLINEAR MEAN", 
                             AR__obs = "AR",
                             ARNONLINEARVARlagged = "NONLINEAR VARIANCE",
                             ARMA__obs="ARMA",
                             ARMAJUMP3="ARMA JUMP",
                             ARMATJUMP3="ARMA JUMP T",
                             JUMPDIFFUSION="JUMP DIFFUSION")
  
  data_quantile$setting <- recode(data_quantile$setting, 
                                  SINElagged = "NONLINEAR MEAN", 
                                  AR__obs = "AR",
                                  ARNONLINEARVARlagged = "NONLINEAR VARIANCE",
                                  ARMA__obs="ARMA",
                                  ARMAJUMP3="ARMA JUMP",
                                  ARMATJUMP3="ARMA JUMP T",
                                  JUMPDIFFUSION="JUMP DIFFUSION")
  
  data_quantile$method <- recode(data_quantile$method, 
                                 FLEX_RF = "FLEXCODE")
  
  data_cde$method <- recode(data_cde$method, 
                            FLEX_RF = "FLEXCODE")
  
  data_quantile$quantile <- recode(data_quantile$quantile,
                                   "0.5"="50%",
                                   "0.8"="80%",
                                   "0.95"="95%",
                                   "CDE"="CDE loss")
  
  
  
  settings <- unique(data_quantile$setting)
  plots <- list()
  grobs <- list()
  ii <- 1
  for(this_setting in seq_along(settings))
  {
    data_sub <- data_quantile %>% 
      filter(setting==settings[this_setting])
    data_sub$title <- settings[this_setting]
    
    if(se)
    {
      plots[[this_setting]] <- ggplot(data_sub,aes(x=n,y=mean,
                                                   ymin=mean-se,ymax=mean+se,fill=method))+
        geom_line(aes(color=method),size=1.2)+
        geom_ribbon(alpha=0.2)+
        facet_wrap(.~quantile, scales="free",ncol = 3)+
        theme_bw(base_size = 12)+
        #facet_grid(. ~ title)+
        scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        scale_fill_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        #scale_color_brewer(name = "",palette="Dark2")+ 
        theme(legend.position="none",
              axis.text.x = element_text(size=6),
              plot.title = element_text(hjust = 0.5))+
        scale_x_continuous(breaks=unique(data_quantile$n))+
        #ggtitle(settings[this_setting])+
        xlab("Time Series Length")+
        ylab("Quantile Loss")
    } else {
      plots[[this_setting]] <- ggplot(data_sub)+
        geom_line(aes(x=n,y=mean,color=method),size=1.2)+
        facet_wrap(.~quantile, scales="free",ncol = 3)+
        theme_bw(base_size = 12)+
        #facet_grid(. ~ title)+
        scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        #scale_color_brewer(name = "",palette="Dark2")+ 
        theme(legend.position="none",
              axis.text.x = element_text(size=6),
              plot.title = element_text(hjust = 0.5))+
        scale_x_continuous(breaks=unique(data_quantile$n))+
        #ggtitle(settings[this_setting])+
        xlab("Time Series Length")+
        ylab("Quantile Loss")
    }
    
    title <- grobTree(rectGrob(gp=gpar(fill="#E2F5DD",col="#E2F5DD")),
                      textGrob(data_sub$title, x=0.5, hjust=0.5,y=0.5,
                               gp=gpar(col="black", cex=1,fontface="bold")))
    
    grobs[[ii]] <-  title
    grobs[[ii+1]] <-  plots[[this_setting]]
    grobs[[ii+2]] <-rectGrob(gp=gpar(col=NA))
    
    #ii <- ii+2
    ii <- ii+3
  }
  
  legend <- get_legend(
    # create some space to the left of the legend
    plots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 6)) +
      theme(legend.position = "top",legend.title=element_text(size=25))
  )
  
  
  g <- grid.arrange(grobs=c(list(legend),grobs),
                    ncol=1,heights=c(2,rep(c(0.6,9,0.6),
                                           length.out=length(grobs))))
  
  ggsave("../figures/loss_values_quantile.png",g,
         height = 14,width = 7)
  
  

  plots <- list()
  grobs <- list()
  ii <- 1
  for(this_setting in seq_along(settings))
  {
    data_sub <- data_cde %>% 
      filter(setting==settings[this_setting])
    data_sub$title <- settings[this_setting]
    
    if(se)
    {
      
      plots[[this_setting]] <-    ggplot(data_sub,aes(x=n,y=mean,fill=method))+
        geom_line(aes(color=method),size=1.2)+
        geom_ribbon(aes(ymin=mean-se,ymax=mean+se),alpha=0.2)+
        #facet_wrap(.~quantile, scales="free",ncol = 3)+
        theme_bw(base_size = 12)+
        #facet_grid(. ~ title)+
        scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        scale_fill_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        #scale_color_brewer(name = "",palette="Dark2")+ 
        theme(legend.position="none",
              axis.text.x = element_text(size=6),
              plot.title = element_text(hjust = 0.5))+
        scale_x_continuous(breaks=unique(data_quantile$n))+
        #ggtitle(settings[this_setting])+
        xlab("Time Series Length")+
        ylab("CDE Loss")
      
      
    } else {
      
      plots[[this_setting]] <- ggplot(data_sub)+
        geom_line(aes(x=n,y=mean,color=method),size=1.2)+
        #facet_wrap(.~quantile, scales="free",ncol = 3)+
        theme_bw(base_size = 12)+
        #facet_grid(. ~ title)+
        scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
        #scale_color_brewer(name = "",palette="Dark2")+ 
        theme(legend.position="none",
              axis.text.x = element_text(size=6),
              plot.title = element_text(hjust = 0.5))+
        scale_x_continuous(breaks=unique(data_quantile$n))+
        #ggtitle(settings[this_setting])+
        xlab("Time Series Length")+
        ylab("CDE Loss")
      
    }
    
    title <- grobTree(rectGrob(gp=gpar(fill="#E2F5DD",col="#E2F5DD")),
                      textGrob(data_sub$title, x=0.5, hjust=0.5,y=0.5,
                               gp=gpar(col="black", cex=1,fontface="bold")))
    
    grobs[[ii]] <-  title
    grobs[[ii+1]] <-  plots[[this_setting]]
    grobs[[ii+2]] <-rectGrob(gp=gpar(col=NA))
    
    #ii <- ii+2
    ii <- ii+3
  }
  
  legend <- get_legend(
    # create some space to the left of the legend
    plots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 6)) +
      theme(legend.position = "top",legend.title=element_text(size=25))
  )
  
  
  #g <- grid.arrange(grobs=c(list(legend),grobs),
  #                  ncol=2,heights=c(2,rep(c(0.6,9,0.6),
  #                                         length.out=length(grobs))))
  
  
  
  lay <- cbind(c(1,1,2,3,3,3,3,3,3,3,4,8,9,9,9,9,9,9,9,10,14,15,15,15,15,15,15,15,16),
               c(1,1,5,6,6,6,6,6,6,6,7,11,12,12,12,12,12,12,12,13,17,18,18,18,18,18,18,18,19))
  g <- grid.arrange(grobs=c(list(legend),grobs),
                    layout_matrix=lay)
  
  
  ggsave("../figures/loss_values_cde.png",g,
         height = 14,width = 7)
  
  
  
}


decimal <- 3
which_quantiles <- c(0.5,0.8,0.95)
which_settings<- c("^AR_3","^ARNONLINEAR",
                   "^ARMAJUMP","^ARMATJUMP",
                   "^JUMPDIFFUSION",
                   "^SINE")
methods_remove <- NULL
folder_files <- "../results/processed/"
arqs <- list.files(folder_files,full.names=TRUE)

plots_paper(arqs,which_quantiles,which_settings,methods_remove)
plots_paper_individual(arqs,which_quantiles,which_settings,methods_remove,se = TRUE)

all_tables(arqs,1000,which_quantiles,which_settings,methods_remove)
