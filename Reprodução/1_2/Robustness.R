## ==========================================
## 
## Robustes
## 
## ==========================================

source('Fig_1_2.r')

pacman::p_load(pscl, broom, ggeffects, pROC)

# Using the R2 -----

get_r2_models <- function(dataset, year){
  type <- c("McFadden", "Maximum likelihood", "Cragg and Uhler", "AUC Roc Curve")
  
  fit_ideol <- glm(voto ~ ideol, data = dataset %>% 
                     mutate(voto = as.factor(voto)) %>%
                     drop_na(ideol),
                   family = binomial())
  
  auc_ideol <-  auc(roc(dataset %>% drop_na(ideol,voto) %>% pull(voto), 
                         predict(fit_ideol, type = "response"),quiet = T))
  
  temp1 <- data.frame(type = type,
                      r2 = c(pR2(fit_ideol)[4:6],auc_ideol[1]),
                      var = "Ideologia",
                      ano = year)
  
  fit_part <- glm(voto ~ part_esq, data = dataset %>% 
                    mutate(voto = as.factor(voto)) %>%
                    drop_na(part_esq),
                  family = binomial())
  
  auc_part <- auc(roc(dataset %>% drop_na(part_esq,voto) %>% pull(voto), 
                      predict(fit_part, type = "response"),quiet = T))
  
  temp2 <- data.frame(type = type,
                      r2 = c(pR2(fit_part)[4:6],auc_part[1]),
                      var = "Partido",
                      ano = year)
  
  fit_gov <- glm(voto ~ aval, data = dataset %>% 
                   mutate(voto = as.factor(voto)) %>%
                   drop_na(aval),
                 family = binomial())
  
  auc_gov <- auc(roc(dataset %>% drop_na(aval,voto) %>% pull(voto), 
                     predict(fit_gov, type = "response"),quiet = T))
  
  temp3 <- data.frame(type = type,
                      r2 = c(pR2(fit_gov)[4:6],auc_gov[1]),
                      var = "Avaliação",
                      ano = year)
  
  rbind(temp1, temp2, temp3)
}


datasets <- list(eseb_2002,eseb_2006,eseb_2010,
                 eseb_2014,eseb_2018,eseb_2022)

years <- c(2002, 2006, 2010, 2014, 2018, 2022)

for (i in 1:length(years)) {
  temp <- get_r2_models(datasets[[i]], years[i])
  
  if (years[i] == 2002) {
    r2_comp <- temp
  }
  else{
    r2_comp <- rbind(r2_comp, temp)
  }
  
  print(years[i])
}

r2_comp |>
  mutate(var = ifelse(var == "Partido", "Id. Partidária", var)) %>% 
  mutate(var = ifelse(var == "Avaliação", "Aval. Governo", var)) %>% 
  mutate(var = as.factor(var)) %>% 
  mutate(var = fct_relevel(var, c("Aval. Governo","Id. Partidária", "Ideologia"))) %>% 
  ggplot(aes(x = ano, y = r2, color = var)) +
  geom_point(position = position_dodge(width=2))+
  theme_light() +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 
                                2018, 2022)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 14)) + 
  facet_wrap(~type)+
  labs(x = "", y = '') +
  scale_color_manual(values = c("grey56", "black", "grey"))

r2_comp |>
  filter(type == "AUC Roc Curve") %>% 
  mutate(var = ifelse(var == "Partido", "Id. Partidária", var)) %>% 
  mutate(var = ifelse(var == "Avaliação", "Aval. Governo", var)) %>% 
  mutate(var = as.factor(var)) %>% 
  mutate(var = fct_relevel(var, c("Aval. Governo","Id. Partidária", "Ideologia"))) %>% 
  ggplot(aes(x = ano, y = r2, color = var)) +
  geom_point(position = position_dodge(width=2))+
  theme_light() +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 
                                2018, 2022)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 14)) + 
  geom_hline(yintercept = .5, linetype = 'dashed')+
  facet_wrap(~type)+
  labs(x = "", y = '') +
  scale_color_manual(values = c( "grey","grey56", "black"))

ggsave('Robustness.png', dpi = 300,units = 'px', width = 1600, height = 1000)


### Using logistic regression ------

std_log_models <- function(database, year) {
  
  print(unique(database$ano))
  print(class(database$ano))
  
  database <- database %>%
    mutate(
      voto = as.factor(voto),
      ideol_std = -1*((ideol-min(database$ideol, na.rm = T))/
                        (max(database$ideol, na.rm = T)-min(database$ideol, na.rm = T))-1),
      aval_std = ((aval-min(database$aval, na.rm = T))/
                    (max(database$aval, na.rm = T)-min(database$aval, na.rm = T))),
      part_std = -1*(part_esq-1)
    )
  
  print(summary(database$aval_std))
  
  if (unique(database$ano) %in% c(2002, 2018, 2022)) {
    database <- database %>%
      mutate(
        aval_std = -1*(aval_std-1)
      )
  }
  
  print(summary(database$aval_std))
  rio::export(x = database, file = paste0("eseb_",year,"_rec.xlsx"))
  
  fit <- glm(voto ~ ideol_std + aval_std + part_esq, data = database, 
             family = binomial())
  
  boxplot(database$aval_std)
  
  tidy(fit) %>% 
    filter(term != '(Intercept)') %>%
    mutate(ano = year,
           odd = exp(estimate),
           odd_up = exp(estimate + qnorm(.975) * std.error),
           odd_lo = exp(estimate - qnorm(.975) * std.error))
}

for (i in 1:length(years)) {
  temp <- std_log_models(datasets[[i]], years[i])
  
  if (years[i] == 2002) {
    models <- temp
  }
  else{
    models <- rbind(models, temp)
  }
}

models |>
  mutate(term = ifelse(term == "part_esq", "Id. Partidária", term)) %>% 
  mutate(term = ifelse(term == "aval_std", "Aval. Governo", term)) %>% 
  mutate(term = ifelse(term == "ideol_std", "Ideologia", term)) %>% 
  mutate(term = as.factor(term)) %>% 
  mutate(term = fct_relevel(term, c("Aval. Governo","Id. Partidária", "Ideologia"))) %>% 
  ggplot(aes(x = ano, y = odd, color = term)) +
  geom_point(position = position_dodge(width=2))+
  geom_errorbar(aes(ymin = odd_up, ymax = odd_lo),
                position = position_dodge(width=2))+
  theme_light() +
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 
                                2018, 2022)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 14)) + 
  labs(x = "", y = '') +
  scale_color_manual(values = c("grey56", "black", "grey"))


### Using logistic marginal ------

pred_log_models <- function(database, year) {
  database <- database %>%
    mutate(
      voto = as.factor(voto),
      ideol_std = (ideol-min(eseb_2002$ideol, na.rm = T))/
        (max(eseb_2002$ideol, na.rm = T)-min(eseb_2002$ideol, na.rm = T)),
      aval_std = (aval-min(eseb_2002$aval, na.rm = T))/
        (max(eseb_2002$aval, na.rm = T)-min(eseb_2002$aval, na.rm = T))
    )
  fit <- glm(voto ~ ideol_std + aval_std + part_esq, data = database, 
             family = binomial())
  
  # Se quiser usar essa estrutura Thiago
}