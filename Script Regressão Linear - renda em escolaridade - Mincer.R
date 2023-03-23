## Aqui são carregados os pacotes
{
  library('PNADcIBGE')
  library('dplyr')
  library('car')
  library('ggplot2')
  library('lmtest')
  library('sandwich')
  library('stargazer')
}

## Importando os dados (online) da PNAD do ano de 2018, segundo trimestre; define-se brutoPNADC20182t
#{
#  brutoPNADC20182t <- get_pnadc(year = 2018, quarter= 2)
#}

# Importando o Workspace com o brutoPNADC20182t já definido
load('/home/brunoldsc/Desktop/SSDG/Estudo/Trabalhos/Econometria I/2022/Dados/PNADc2018t02data.RData')

## Filtrando a base de dados para que contenha apenas as variáveis relevantes; define-se dr
{
  dr <- data.frame(educaçao = brutoPNADC20182t$variables$VD3005, 
                   categoria_educaçao = brutoPNADC20182t$variables$VD3004,
                   renda_efetiva_mensal = brutoPNADC20182t$variables$V4034,
                   renda_passada = brutoPNADC20182t$variables$V4033,
                   renda_habitual = brutoPNADC20182t$variables$VD4016,
                   renda_efetiva = brutoPNADC20182t$variables$VD4017,
                   idade = brutoPNADC20182t$variables$V2009,
                   cor = brutoPNADC20182t$variables$V2010,
                   sexo = brutoPNADC20182t$variables$V2007,
                   q_pessoas_dom = brutoPNADC20182t$variables$V2001,
                   P_experiencia = (brutoPNADC20182t$variables$V2009 - 
                                      as.numeric(brutoPNADC20182t$variables$VD3005)),
                   H_habit_trab = brutoPNADC20182t$variables$VD4031,
                   condição_trabalho = brutoPNADC20182t$variables$VD4001
                   )
}

## Transformando as variáveis para números e dummys
{
  dr$anos_educaçao <- as.numeric(dr$educaçao)
  dr$homem <- ifelse(dr$sexo == 'Homem', 1, 0)
  dr$trabalha <- ifelse(dr$condição_trabalho == 'Pessoas na força de trabalho', 1, 0)
  dr <- subset(dr, select = c(-educaçao, -sexo, -condição_trabalho,
                              -renda_efetiva_mensal, -renda_passada))
}

# Deixando na base de dados apenas as pessoas que estão trabalhando 
# (o que pode gerar viés devido a erro de medida - restrição populacional)
# A população de estudo é portanto de pessoas que trabalham e respondem a PNADc.
# termina-se a definição do dr (base de dados filtrada).
{
dr  <- filter(dr, trabalha==1)
}

# Aqui são vistos o número de nas na base de dados para variáveis específicas
{
  nNAsinH_habit_trab <- sum(is.na(dr$H_habit_trab))
  nNAsincor <- sum(is.na(dr$cor))
  nNAsinP_experiencia <- sum(is.na(dr$P_experiencia))
  nNAsinrenda_habitual <- sum(is.na(dr$renda_habitual))
  nNasinrenda_efetiva <- sum(is.na(dr$renda_efetiva))
}

# Criando uma base de dados sem os NAs nas variáveis de renda
{
  dr_renda_no_NA <- subset(dr, subset = is.na(dr$renda_efetiva) == FALSE)
  dr_renda_no_NA <- subset(dr, subset = is.na(dr$renda_habitual) == FALSE)
}

# Definindo a base final filtrada com nomes melhores
{
  BF <- data.frame(Anos_de_Educação = dr_renda_no_NA$anos_educaçao,
                   Categoria_de_Educação = dr_renda_no_NA$categoria_educaçao,
                   Renda_Efetiva_Mensal = dr_renda_no_NA$renda_efetiva,
                   Renda_Habitual_Mensal = dr_renda_no_NA$renda_habitual,
                   Idade = dr_renda_no_NA$idade,
                   Etnia = dr_renda_no_NA$cor,
                   Masculino = dr_renda_no_NA$homem,
                   Pessoas_no_Domicílio = as.factor(dr_renda_no_NA$q_pessoas_dom),
                   Proxy_de_Experiência = dr_renda_no_NA$P_experiencia,
                   Horas_no_trabalho = as.factor(dr_renda_no_NA$H_habit_trab)
  )
}

# Definindo uma base de dados em que aqueles com renda igual a zero são 
#retiradas da amostra
{
  BF_sane_persons <- subset(BF, subset = Renda_Efetiva_Mensal > 0)
}

# Removendo os objetos desnecessários
{
  rm('brutoPNADC20182t', 'dr', 'dr_renda_no_NA', 'nNAsincor',
       'nNAsinH_habit_trab', 'nNAsinP_experiencia', 'nNAsinrenda_habitual',
       'nNasinrenda_efetiva')
}

# Efetuando um gráfico dos dados brutos
{
  # Grafico dos dados brutos
  dataggplot <- ggplot(BF) +
    labs(title = 'Renda Efetiva Mensal e Anos de Educação', 
         x = 'Anos de Educação' , y = 'Renda em Reais de 2018',
         caption = 'Criado com dados de 2018 do IBGE
         Pesquisa Nacional por Amostra de Domicílios Contínua') +
    geom_point(aes(x=Anos_de_Educação, y=Renda_Efetiva_Mensal, color='Dados')) +
    scale_color_manual(values = c('#008000'), name = 'Legenda')
  
  dataggplot
  
  # Gráfico dos dados em ln
  datalnggplot <- ggplot(BF) +
    labs(title = 'ln(Renda Efetiva Mensal) e Anos de Educação', 
         x = 'Anos de Educação' , y = 'ln(Renda em Reais de 2018 + 1)',
         caption = 'Criado com dados de 2018 do IBGE
         Pesquisa Nacional por Amostra de Domicílios Contínua') +
    geom_point(aes(x=Anos_de_Educação, y=log(Renda_Efetiva_Mensal + 1),
                   color='Dados')) +
    scale_color_manual(values = c('#ff4f00'), name = 'Legenda')
  
  datalnggplot
  
}

# Definindo as fórmulas para serem feitas as regressões lineares
{
  # Regressão simples
  regsimples <- Renda_Efetiva_Mensal ~ Anos_de_Educação
  
  # Regressão simples com ln
  reglnsimples <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação
  
  # Minceriano comum
  regcomumminceriano <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação +
                                                    I(Proxy_de_Experiência^2)
  
  # Minceriano sem ser com termo quadrático (lectures Acemoglu)
  regminceriano <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação +
    Proxy_de_Experiência
  
  # Minceriano com os dois termos para teste de robustez
  regmincerianoteste <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação +
    Proxy_de_Experiência + I(Proxy_de_Experiência^2)
  
  # Minceriano com todos os controles
  regmincerfullcontrol <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação +
    Proxy_de_Experiência + I(Proxy_de_Experiência^2) + Masculino + Etnia +
    Horas_no_trabalho + Pessoas_no_Domicílio
  
  # Minceriano com interações
  regmincercontrolinterações <- log(Renda_Efetiva_Mensal + 1) ~ Anos_de_Educação +
    Proxy_de_Experiência + I(Proxy_de_Experiência^2) +
    I(Proxy_de_Experiência*Anos_de_Educação) + I(Masculino*Anos_de_Educação) +
    Etnia + Horas_no_trabalho + Pessoas_no_Domicílio
}

# Rodando as Regressões
{
  # Base BF (pessoas que indicaram renda e trabalham)
  lmregsimplesBF <- lm(formula = regsimples, data = BF)
  lmreglnsimplesBF <- lm(formula = reglnsimples, data = BF)
  lmregcomummincerianoBF <- lm(formula = regcomumminceriano, data = BF)
  lmregmincerianoBF <- lm(formula = regminceriano, data = BF)
  lmregmincerianotesteBF <- lm(formula = regmincerianoteste, data = BF)
  lmregmincerfullcontrolBF <- lm(formula = regmincerfullcontrol, data = BF)
  lmregmincercontrolinteraçõesBF <- lm(formula = regmincercontrolinterações, data = BF)
  
  # Base com cortes (pessoas que indicaram renda e ganham acima de 0)
  lmregsimplesBF_sane_persons <- lm(formula = regsimples, data = BF_sane_persons)
  lmreglnsimplesBF_sane_persons <- lm(formula = reglnsimples, data = BF_sane_persons)
  lmregcomummincerianoBF_sane_persons <- lm(formula = regcomumminceriano, data = BF_sane_persons)
  lmregmincerianoBF_sane_persons <- lm(formula = regminceriano, data = BF_sane_persons)
  lmregmincerianotesteBF_sane_persons <- lm(formula = regmincerianoteste, data = BF_sane_persons)
  lmregmincerfullcontrolBF_sane_persons <- lm(formula = regmincerfullcontrol, data = BF_sane_persons)
  lmregmincercontrolinteraçõesBF_sane_persons <- lm(formula = regmincercontrolinterações, data = BF_sane_persons)
   
}

# Efetuando os testes de significância, com estimadores robustos
#a heterocedasticidade, de White.
{
  # Coeftest da base sem corte
  regsimplesBFcoeftest <- coeftest(lmregsimplesBF,
                                   vcov=hccm(lmregsimplesBF, vcov=HC0))
  reglnsimplesBFcoeftest <- coeftest(lmreglnsimplesBF,
                                     vcov=hccm(lmreglnsimplesBF, vcov=HC0))
  regcomummincerianoBFcoeftest <- coeftest(lmregcomummincerianoBF,
                                           vcov=hccm(lmregcomummincerianoBF, vcov=HC0))
  regmincerianoBFcoeftest <- coeftest(lmregmincerianoBF,
                                   vcov=hccm(lmregmincerianoBF, vcov=HC0))
  regmincerianotesteBFcoeftest <- coeftest(lmregmincerianotesteBF,
                                      vcov=hccm(lmregmincerianotesteBF, vcov=HC0))
  regmincerfullcontrolBFcoeftest <- coeftest(lmregmincerfullcontrolBF,
                                      vcov=vcovHC(lmregmincerfullcontrolBF, type='HC0'))
  regmincercontrolinteraçõesBFcoeftest <- coeftest(lmregmincercontrolinteraçõesBF,
                                             vcov=vcovHC(lmregmincercontrolinteraçõesBF, type='HC0'))
  
  # Coeftest da base com corte
  regsimplesBF_sane_personscoeftest <- coeftest(lmregsimplesBF_sane_persons,
                                                vcov=hccm(lmregsimplesBF_sane_persons, vcov=HC0))
  reglnsimplesBF_sane_personscoeftest <- coeftest(lmreglnsimplesBF_sane_persons,
                                                  vcov=hccm(lmreglnsimplesBF_sane_persons, vcov=HC0))
  regcomummincerianoBF_sane_personscoeftest <- coeftest(lmregcomummincerianoBF_sane_persons,
                                                        vcov=hccm(lmregcomummincerianoBF_sane_persons, vcov=HC0))
  regmincerianoBF_sane_personscoeftest <- coeftest(lmregmincerianoBF_sane_persons,
                                                   vcov=hccm(lmregmincerianoBF_sane_persons, vcov=HC0))
  regmincerianotesteBF_sane_personscoeftest <- coeftest(lmregmincerianotesteBF_sane_persons,
                                                        vcov=hccm(lmregmincerianotesteBF_sane_persons, vcov=HC0))
  # os códigos abaixo não rodam, eles retornam
  # Error in hccm.lm(lmregmincerfullcontrolBF_sane_persons, vcov = HC0) : 
  #cannot proceed because of 2 cases with hatvalue = 1:
  #  102020, 199896)
  #regmincerfullcontrolBF_sane_personscoeftest <- coeftest(lmregmincerfullcontrolBF_sane_persons,
  #                                                        vcov=hccm(lmregmincerfullcontrolBF_sane_persons, vcov=HC0))
  #regmincercontrolinteraçõesBF_sane_personscoeftest <- coeftest(lmregmincercontrolinteraçõesBF_sane_persons,
  #                                                              vcov=hccm(lmregmincercontrolinteraçõesBF_sane_persons, vcov=HC0))
  
  regmincerfullcontrolBF_sane_personscoeftest <- coeftest(lmregmincerfullcontrolBF_sane_persons,
                                                          vcov=vcovHC(lmregmincerfullcontrolBF_sane_persons, type='HC0'))
  regmincercontrolinteraçõesBF_sane_personscoeftest <- coeftest(lmregmincercontrolinteraçõesBF_sane_persons,
                                                                vcov=vcovHC(lmregmincercontrolinteraçõesBF_sane_persons, type='HC0'))
}  


# Resultados das Regressões na base sem cortes
{
  regsimplesBFcoeftest
}
{
  reglnsimplesBFcoeftest
}
{
  regcomummincerianoBFcoeftest
}
{
  regmincerianoBFcoeftest
}
{
  regmincerianotesteBFcoeftest
}
{
  regmincerfullcontrolBFcoeftest
}
{
  regmincercontrolinteraçõesBFcoeftest
}
# Resultados das regressões na base com cortes
{
  regsimplesBF_sane_personscoeftest
}
{
  reglnsimplesBF_sane_personscoeftest
}
{
  regcomummincerianoBF_sane_personscoeftest
}
{
  regmincerianoBF_sane_personscoeftest
}
{
  regmincerianotesteBF_sane_personscoeftest
}
{
  regmincerfullcontrolBF_sane_personscoeftest
}
{
  regmincercontrolinteraçõesBF_sane_personscoeftest
}

# Fazendo um gráfico (histograma) dos resíduos
{
  regcomumincerianoresiduosBF <- data.frame(Resíduo = lmregcomummincerianoBF$residuals)
  histresmincercomumBF <- ggplot(regcomumincerianoresiduosBF, aes(x = Resíduo)) +                           
    geom_histogram(binwidth = 0.1) +
    labs(title = "Histograma dos Resíduos - Minceriano da Base sem Cortes",
         x = "Resíduo",
         y = "Frequência")
  
  regsimplesresiduosBF <- data.frame(Resíduo = lmregsimplesBF$residuals)
  histressimplesBF <- ggplot(regsimplesresiduosBF, aes(x = Resíduo)) +                           
    geom_histogram(binwidth = 100) +
    labs(title = "Histograma dos Resíduos - Modelo Simples da Base sem Cortes",
         x = "Resíduo",
         y = "Frequência")
  
  regcomumincerianoresiduosBF_sane_persons <- data.frame(Resíduo = lmregcomummincerianoBF_sane_persons$residuals)
  histresmincercomumBF_sane_persons <- ggplot(regcomumincerianoresiduosBF_sane_persons, aes(x = Resíduo)) +                           
    geom_histogram(binwidth = 0.01) +
    labs(title = "Histograma dos Resíduos - Minceriano da Base com Cortes",
         x = "Resíduo",
         y = "Frequência")

}

# Histogramas dos resíduos
histressimplesBF
histresmincercomumBF
histresmincercomumBF_sane_persons

# Definindo o mínimo e o máximo dos valores supostamente causai
{
minimumcausalsemcortes <- min(lmreglnsimplesBF$coefficients[2],
                              lmregcomummincerianoBF$coefficients[2], 
                              lmregmincerianoBF$coefficients[2],
                              lmregmincerianotesteBF$coefficients[2],
                              lmregmincerfullcontrolBF$coefficients[2])

minimumcausalsane <- min(lmreglnsimplesBF_sane_persons$coefficients[2],
                         lmregcomummincerianoBF_sane_persons$coefficients[2],
                         lmregmincerianoBF_sane_persons$coefficients[2],
                         lmregmincerianotesteBF_sane_persons$coefficients[2],
                         lmregmincerfullcontrolBF_sane_persons$coefficients[2],
                         lmregmincercontrolinteraçõesBF_sane_persons$coefficients[2])  
  
maximumcausalsemcortes <- max(lmreglnsimplesBF$coefficients[2],
                              lmregcomummincerianoBF$coefficients[2], 
                              lmregmincerianoBF$coefficients[2],
                              lmregmincerianotesteBF$coefficients[2],
                              lmregmincerfullcontrolBF$coefficients[2])
  
maximumcausalsane <- max(lmreglnsimplesBF_sane_persons$coefficients[2],
                         lmregcomummincerianoBF_sane_persons$coefficients[2],
                         lmregmincerianoBF_sane_persons$coefficients[2],
                         lmregmincerianotesteBF_sane_persons$coefficients[2],
                         lmregmincerfullcontrolBF_sane_persons$coefficients[2],
                         lmregmincercontrolinteraçõesBF_sane_persons$coefficients[2])
}

# Fazendo os gráficos com as retas das regressões simples
{
  # Grafico da regressão simples
  retaregsimplesggplotBF <- ggplot(BF) +
    labs(title = 'Renda Efetiva Mensal e Anos de Educação', 
         x = 'Anos de Educação' , y = 'Renda em Reais de 2018',
         caption = 'Criado com dados de 2018 do IBGE
         Pesquisa Nacional por Amostra de Domicílios Contínua') +
    geom_point(aes(x=Anos_de_Educação, y=Renda_Efetiva_Mensal, color='Dados')) +
    geom_abline(intercept = lmregsimplesBF$coefficients[1],
                slope = lmreglnsimplesBF$coefficients[2]) +
    scale_color_manual(values = c('#008000'), name = 'Legenda')
  
  retaregsimplesggplotBF
  
  # Gráfico da regressão em ln
  retareglnsimplesggplotBF <- ggplot(BF) +
    labs(title = 'ln(Renda Efetiva Mensal) e Anos de Educação', 
         x = 'Anos de Educação' , y = 'ln(Renda em Reais de 2018 + 1)',
         caption = 'Criado com dados de 2018 do IBGE
         Pesquisa Nacional por Amostra de Domicílios Contínua') +
    geom_point(aes(x=Anos_de_Educação, y=log(Renda_Efetiva_Mensal + 1),
                   color='Dados')) +
    geom_abline(intercept = lmreglnsimplesBF$coefficients[1],
                slope = maximumcausalsemcortes,
                color = 'red') +
    geom_abline(intercept = lmreglnsimplesBF_sane_persons$coefficients[1],
                slope = minimumcausalsane,
                color = 'blue') +
    scale_color_manual(values = c('#778899'), name = 'Legenda')
  
  retareglnsimplesggplotBF
}

# Fazendo a tabela 1 dos mincerianos sem controles
{
  SE_list_for_table1 <- list(c(reglnsimplesBFcoeftest[3:4]),
                             c(regmincerianoBFcoeftest[4:6]),
                             c(regmincerianotesteBFcoeftest[5:8]))
  
  p_list_for_table1 <- list(c(reglnsimplesBFcoeftest[7:8]),
                            c(regmincerianoBFcoeftest[10:12]),
                            c(regmincerianotesteBFcoeftest[13:16]))
  
  table1 <- stargazer(lmreglnsimplesBF, lmregmincerianoBF, lmregmincerianotesteBF,
                      type='latex', se = SE_list_for_table1, report = 'vcsp*',
                      omit.stat = c('f','ser'), p = p_list_for_table1)
  
}

# Código para salvar a tabela 1 num arquivo
{
  stargazer(lmreglnsimplesBF, lmregmincerianoBF, lmregmincerianotesteBF,
            type='latex', se = SE_list_for_table1, report = 'vcsp*',
            omit.stat = c('f','ser'), p = p_list_for_table1, title = 'Tabela das Regressões de Mincer - Base sem Corte',
            out = '/SSDG/Estudo/Trabalhos/Econometria I/2022/Artigo/tabela1.tex')
}

# Fazendo a tabela 2 dos mincerianos sem controles na base com corte
{
  a1 <- lmreglnsimplesBF_sane_persons
  a2 <- lmregmincerianoBF_sane_persons
  a3 <- lmregmincerianotesteBF_sane_persons
  
  SE_list_for_table2 <- list(c(reglnsimplesBF_sane_personscoeftest[3:4]),
                             c(regmincerianoBF_sane_personscoeftest[4:6]),
                             c(regmincerianotesteBF_sane_personscoeftest[5:8]))
  
  p_list_for_table2 <- list(c(reglnsimplesBF_sane_personscoeftest[7:8]),
                            c(regmincerianoBF_sane_personscoeftest[10:12]),
                            c(regmincerianotesteBF_sane_personscoeftest[13:16]))
  
  table2 <- stargazer(a1, a2, a3,
                      type='latex', se = SE_list_for_table2, report = 'vcsp*',
                      omit.stat = c('f','ser'), p = p_list_for_table2)
  
}
# Código para salvar a tabela 2 num arquivo
{
  stargazer(a1, a2, a3,
            type='latex', se = SE_list_for_table2, report = 'vcsp*',
            omit.stat = c('f','ser'), p = p_list_for_table2,
            title = 'Tabela das Regressões de Mincer - Base com Cortes',
            out = '/SSDG/Estudo/Trabalhos/Econometria I/2022/Artigo/tabela2.tex')
}

# Tabela 3, modelo de Mincer com vários controles, na base sem cortes
{
  SE_list_for_table3 <- list(c(regmincerfullcontrolBFcoeftest[141:280]))
  
  p_list_for_table3 <- list(c(regmincerfullcontrolBFcoeftest[421:560]))
  
  table3 <- stargazer(lmregmincerfullcontrolBF, type='latex', se = SE_list_for_table3,
                      report = 'vcsp*',omit.stat = c('f','ser'), p = p_list_for_table3)
  
}

# Código para salvar a tabela 3 num arquivo
{
  stargazer(lmregmincerfullcontrolBF,
            type='latex', se = SE_list_for_table3, report = 'vcsp*',
            omit.stat = c('f','ser'), p = p_list_for_table3,
            title = 'Tabela da Regressão com Controles - Base sem Cortes',
            out = '/SSDG/Estudo/Trabalhos/Econometria I/2022/Artigo/tabela3.tex')
}


# Alguns códigos para encontrar estatísticas básicas sobre a amostra.
summary(BF)
summary(BF_sane_persons)

sd(BF$Anos_de_Educação)
sd(BF$Renda_Habitual_Mensal)
sd(BF$Renda_Efetiva_Mensal)
sd(BF$Proxy_de_Experiência)
sd(BF$Idade)

sd(BF_sane_persons$Anos_de_Educação)
sd(BF_sane_persons$Renda_Habitual_Mensal)
sd(BF_sane_persons$Renda_Efetiva_Mensal)
sd(BF_sane_persons$Proxy_de_Experiência)
sd(BF_sane_persons$Idade)
