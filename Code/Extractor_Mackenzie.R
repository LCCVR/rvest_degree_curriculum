library(rvest)
library(dplyr)
library(stringr)

#Estrutura a lista de cursos
listaCursos <- tibble() %>% mutate(
  Instituicao = character()
  , Curso = character()
  , Nivel = character()
  , Modalidade =character()
  , Link = character()
)
#Estrutura de grades curriculares
gradeCurricular <- tibble() %>% mutate(
  Instituicao = character()
  , Curso = character()
  , Periodo = character()
  , Ordem = integer()
  , Materia = character()
)
instituicao <- "Mackenzie"
#Página de cursos de graduação
url <- "https://www.mackenzie.br/processos-seletivos/vestibular-graduacao/sao-paulo-higienopolis"
page <- read_html(url)
cursos <- page %>% html_node("#mc-Menus-id56581") %>% html_nodes("h3") %>% html_text()
niveis <- page %>% html_node("#mc-Menus-id56581") %>% html_nodes("p") %>% html_text() %>%
  gsub("[\r\n]","", .) %>% gsub(" ","", .) %>% gsub("Bacharelado","", .) %>%
  gsub("Licenciatura","", .) %>% gsub("(?<!^)(?=[A-Z])", " e ", ., perl=T)
modalidades <- page %>% html_node("#mc-Menus-id56581") %>% html_nodes("p") %>% html_nodes("span") %>% 
  html_text() %>% gsub("[\r\n[:space:]]+","", .) %>% 
  gsub("(?<!^)(?=[A-Z])", " e ", ., perl=T)
links <- page %>% html_node("#mc-Menus-id56581") %>% html_nodes("a") %>%
  html_attr("href")
listaCursos <- add_row(listaCursos,
                       Instituicao = rep(instituicao, length(cursos))
                       , Curso = cursos
                       , Nivel = niveis
                       , Modalidade = modalidades
                       , Link = links 
                       )
listaCursos <- filter(listaCursos, Curso != "Vestibular")

#Faz a extração da grade curricular de cada curso
listaFiltrada <- filter(listaCursos, Instituicao==instituicao, is.na(Link)==F)

for(i in 1:nrow(listaFiltrada)){
  if (listaFiltrada$Curso[i] == "Ciência da Computação"){
    #Código exclusivo para Ciência da Computação, que tem diferença da pág. padrão
    url <- paste("https://www.mackenzie.br", listaFiltrada$Link[i], "/matriz-curricular", sep="")
    page <- read_html(url)
    element <- html_element(page, ".mc-LpCurso-Main-Content") %>%
      html_element(".entry-content") %>% html_children()
    subid <- element[1] %>% html_children() %>% html_attr("id")
    tabs <- html_element(page, paste("#", subid, " > ul",sep="")) %>%
      html_children() %>% html_children() %>% html_attr("href")
    periodos <- html_element(page, paste("#", subid, " > ul",sep="")) %>%
      html_children() %>% html_text()
    for (j in 1:length(tabs)){
      periodo <- periodos[j] 
      materias <- html_element(page, paste(tabs[j]," > div > table > tbody", sep="")) %>% 
        html_elements("tr") %>% html_text() %>% gsub("\t","", .)
      for (k in 1:length(materias))
      {
        newRow <- list(Instituicao=instituicao,
                       Curso=listaFiltrada$Curso[i],
                       Periodo=periodo,
                       Ordem=k,
                       Materia=materias[k])
        gradeCurricular <- rbind(gradeCurricular, newRow )    
      }
    }
  }
  else if (str_detect(listaFiltrada$Curso[i],"Engenharia") == T | listaFiltrada$Curso[i]=="Química"){
    #Engenharias e Química: objeto materias é diferente
    url <- paste("https://www.mackenzie.br", listaFiltrada$Link[i], "/matriz-curricular", sep="")
    page <- read_html(url)
    id <- html_element(page, ".mc-LpCurso-Main-Content") %>%
      html_element(".entry-content") %>% html_children() %>%
      html_attr("id") %>% gsub("c", "", .) 
    tabs <- html_element(page, paste("#mc-tabs-content-",id[1],sep="")) %>%
      html_element(".tab-content") %>% html_children() %>% html_attr("id")
    for (j in 1:length(tabs)){
      periodo <- html_element(page, paste("a[href='#", tabs[j],"']", sep="")) %>% 
        html_text() 
      materias <- html_element(page, paste("#",tabs[j],sep="")) %>% 
        html_children() %>% html_children() %>% html_text() %>%
        gsub("\u00A0","", .)
      materias <- materias[nzchar(materias)]
      for (k in 1:length(materias))
      {
        newRow <- list(Instituicao=instituicao,
                       Curso=listaFiltrada$Curso[i],
                       Periodo=periodo,
                       Ordem=k,
                       Materia=materias[k])
        gradeCurricular <- rbind(gradeCurricular, newRow )    
      }
    }
  }
  else
  {
    url <- paste("https://www.mackenzie.br", listaFiltrada$Link[i], "/matriz-curricular", sep="")
    page <- read_html(url)
    id <- html_element(page, ".mc-LpCurso-Main-Content") %>%
      html_element(".entry-content") %>% html_children() %>%
      html_attr("id") %>% gsub("c", "", .) 
    tabs <- html_element(page, paste("#mc-tabs-content-",id[1],sep="")) %>%
      html_element(".tab-content") %>% html_children() %>% html_attr("id")
    for (j in 1:length(tabs)){
      periodo <- html_element(page, paste("a[href='#", tabs[j],"']", sep="")) %>% 
        html_text() 
      materias <- html_element(page, paste("#", tabs[j]," > div > table > tbody", sep="")) %>% 
        html_elements("tr") %>% html_text() 
      for (k in 1:length(materias))
      {
        materia <- html_element(page, paste("#", tabs[j]," > div > table > tbody > tr:nth-child(", k , ") > th:nth-child(1)", sep="")) %>% 
          html_text()
        newRow <- list(Instituicao=instituicao,
                       Curso=listaFiltrada$Curso[i],
                       Periodo=periodo,
                       Ordem=k,
                       Materia=materia)
        gradeCurricular <- rbind(gradeCurricular, newRow )    
      }
    }
  }
}

