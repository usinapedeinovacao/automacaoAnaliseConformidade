# PACOTES ------
# O primeiro foi o pacman, e dele foi utilizada apenas uma fun??o para deixar o c?digo menos extenso nesse momento. 
pacman::p_load( # A p_load tem como fun??o utilizar o comando library (que abre pacotes) para mais de um pacote, e caso algum n?o esteja instalado ela o instala e o abre.  
  pdftools, # Ler PDFs 
  tidyverse, # Comandos e filtragem e uso do %>% (pipe).
  textreadr, tm, tidytext, tidyr, stringr, dplyr, # Padroniza??o de texto.
  writexl) # Exportar um data frame como tabela xlsx.

# DADOS -----
# Criar uma lista de documentos
Documentos.Lista <- list.files(path = "../danilo_goncalves/automacao/Teste", # Selecionar a pasta
                               pattern = ".pdf$") # Selecionar o tipo de arquivo

Documentos.Lista <- as.data.frame(Documentos.Lista) # Transformar a lista em um data frame

Documentos.Lista$ID <- 1:nrow(Documentos.Lista) # Aqui se cria uma coluna que coloque o ID, sendo de 1 a o n?mero de linhas do data frame

# Abrir os pdfs

setwd("../danilo_goncalves/automacao/Teste")
# Selecionando uma pasta

## Aplicar essa fun??o para abrir todos de uma vez, mas eles ser?o abertos de forma de lista, precisando posteriormente serem transformados em um data frame

Documentos <- lapply(Documentos.Lista$Documentos.Lista, FUN = function(files) {
  # Ali est?o os "Documentos.Lista$Documentos.Lista" que ? a coluna com os nomes dos documentos igualando eles ao files da fun??o
  read_pdf(files, # Aqui est? sendo mandando ler os PDFs em "files" ou seja em "Documentos.Lista$Documentos.Lista"
           ocr = T) # Isso aqui por sua vez ? para que se leia sem problemas os acentos e ?
  
})

# Transformar o objeto de formato lista em formato data frame
Documentos <- bind_rows(Documentos, .id = "ID")

# Atribuindo o nome do documento a o ID dele
Documentos$Documento <- Documentos.Lista[Documentos$ID,1]

# Formata??o de texto
Documentos$text <- Documentos$text %>%
  str_to_lower() %>% # Jogar tudo pra min?sculo
  str_squish() %>% # Retirar espa?o
  iconv(from="UTF-8",to="ASCII//TRANSLIT") # Remover ? e acentua??es (? ^ ~)

# IDENTIFICANDO -----
# Agregando o texto
Docsss <- aggregate(text ~ Documento # Criar um novo objeto agregando a coluna text pela categoria na coluna documento
                    , data = Documentos, # Selecionando a base de dados
                    paste, collapse = " ") # Separando o texto agregado por espa?o

# Certid?o de regularidade fiscal

Docsss$OQue <-  ifelse( # Caso
  grepl("certidao de regularidade fiscal", Docsss$text) & # Esse & no lugar da v?gula ? crucial para que sejam identificadas mais de uma caracter?stica
    grepl("perante a fazenda publica estadual", Docsss$text), # Identificar trecho de texto
  "CertReguFiscal", # Se identificados os dois trechos ? uma "CertReguFiscal"
  NA) # Caso n?o, n?o preencher com nada

# Presta??o de contas - Cert Negativa

Docsss$OQue <- ifelse(grepl("https://efisco.sefaz.pe.gov.br/sfi_com_sca/prmontarmenuacesso", Docsss$text) & 
                        grepl("lei 7.741/78", Docsss$text) &
                        grepl("nao se encontra em atraso", Docsss$text) &
                        grepl("na entrega de prestacao de contas", Docsss$text) &
                        grepl("de transferencia por convenio junto ao governo do estado de pernambuco", Docsss$text),
                      "PC - CertNeg", 
                      Docsss$OQue) # A partir daqui se colocar o que j? existia antes na vari?vel caso n?o seja achado os itens que se pediu

## Presta??o de contas - N?o cadastrado

Docsss$OQue <- ifelse(grepl("nao apresentando", Docsss$text) & 
                        grepl("pendencias de prestacao de contas ate a presente data", Docsss$text)&
                        grepl("nao se encontra cadastrado", Docsss$text) &
                        grepl("sistema de execucao orcamentaria", Docsss$text),
                      "PC - NCadastr", 
                      Docsss$OQue)

## Presta??o de contas - Inadimplencia

Docsss$OQue <- ifelse(grepl("pendencias junto ao governo do estado de pernambuco", Docsss$text) &
                        grepl("artigo 207, da lei 7.741/78, e do artigo 5",Docsss$text) & 
                        grepl("incisos xxxiii e xxxiv", Docsss$text) &
                        grepl("alinea b, da constituicao federal de 1988", Docsss$text), 
                      "PC - Inadi", 
                      Docsss$OQue)

## Presta??o de contas - Positiva com efeito negativa

Docsss$OQue <- ifelse(grepl("certidao de regularidade de prestacao de contas", Docsss$text) &
                        grepl("positiva com efeito de negativa", Docsss$text) &
                        grepl("mesmo estando inadimplente", Docsss$text) &
                        grepl("suspensa a situacao de irregularidade", Docsss$text), 
                      "PC - PosiNeg", 
                      Docsss$OQue)

## CRF - Certid?o negativa

Docsss$OQue <- ifelse(grepl("nao sao suficientes", Docsss$text) &
                        grepl("impedimentos", Docsss$text),
                      "CRF - Negativa",
                      Docsss$OQue)

## CRF - Positiva

Docsss$OQue <- ifelse(grepl("caixa economica federal", Docsss$text) & 
                        grepl("certifica", Docsss$text) &
                        grepl("regular", Docsss$text) &
                        grepl("fundo de garantia do tempo de servico", Docsss$text) &
                        grepl("fgts", Docsss$text) &
                        grepl("www.caixa.gov.br", Docsss$text),
                      "CRF - Positiva",
                      Docsss$OQue)

## Comprovante de situa??o cadastral no CPF

Docsss$OQue <- ifelse(grepl("este documento nao substitui o", Docsss$text) &
                        grepl("comprovante de situacao cadastral no cpf", Docsss$text) &
                        grepl("https://servicos.receita.fazenda.gov.br/servicos/cpf/consultasituacao", Docsss$text),
                      "CompSituCPF",
                      Docsss$OQue)

## Certid?o negativa de d?bitos trabalhistas

Docsss$OQue <- ifelse(grepl("certifica-se", Docsss$text) &
                        grepl("http://www.tst.jus.br", Docsss$text) &
                        grepl("autenticidade no portal do tribunal superior do trabalho", Docsss$text) &
                        grepl("art. 642-a da consolidacao das leis do trabalho", Docsss$text) &
                        grepl("acrescentado pela lei no 12.440", Docsss$text) &
                        grepl("de 7 de julho de 2011", Docsss$text) &
                        grepl("resolucao administrativa no 1470/2011", Docsss$text) &
                        grepl("poder judiciario", Docsss$text) &
                        grepl("justica do trabalho", Docsss$text),
                      "Cert. Negativa de Deb. Trabalhistas",
                      Docsss$OQue)

## Certid?o positiva com efeito de negativa de d?bitos trabalhistas
Docsss$OQue <-  ifelse(grepl("a certidao positiva de debitos trabalhistas, com os mesmos efeitos", Docsss$text) &
                         grepl("da negativa", Docsss$text) &
                         grepl("com efeito de negativa", Docsss$text),
                       "Cert. Posi. com efeito de Negativa de Deb. Trabalhistas",
                       Docsss$OQue)

## Certid?o de Regularidade Fiscal perante a Fazenda Federal relativa a tributos e ? Seguridade Social

Docsss$OQue <-  ifelse(grepl("certidao negativa de debitos", Docsss$text) &
                         grepl("relativos aos tributos federais", Docsss$text) &
                         grepl("ativa da uniao", Docsss$text) &
                         grepl("nao constam pendencias em seu nome", Docsss$text) &
                         grepl("fazenda nacional", Docsss$text),
                       "Cert. Regul. Fazenda Federal",
                       Docsss$OQue)

## Positiva com efeito de negativa Certid?o de Regularidade Fiscal perante a Fazenda Federal relativa a tributos e ? Seguridade Social
Docsss$OQue <-  ifelse(grepl("certidao positiva com efeitos de negativa", Docsss$text) &
                         grepl("federais e a divida ativa da uniao", Docsss$text) &
                         grepl("constam debitos administrados", Docsss$text) &
                         grepl("conforme disposto nos arts. 205 e 206 do ctn, este documento tem os mesmos efeitos", Docsss$text),
                       "Cert. Regul. Fazenda Federal - PosiNeg",
                       Docsss$OQue)
# Tirando o texto grosso pra facilitar
Docsss2 <- Docsss[-2] # Aqui j? se tem o que cada documento ?

# EXTRAINDO INFORMA??O
# Juntando os data frames
Documentos <- left_join(Documentos, Docsss2, by = "Documento")

# CNPJ 

Documentos$CNPJ <- ifelse(grepl("\\d{2}.\\d{3}.\\d{3}/\\d{4}-\\d{2}", Documentos$text), # Se identificar um padr?o num?rico
                          as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}.\\d{3}.\\d{3}/\\d{4}-\\d{2}", simplify = TRUE)), # Extrair o padr?o
                          NA)

##CPF

Documentos$CPF <- ifelse(grepl("\\d{3}.\\d{3}.\\d{3}-\\d{2}", Documentos$text),
                         as.character(gsubfn::strapplyc(Documentos$text, "\\d{3}.\\d{3}.\\d{3}-\\d{2}", simplify = TRUE)),
                         NA)

## Inscri??o

Documentos$Inscri <- ifelse(grepl("\\d+.\\d+.\\d+/\\d+-\\d+", Documentos$text),
                            as.character(gsubfn::strapplyc(Documentos$text, "\\d+.\\d+.\\d+/\\d+-\\d+", simplify = TRUE)),
                            NA)

## Raz?o social

Documentos$RS <- ifelse(grepl("razao social:", Documentos$text), 
                        gsub(".*:", "", Documentos$text), 
                        NA)

## Nome

Documentos$Nome <- ifelse(grepl("nome: ", Documentos$text), 
                          gsub(".*:", "", Documentos$text), 
                          NA)



## Processo

Documentos$Processo <- ifelse(grepl("\\d{7}-\\d{2}.\\d{4}.\\d{1}.\\d{2}.\\d{4}", Documentos$text),
                              as.character(gsubfn::strapplyc(Documentos$text, "\\d{7}-\\d{2}.\\d{4}.\\d{1}.\\d{2}.\\d{4}", simplify = TRUE)),
                              NA)

## Situa??o Cadastral
Documentos$Situcad <- ifelse(grepl("situacao cadastral: ", Documentos$text), 
                             gsub(".*: ", "", Documentos$text), 
                             NA)

## Data de nascimento
Documentos$NASCI <- ifelse(grepl("data de nascimento", Documentos$text), 
                           as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)), 
                           NA)

## Data de inscrti??o
Documentos$DATINSCRI <- ifelse(grepl("data da inscricao", Documentos$text), 
                               gsub(".*: ", "", Documentos$text), 
                               NA) %>%
  str_squish()

## Situa??o

Documentos$SITU <- ifelse(grepl("esta em situacao", Documentos$text), # Se identificar esse texto
                          gsub(".*esta em situacao ", "", Documentos$text), # Copirar a linha dele e subistituir ele e tudo que est? antes por nada 
                          NA)

Documentos$SITU <- gsub(" perante.*", "", Documentos$SITU) # Substituir tudo que est? depois de perante por nada

# N?mero da certid?o
Documentos$NCERT <- ifelse(grepl("CertReguFiscal", Documentos$OQue) &
                             grepl("numero da certidao:", Documentos$text), # Se identificar esse texto
                           as.character(gsubfn::strapplyc(Documentos$text, "\\d{4}.\\d+-\\d{2}", simplify = TRUE)), # Extrair esse padr?o num?rico
                           NA)

Documentos$NCERT <- ifelse(grepl("CRF", Documentos$OQue) & # Se for qualquer tipo de CRF
                             grepl("certificacao numero: ", Documentos$text), # Usar esse outro padr?o
                           gsub(".*: ", "", Documentos$text), # Retirar o texto que vem antes
                           Documentos$NCERT) # Repetir o que j? se tinha

Documentos$NCERT <- ifelse(grepl(c("Cert. Negativa de Deb. Trabalhistas|Cert. Posi. com efeito de Negativa de Deb. Trabalhistas"), 
                                 # Aqui se usa esse c() com a | separando o tipo de documento porque ?s vezes documentos muito diferentes podem ter padr?es iguais, ai nesse caso se pede pra identificar esse ou aquele documento com o grelp, mas como ambas s?o de d?bitos trabalhistas era poss?vel usar por exemplo apenas o grelp e "Deb. Trabalhistas", como foi feito com as CRFs que tem mais de um tipo, mas todos os nomes das categorias tem CRF
                                 Documentos$OQue) &
                             grepl("certidao no: ", Documentos$text),
                           as.character(gsubfn::strapplyc(Documentos$text, "\\d+/\\d{4}", simplify = TRUE)),
                           Documentos$NCERT)

## Emiss?o

Documentos$EMI <- ifelse(grepl("CertReguFiscal", Documentos$OQue) &
                           grepl("data de emissao:", Documentos$text),
                         as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)),
                         NA)

Documentos$EMI <- ifelse(grepl("CRF", Documentos$OQue) &
                           grepl(c("validade:\\d{2}/\\d{2}/\\d{4} a |validade: \\d{2}/\\d{2}/\\d{4} a "), Documentos$text), 
                         gsub(" a \\d{2}/\\d{2}/\\d{4}.*", "", Documentos$text),
                         Documentos$EMI)
# Ficou a "validade:", tem que tirar
Documentos$EMI <- gsub(".*: ", "", Documentos$EMI) # tirando tudo que vem antes de ": "
Documentos$EMI <- gsub(".*:", "", Documentos$EMI) # tirando tudo que vem antes de ":"

Documentos$EMI <- ifelse(grepl("Deb. Trabalhistas", Documentos$OQue) &
                           grepl("expedicao: ", Documentos$text), 
                         as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)),
                         Documentos$EMI)

Documentos$EMI <- ifelse(grepl("CompSituCPF", Documentos$OQue) &
                           grepl("comprovante emitido as", Documentos$text), 
                         as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)), 
                         Documentos$EMI)

Documentos$EMI <- ifelse(grepl("PC - ", Documentos$OQue) &
                           grepl("emitida as ", Documentos$text), 
                         as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)), 
                         Documentos$EMI)

## Validade

Documentos$Vali <- ifelse(grepl(c("Regul. Fazenda Federal|PC - |CertReguFiscal"), Documentos$OQue) &
                            grepl("valida ate", Documentos$text), 
                          as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)), 
                          NA)

Documentos$Vali <- ifelse(grepl("Deb. Trabalhistas", Documentos$OQue) &
                            grepl("validade: ", Documentos$text), 
                          as.character(gsubfn::strapplyc(Documentos$text, "\\d{2}/\\d{2}/\\d{4}", simplify = TRUE)), 
                          Documentos$Vali) 

Documentos$Vali <- ifelse(grepl("CRF", Documentos$OQue) &
                            grepl(c("validade:\\d{2}/\\d{2}/\\d{4} a |validade: \\d{2}/\\d{2}/\\d{4} a "), Documentos$text), 
                          gsub(".*\\d{2}/\\d{2}/\\d{4} a ", "", Documentos$text),
                          Documentos$Vali)

# C?digo
Documentos$Cod <- ifelse(grepl("codigo ", Documentos$text), 
                         gsub(".*codigo ", "", Documentos$text), 
                         NA)

Documentos$Cod <- gsub(".*: ", "", Documentos$Cod) # retirar tudo que vem antes de :
Documentos$Cod <- ifelse(grepl("tributario nacional.*", Documentos$Cod), NA, Documentos$Cod) # retirar tudo que vem depois

Documentos$Cod <- Documentos$Cod %>%
  str_squish() # Remover esp?os maiores que " "

## C?digo de autenticidade

Documentos$Acod <- ifelse(!grepl("CRF", Documentos$OQue) &
                            grepl("\\d{3}.\\d{4}.\\d{4}", Documentos$text),
                          as.character(gsubfn::strapplyc(Documentos$text, "\\d{3}.\\d{4}.\\d{4}", simplify = TRUE)),
                          NA)

# Desenvolvendo a planilha final ------
## Juntando tudo (Separando)

TabelaFinal <- as.data.frame(cbind(Documentos$Documento, Documentos$OQue, 
                                   Documentos$CNPJ, Documentos$CPF, 
                                   Documentos$RS, Documentos$Nome,
                                   Documentos$Acod, Documentos$Cod,
                                   Documentos$NASCI, Documentos$DATINSCRI,
                                   Documentos$Situcad, Documentos$Processo, 
                                   Documentos$EMI, Documentos$Vali, 
                                   Documentos$SITU, Documentos$NCERT))

## Renomeando

TabelaFinal <- TabelaFinal %>%
  rename(Documento = V1,
         TIPO = V2,
         CNPJ = V3,
         CPF = V4,
         RAZAOSOCIAL = V5,
         NOME = V6,
         ACOD = V7,
         COD = V8,
         NASCI = V9,
         DATINSCRI = V10,
         SITUCAD = V11,
         PROCESSO = V12,
         EMI = V13,
         VALI = V14,
         SITU = V15,
         NCERT = V16)


## Agregando


TabelaFinal <- TabelaFinal %>% group_by(Documento) %>% # Dterminar grupos
  fill(TIPO, CNPJ, CPF, RAZAOSOCIAL, NOME, ACOD, COD, NASCI, DATINSCRI, SITUCAD, PROCESSO, EMI, VALI, SITU, NCERT, # Completar de acordoco com
       .direction = "downup") %>%  # Dire??o na qual preencher valores faltantes, "downup" - primeiro para baixo e depois para cima
  # Basicamente o fill serve para deixar tudo igual, preenchendo de acordo com os grupos mostrados na V1
  distinct()


## No fim temos

TabelaFinal

# SALVANDO EM XLSX ------
setwd("C:/Users/Danilo/Desktop/Coisas Relevantes/Trabalhos em andamento/Usina/Dados/Documentos/Teste") # Selecionando uma pasta
writexl::write_xlsx(TabelaFinal, "TabelaFinal.xlsx")
