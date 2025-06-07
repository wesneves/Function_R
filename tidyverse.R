# Funções Tidyverse ----

## unite() ----
## Unir colunas, col = nome da coluna que vai receber as colunas unidas, sep = caractere separador das colunas unidas, remove = resposta lógica informando se as colunas são unidas ou não. 

pinguins_unir <- tidyr::unite(data = penguins_raw,
                              col = "region_island",
                              Region:Island,
                              sep = ", ",
                              remove = FALSE)

head(pinguins_unir[, c("Region", "Island", "region_island")])


## separate() ---- 
## Separar elementos de uma coluna em mais colunas. tidyr::separate() separa uma coluna em novas colunas, tidyr::separate_rows() separa uma coluna distribuindo os elementos nas linhas. col =  coluna a ser separada, into = nomes das novas colunas, sep = caractere separador das colunas, remove = resposta lógica informando se as colunas são unidas ou não.

pinguins_separar <- tidyr::separate(data = penguins_raw,
                                        col = Stage,
                                        into = c("stage", "egg_stage"),
                                        sep = ", ",
                                        remove = FALSE)

head(pinguins_separar[, c("Stage", "stage", "egg_stage")])

## Separar em novas linhas
pinguins_separar_linhas <- tidyr::separate_rows(data = penguins_raw,
                                                Stage,
                                                sep = ", ")

head(pinguins_separar_linhas[, c("studyName", "Sample Number","Species",
                                 "Region", "Island","Stage")])

## drop_na() e replace_na() ----
## Retirar e substituir valores ausentes

## Remover todas as linhas com NAs
pinguins_raw_na <- tidyr::drop_na(data = penguins_raw)

head(pinguins_raw_na)

## Remover linhas de colunas específicas com NAs
pinguins_raw_colunas_na <- tidyr::drop_na(data = penguins_raw,
                                          any_of("Comments"))

head(pinguins_raw_colunas_na[, "Comments"])

## Substituir NAs por outro valor
pinguins_raw_subs_na <- tidyr::replace_na(data = penguins_raw,
                                          list(Comments = "Unknown"))

head(pinguins_raw_subs_na[, "Comments"])

## pivot_longer() e pivot_large() ----
## Funções semelhantes a tabela dinâmica no excell, na qual é possível mudar o formato da tabela para longo ou largo. Consiste em usar uma coluna para distribuir seus calores em outras colunas, de modo que os valores dos elementos são preenchidos corretamente, reduzindo assim o número de linhas e aumentando o número de colunas. 

## Selecionar colunas
pinguins_raw_sel_col <- penguins_raw[, c(2, 3, 13)]

head(pinguins_raw_sel_col)

## Pivotar para largo
pinguins_raw_pivot_wider <- tidyr::pivot_wider(data = pinguins_raw_sel_col,
                                               names_from = Species,
                                               values_from = `Body Mass (g)`)
head(pinguins_raw_pivot_wider)


## Selecionar colunas
pinguins_raw_sel_col <- penguins_raw[, c(2, 3, 10:13)]

head(pinguins_raw_sel_col)

## Pivotar para longo
pinguins_raw_pivot_longer <- tidyr::pivot_longer(
  data = pinguins_raw_sel_col,
  cols = `Culmen Length (mm)`:`Body Mass (g)`,
  names_to = "medidas", values_to = "valores")

head(pinguins_raw_pivot_longer)


## dplyr() ----
## È um pacote que facilita a manipulação de dados como filtragem, reordenamento, seleção entre outros. a forma mais conveniente de utilizar um objeto nesse pacote é a versão tibble.

# Verbos: mutate(), select(), filter(), arrange(), summarise(), slice(), rename(), etc.
# Replicação: across(), if_any(), if_all(), where(), starts_with(), ends_with(), contains(), etc.
# Agrupamento: group_by() e ungroup()
# Junções: inner_join(), full_join(), left_join(), right_join(), etc.
# Combinações: bind_rows() e bind_cols()
# Resumos, contagem e seleção: n(), n_distinct(), first(), last(), nth(), etc.

# relocate(): muda a ordem das colunas
# rename(): muda o nome das colunas
# select(): seleciona colunas pelo nome ou posição
# pull(): seleciona uma coluna como vetor
# mutate(): adiciona novas colunas ou resultados em colunas existentes
# arrange(): reordena as linhas com base nos valores de colunas
# filter(): seleciona linhas com base em valores de colunas
# slice(): seleciona linhas de diferente formas
# distinct(): remove linhas com valores repetidos com base nos valores de colunas
# count(): conta observações para um grupo com base nos valores de colunas
# group_by(): agrupa linhas pelos valores das colunas
# summarise(): resume os dados através de funções considerando valores das colunas
# *_join(): funções que juntam dados de duas tabelas através de uma coluna chave

## As funções do dplyr podem seguir uma mesma sintaxe: o tibble será sempre o primeiro argumento dessas funções, seguido de um operador pipe (%>%) e pelo nome da função que irá fazer a manipulação nesses dados.


# dplyr::relocate() ----
# Utilizado para reordenar as colunas há dois argumentos: .before que indica a coluna onde a coluna realocada deve se mover antes, e o argumento .after indicando onde deve se mover depois.

## Reordenar colunas - nome
pinguins_relocate_col <- penguins |> 
  dplyr::relocate(sex, year, .after = island)

head(pinguins_relocate_col)

## Reordenar colunas - posição
pinguins_relocate_col <- penguins |> 
  dplyr::relocate(sex, year, .after = island)

head(pinguins_relocate_col)


# dplyr::rename() ----
# Renomear colunas (nova_coluna = antiga_coluna), Também podemos utilizar a função dplyr::rename_with(), que faz a mudança do nome em múltiplas colunas, que pode depender ou não de resultados booleanos.

pinguins_rename <- penguins |> 
  dplyr::rename(bill_length = bill_len,
                bill_depth = bill_dep,
                flipper_length = flipper_len)

head(pinguins_rename)

## mudar o nome de todas as colunas para maiúsculo
pinguins_rename_with <- penguins |>
  dplyr::rename_with(toupper)

head(pinguins_rename_with)


# dplyr::select() ----
# Seleção de colunas pelo nome ou pela sua posição. Aqui há uma série de possibilidades de seleção de colunas, desde utilizar operadores como : para selecionar intervalos de colunas, ! para tomar o complemento (todas menos as listadas), além de funções como dplyr::starts_with(), dplyr::ends_with(), dplyr::contains() para procurar colunas com um padrão de texto do nome da coluna.

## Selecionar colunas por posição (Seleciona e grava no objeto)
pinguins_select_position <- penguins |> 
  dplyr::select(3:6) # Pode ser por : ou pelo nome da coluna

head(pinguins_select_position)


# dplyr::mutate() ----
# Função para adicionar ou atualizar os valores de colunas. Podemos ainda usar os argumentos .before e .after para indicar onde a nova coluna deve ficar, além do parâmetro .keep com diversas possibilidades de manter colunas depois de usar a função dplyr::mutate(). Por fim, é fundamental destacar o uso das funções de replicação: dplyr::across(), dplyr::if_any() e dplyr::if_all(), para os quais a função fará alterações em múltiplas colunas de uma vez, dependendo de resultados booleanos.

## Adicionar colunas
pinguins_mutate <- penguins |> 
  dplyr::mutate(body_mass_kg = body_mass/1e3, .before = sex)

head(pinguins_mutate)

## Modificar várias colunas
pinguins_mutate_across <- penguins |> 
  dplyr::mutate(across(where(is.factor), as.character))

head(pinguins_mutate_across)


# dplyr::arrange() ----
# Função para adicionar ou alterar valores em linhas. Podemos reordenar as linhas com base nos valores das colunas. Para essa operação, usamos a função dplyr::arrange(). Podemos reordenar por uma ou mais colunas de forma crescente ou decrescente usando a função desc() ou o operador - antes da coluna de interesse. Da mesma forma que na função dplyr::mutate(), podemos usar as funções de replicação para ordenar as linhas para várias colunas de uma vez, dependendo de resultados booleanos.

## Reordenar linhas - crescente
pinguins_arrange <- penguins |> 
  dplyr::arrange(body_mass)

head(pinguins_arrange)

## Reordenar linhas - decrescente
pinguins_arrange_desc <- penguins |>
  dplyr::arrange(-body_mass)

head(pinguins_arrange_desc)

## Reordenar linhas - multiplas colunas
pinguins_arrange_across <- penguins |>
  dplyr::arrange(across(where(is.numeric)))

head(pinguins_arrange_across)


# dplyr::filter() ----
# Função utilizada para seleção de linhas, podemos filtrar utilizando grande parte dos operadores relacionais e lógicos. Também podemos utilizar a função is.na() para filtros em elementos faltantes, e as funções dplyr::between() e dplyr::near() para filtros entre valores, e para valores próximos com certa tolerância, respectivamente.

## Filtrar linhas
pinguins_filter <- penguins |>
  dplyr::filter(species == "Adelie")

head(pinguins_filter)

## Filtrar linhas
pinguins_filter_two <- penguins |>
  dplyr::filter(species == "Adelie" & sex == "female")

head(pinguins_filter_two)

## Filtrar linhas
pinguins_filter_in <- penguins |>
  dplyr::filter(species %in% c("Adelie", "Gentoo"),
                sex == "female")

head(pinguins_filter_in)

## Filtrar linhas - NA
pinguins_filter_na <- penguins |>
  dplyr::filter(!is.na(sex) == TRUE)

head(pinguins_filter_na)

## Filtrar linhas - intervalos
pinguins_filter_between <- penguins |>
  dplyr::filter(between(body_mass, 3000, 4000))

head(pinguins_filter_between)

## Filtrar linhas por várias colunas
pinguins_filter_if <- penguins |>
  dplyr::filter(if_all(where(is.integer), ~ . > 200))

head(pinguins_filter_if)


# dplyr::slice() ----
# Função destinada a seleção das linhas por intervalos, devemos informar o argumento n para o número da linha ou intervalo das linhas. Essa função possui variações no sufixo muito interessantes:  dplyr::slice_head() e dplyr::slice_tail() seleciona as primeiras e últimas linhas, dplyr::slice_min() e dplyr::slice_max() seleciona linhas com os maiores e menores valores de uma coluna, e dplyr::slice_sample() seleciona linhas aleatoriamente.


## Seleciona linhas - head
pinguins_slice_head <- penguins |> 
  dplyr::slice_head(n = 5)

head(pinguins_slice_head)

## Seleciona linhas - max
pinguins_slice_max <- penguins |> 
  dplyr::slice_max(body_mass, n = 5)

head(pinguins_slice_max)

## Seleciona linhas - sample
pinguins_slice_sample <- penguins |> 
  dplyr::slice_sample(n = 30)

head(pinguins_slice_sample)


# dplyr::distinct() ----
# Essa função por padrão retorna apenas a(s) coluna(s) utilizada(s) para retirar as linhas com valores repetidos, sendo necessário acrescentar o argumento .keep_all = TRUE para retornar todas as colunas. Por fim, podemos usar as funções de replicação para retirar linhas com valores repetidos para mais de uma coluna, dependendo de resultados booleanos.

## Retirar linhas com valores repetidos
pinguins_distinct <- penguins |> 
  dplyr::distinct(body_mass)

head(pinguins_distinct)

## Retirar linhas com valores repetidos - manter as outras colunas
pinguins_distinct_keep_all <- penguins |> 
  dplyr::distinct(body_mass, .keep_all = TRUE)

head(pinguins_distinct_keep_all)

## Retirar linhas com valores repetidos para várias colunas
pinguins_distinct_keep_all_across <- penguins |> 
  dplyr::distinct(across(where(is.integer)), .keep_all = TRUE)

head(pinguins_distinct_keep_all_across)


# dplyr::count() ----
# Essa função contará valores de uma ou mais colunas, geralmente para variáveis categóricas, semelhante à função R Base table(), mas num contexto tidyverse.

pinguins_count <- penguins |> 
  dplyr::count(species)

pinguins_count

## Contagens de valores para mais de uma coluna
pinguins_count_two <- penguins %>%
  dplyr::count(species, island)

pinguins_count_two


# dplyr::group_by() ----
# Esta função transforma um tibble em um tibble grouped, onde as operações são realizadas “por grupo.” Essa função é utilizada geralmente junto com a função dplyr::summarise(), que veremos logo em seguida. O agrupamento não altera a aparência dos dados (além de informar como estão agrupados). A função dplyr::ungroup() remove o agrupamento. Podemos ainda usar funções de replicação para fazer os agrupamentos para mais de uma coluna, dependendo de resultados booleanos.

## Agrupamento
pinguins_group_by <- penguins |> 
  dplyr::group_by(species)

head(pinguins_group_by)

## Agrupamento de várias colunas
pinguins_group_by_across <- penguins |> 
  dplyr::group_by(across(where(is.factor)))

head(pinguins_group_by_across)


# dplyr::summarise() ----
# Resumir informações

## Resumo
pinguins_summarise <- penguins |> 
  dplyr::group_by(species) |> 
  dplyr::summarize(body_mass_mean = mean(body_mass, na.rm = TRUE),
                   body_mass_sd = sd(body_mass, na.rm = TRUE))

pinguins_summarise

## Resumo para várias colunas
pinguins_summarise_across <- penguins |> 
  dplyr::group_by(species) |> 
  dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

pinguins_summarise_across

# dplyr::bind_rows() e dplyr::bind_cols() ----
# Função para combinar duas ou mais tabelas de dados exatamente igual a função rbind e cbind.

## Selecionar as linhas para dois tibbles
pinguins_01 <- dplyr::slice(penguins, 1:5)

pinguins_02 <- dplyr::slice(penguins, 51:55)

## Combinar as linhas
pinguins_bind_rows <- dplyr::bind_rows(pinguins_01, pinguins_02,
                                       .id = "id")

head(pinguins_bind_rows)

## Combinar as colunas
pinguins_bind_cols <- dplyr::bind_cols(pinguins_01, pinguins_02,
                                       .name_repair = "unique")

head(pinguins_bind_cols)

# dplyr::*_join() ----
# Nessa operação, fazemos a combinação de pares de conjunto de dados tabulares por uma ou mais colunas chaves. Há dois tipos de junções: junção de modificação e junção de filtragem. 
# A junção de modificação -- primeiro combina as observações por suas chaves e, em seguida, copia as variáveis (colunas) de uma tabela para a outra. É fundamental destacar a importância da coluna chave, que é indicada pelo argumento by. Essa coluna deve conter elementos que sejam comuns às duas tabelas para que haja a combinação dos elementos. 
# Existem quatro tipos de junções de modificações, que são realizadas pelas funções: dplyr::inner_join(), dplyr::left_join(), dplyr::full_join() e dplyr::right_join().

# inner_join(x, y): mantém apenas as observações em x e em y - no meio entre dois circulos sobrepostos - Diagrama Venn
# left_join(x, y): mantém todas as observações em x 
# right_join(x, y): mantém todas as observações em y
# full_join(x, y): mantém todas as observações em x e em y

## Adicionar uma coluna chave de ids
pinguin_islands <- tibble(
  island = c("Torgersen", "Biscoe", "Dream", "Alpha"),
  longitude = c(-64.083333, -63.775636, -64.233333, -63),
  latitude = c(-64.766667, -64.818569, -64.733333, -64.316667))

## Junção - left
pinguins_left_join <- dplyr::left_join(penguins, pinguin_islands,
                                       by = "island")

head(pinguins_left_join)

# Já a junção de filtragem combina as observações da mesma maneira que as junções de modificação, mas afetam as observações (linhas), não as variáveis (colunas). Existem dois tipos:
# semi_join(x, y): mantém todas as observações em x que têm uma correspondência em y 
# anti_join(x, y): elimina todas as observações em x que têm uma correspondência em y.

# Operações de conjuntos e comparação de dados:
# Temos ainda operações de conjuntos e comparação de dados.
# union(x, y): retorna todas as linhas que aparecem em x, y ou mais dos conjuntos de dados
# interesect(x, y): retorna apenas as linhas que aparecem em x e em y
# setdiff(x, y): retorna as linhas que aparecem x, mas não em y
# setequal(x, y): retorna se x e y são iguais e quais suas diferenças


stringr() ----