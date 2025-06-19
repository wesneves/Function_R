library(googlesheets4)

# Definir o autor da planilha do google
gs4_auth() 

# Criar uma nova planilha
sheet <- gs4_create("NovaPlanilha", sheets = list(dados = mtcars))

# Mostrar o link da planilha no console
sheet_obj <- gs4_get(sheet)
sheet_obj$spreadsheet_url

# Ler dados de uma planilha pública
dados <- read_sheet("https://docs.google.com/spreadsheets/d/1cMshIrPJuMC_x6sogUrq4Vr62V9ppqIm-yZWVDI6vPE/edit?ouid=110451377940373390122")

# Escrever dados em uma aba existente
sheet_write(mtcars, ss = sheet, sheet = "dados")

# Atualizar dados
range_write(mtcars[1:5, ], ss = sheet, range = "A1", sheet = "dados")
