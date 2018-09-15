args <- commandArgs(trailingOnly = TRUE)

f <- args[1]
o <- args[2]

df <- read.fwf(file = f,
               encoding = 'UTF-8',
               strip.white = TRUE,
               widths = c(2, 5, 2, 2, 4, 1, 20, 30, 60, 8, 7, 20, 10, 20, 10, 20,
                          10, 20, 10, 20, 10, 20, 10, 15, 15, 60, 60, 2, 40, 1,
                          30, 3, 3, 8),
               col.names = c('UF', 'Municipio', 'Distrito', 'Subdistrito', 'CodSetor',
                             'SituacaoSetor', 'TipoLogradouro', 'TituloLogradouro',
                             'NomeLogradouro', 'NumeroLogradouro', 'ModificadorNumero',
                             'Elemento1', 'Valor1', 'Elemento2', 'Valor2',
                             'Elemento3', 'Valor3', 'Elemento4', 'Valor4', 'Elemento5',
                             'Valor5', 'Elemento6', 'Valor6', 'Lat', 'Lon', 'Localidade',
                             'Nulo', 'EspecieEndereco', 'IdEstabelecimento',
                             'IndicadorEndereco', 'IdDomicilioColetivo', 'Quadra',
                             'Face', 'CEP'))

compat = substr(f, 1, 7)
df$CompatMunicipio = compat 

write.csv(df, o, row.names = FALSE)
