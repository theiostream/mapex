library(fuzzyjoin)
library(tidyr)
library(textclean)

rep_esc = c('AV ', 'AV. ', 'R ', 'R. ', 'ESTR. ', 'ESTR ', 'ROD. ', 'ROD ', 'TRAV ', 'TRAV. ')
rep_esc_with = c('AVENIDA ', 'AVENIDA ', 'RUA ', 'RUA ', 'ESTRADA ', 'ESTRADA ', 'RODOVIA ', 'RODOVIA ', 'TRAVESSA ', 'TRAVESSA ')

esc = read.csv('./Important_Cadastro.csv', header = T)

esc$RamalSplit = strsplit(as.character(esc$'Endereço'), 'RAMAL', fixed=T)
esc$EnderecoInteiro = ifelse(grepl('RAMAL', esc$'Endereço', fixed=T), paste('RAMAL', sapply(esc$RamalSplit, function(x) x[2])), as.character(esc$'Endereço'))
esc$EnderecoSplit = strsplit(as.character(esc$EnderecoInteiro), 'KM', fixed=T)
esc$EnderecoInteiro = sapply(esc$EnderecoSplit, function(x) x[1])
esc$EnderecoInteiro = trimws(mgsub(esc$EnderecoInteiro, rep_esc, rep_esc_with, ignore.case=T, fixed=F))

esc$NumeroSplit = strsplit(as.character(esc$'Número'), 'KM', fixed=T)
esc$NumeroInteiro = ifelse(grepl('KM', esc$'Número', fixed=T), NA, as.character(esc$'Número'))

esc$EnderecoKm = trimws(ifelse(grepl('KM', esc$'Número', fixed=T), sapply(esc$NumeroSplit, function(x) x[2]), sapply(esc$EnderecoSplit, function(x) x[2])))

ac = read.csv('./allacre.csv', header = T)
ac$PlaceId = rownames(ac)
ac = ac[ac$EspecieEndereco == 4,]

ac_bad = ac
ac_bad$FullAddr = paste(ac_bad$TipoLogradouro, ac_bad$TituloLogradouro, ac_bad$NomeLogradouro)
ac_bad$FullAddr = replace_white(ac_bad$FullAddr)
ac_bad$FullNumber = ifelse(ac_bad$ModificadorNumero != 'KM', trimws(paste(ac_bad$ModificadorNumero, ac_bad$NumeroLogradouro)), NA)
ac_bad$AddrKm = ifelse(ac_bad$ModificadorNumero == 'KM', ac_bad$NumeroLogradouro, NA)

cities = ac_bad[!duplicated(ac_bad$CompatMunicipio),]$CompatMunicipio

m = data.frame()
for (city in cities) {
    print(city)
    ibge_addresses = ac_bad[ac_bad$CompatMunicipio==city,]
    inep_addresses = esc[esc$'Código.do.Município'==city,]

    x = stringdist_left_join(
        ibge_addresses,
        inep_addresses,
        by=c('FullAddr' = 'EnderecoInteiro'),
        distance_col='escdistance',
        method='jw',
        p=0,
        max_dist=0.11)

    if (!('escdistance' %in% colnames(x))) {
        x$escdistance = NA
    }

    print(paste(nrow(x), 'out of', nrow(ibge_addresses)))
    m = rbind(m, x)
}

write.csv(m[is.na(m$escdistance),c('PlaceId', 'Nome.da.Escola', 'FullAddr', 'FullNumber', 'EnderecoInteiro', 'NumeroInteiro', 'escdistance')], './testesc_na.csv', row.names = F)

o = m[!is.na(m$escdistance),]
ambiguous = o[(duplicated(o$PlaceId) | duplicated(o$PlaceId, fromLast = TRUE)),]

o = o[!(duplicated(o$PlaceId) | duplicated(o$PlaceId, fromLast = TRUE)),]
o = o[order(o$escdistance),]
o = o[!duplicated(o$'Nome.da.Escola'),]
write.csv(o[,c('Código.do.Município', 'PlaceId', 'Nome.da.Escola', 'FullAddr', 'FullNumber', 'EnderecoInteiro', 'NumeroInteiro', 'escdistance')], './testesc.csv', row.names = F)

unambiguous_numbers = ambiguous[!is.na(ambiguous$NumeroInteiro) & !is.na(ambiguous$FullNumber) & ambiguous$NumeroInteiro == ambiguous$FullNumber,]
unambiguous_numbers = rbind(unambiguous_numbers, ambiguous[!(duplicated(ambiguous$PlaceId) | duplicated(ambiguous$PlaceId, fromLast = TRUE)),])
write.csv(unambiguous_numbers[,c('Código.do.Município', 'PlaceId', 'Nome.da.Escola', 'FullAddr', 'FullNumber', 'EnderecoInteiro', 'NumeroInteiro', 'escdistance')], './testesc2.csv', row.names = F)

ambiguous = ambiguous[!(ambiguous$PlaceId %in% unambiguous_numbers$PlaceId | ambiguous$'Nome.da.Escola' %in% unambiguous_numbers$'Nome.da.Escola'),]
write.csv(ambiguous[,c('PlaceId', 'Nome.da.Escola', 'FullAddr', 'FullNumber', 'AddrKm', 'EnderecoInteiro', 'NumeroInteiro', 'EnderecoKm', 'escdistance')], './testesc3.csv', row.names = F)
