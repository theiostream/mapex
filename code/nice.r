library(fuzzyjoin)
library(tidyr)
library(dplyr)
library(textclean)

unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

our_rep = c(' um ', ' dois ', ' tres ', ' quatro ', ' cinco ',
             ' seis ', ' sete ', ' oito ', ' nove ', ' dez ',
             ' onze ', ' doze ', ' treze ', ' catorze ', ' quatorze ', ' quinze ',
             ' dezesseis ', 'dezessete ', 'dezoito ', ' dezenove ', ' vinte ',
             ' vinte e um ', ' vinte e dois ', ' vinte e tres ', ' vinte e quatro ', ' vinte e cinco ',
             ' vinte e seis ', ' vinte e sete ', ' vinte e oito ', ' vinte e nove ', ' trinta ', ' trinta e um ',
             'estadual', 'rural', 'municipal', 'municipl', 'federal', 'est\\.', ' est ',
             'esc ', 'esc\\. ', 'escola publica ', 'grupo escolar ',
             'infantil', 'fundamental', 'fund\\.', ' ens ', ' ensino ', ' fund ', ' fun ', 'fun\\.', ' medio ', ' de ', ' do ', ' da ',
             'primeiro grau', 'segundo grau', '1 grau', '2 grau', '1 e 2 grau', '1º grau', '2º grau', '1º e 2º grau',
             ' professor ', ' professora ', ' padre ', ' doutor ', ' prof ', ' pe ', ' dr ', ' prof\\. ', ' pe\\. ', ' dr\\. ',
             ' major ', ' marechal ', ' cabo ', ' senador ', ' coronel ', ' capitao ', ' cap ', ' cap\\. ',' barao ',
             ' sargento ', ' general ', ' conde ', ' almirante ', ' tenente ', ' brigadeiro ', ' visconde ',
             ' enfermeiro ', ' enfermeira ', ' pad ', ' coroneu ', ' indigena ',
             'camera municipal', 'encino')
our_replacement = c(' 1 ', ' 2 ', ' 3 ', ' 4 ', ' 5 ', ' 6 ', ' 7 ', ' 8 ', ' 9 ', ' 10 ',
                ' 11 ', ' 12 ', ' 13 ', ' 14 ', ' 14 ', ' 15 ', ' 16 ', ' 17 ', ' 18 ', ' 19 ', ' 20 ',
                ' 21 ', ' 22 ', ' 23 ', ' 24 ', ' 25 ', ' 26 ', ' 27 ', ' 28 ', ' 29 ', ' 30 ', ' 31 ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', #estadual
                'ESCOLA ', 'ESCOLA ', 'ESCOLA ', 'ESCOLA ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', #infantil, de
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                ' ', ' ', ' ', ' ', ' ',
                'CAMARA MUNICIPAL', ' ')

print(length(our_rep))
print(length(our_replacement))

removeDiacritics <- function(string) {
    return(chartr(paste(names(unwanted_array), collapse=''),
           paste(unwanted_array, collapse=''),
           string))
}

cleanString <- function(string) {
    return(replace_white(removeDiacritics(string)))
}

ac = read.csv('./allacre.csv', header = T)
ac = ac[ac$IdEstabelecimento != '',]
ac$PlaceId = rownames(ac)

df = read.csv('./all_places_dedup.csv', header = T)
ac_df = df[df$UF == 'AC',]
ac_df$LOCAL_ID = rownames(ac_df)

ac_df$NM_LOCALVOTACAO = cleanString(ac_df$NM_LOCALVOTACAO)
ac$IdEstabelecimento = cleanString(ac$IdEstabelecimento)

ac_df$NM_LOCALVOTACAO_M = mgsub(ac_df$NM_LOCALVOTACAO, our_rep, our_replacement, ignore.case=T, fixed=F)
ac$IdEstabelecimento_M = mgsub(ac$IdEstabelecimento, our_rep, our_replacement, ignore.case=T, fixed=F)
ac_df$NM_LOCALVOTACAO_M =  replace_white(trimws(ac_df$NM_LOCALVOTACAO_M))
ac$IdEstabelecimento_M = replace_white(trimws(ac$IdEstabelecimento_M))

# Temporarily remove all ambiguous schools which will lead us nowhere
ac_storage = ac
ac = ac[ac$IdEstabelecimento_M != 'ESCOLA',]

hyphen = ac_df[grepl(' - ', ac_df$NM_LOCALVOTACAO_M, fixed = T),]
hyphen$NM_LOCALVOTACAO_SPLIT = strsplit(hyphen$NM_LOCALVOTACAO_M, ' - ', fixed=T)
h = unnest(hyphen, NM_LOCALVOTACAO_SPLIT)
h$NM_LOCALVOTACAO_M = h$NM_LOCALVOTACAO_SPLIT
ac_df = rbind(ac_df, subset(h, select=-c(NM_LOCALVOTACAO_SPLIT)))

cities = ac_df[!duplicated(ac_df$COD_MUNIC_IBGE),]$COD_MUNIC_IBGE

matched = data.frame()
not_matched = data.frame()

for (city in cities) {
    tse_addresses = ac_df[ac_df$COD_MUNIC_IBGE==city,]
    ibge_addresses = ac[ac$CompatMunicipio==city,]

    m = stringdist_inner_join(
        tse_addresses,
        ibge_addresses,
        by=c('NM_LOCALVOTACAO_M' = 'IdEstabelecimento_M'),
        distance_col='bozodistance',
        method='jw',
        p=0,
        max_dist=0.125)
    
    nm = stringdist_anti_join(
        tse_addresses,
        ibge_addresses,
        by=c('NM_LOCALVOTACAO_M' = 'IdEstabelecimento_M'),
        method='jw',
        p=0,
        max_dist=0.125)
    
    matched = rbind(matched, m)
    not_matched = rbind(not_matched, nm)
}

# Provide a warning
places = matched[!duplicated(m$PlaceId),]$PlaceId
places = places[!is.na(places)]
for (place in places) {
    p = matched[matched$PlaceId == place,]
    if (nrow(p) > 1) {
        pp = p[2:nrow(p),]$LOCAL_ID
        if (length(unique(pp)) > 1) {
            print(paste('Danger!', place))
        }
    }
}

matched = matched %>% group_by(LOCAL_ID) %>% filter(bozodistance == min(bozodistance))

ac = ac_storage
ac_notmatched = anti_join(ac, matched, by=c('PlaceId'='PlaceId'))

all_esc = read.csv('./Important_Cadastro.csv', header=T)
esc = read.csv('./allesc.csv', header = T)
esc$NomeDaEscola = replace_white(mgsub(esc$'Nome.da.Escola', our_rep, our_replacement, ignore.case=T, fixed=F))
all_esc$NomeDaEscola = replace_white(mgsub(all_esc$'Nome.da.Escola', our_rep, our_replacement, ignore.case=T, fixed=F))

matched_esc = data.frame()
not_matched_esc = data.frame()

for (city in cities) {
    tse_addresses = not_matched[not_matched$COD_MUNIC_IBGE==city,]
    inep_addresses = esc[esc$'Código.do.Município'==city,]

    m = stringdist_inner_join(
        tse_addresses,
        inep_addresses,
        by=c('NM_LOCALVOTACAO_M' = 'NomeDaEscola'),
        distance_col='bozodistance',
        method='jw',
        p=0,
        max_dist=0.125)
    
    nm = stringdist_anti_join(
        tse_addresses,
        inep_addresses,
        by=c('NM_LOCALVOTACAO_M' = 'NomeDaEscola'),
        method='jw',
        p=0,
        max_dist=0.125)
    
    matched_esc = rbind(matched_esc, m)
    not_matched_esc = rbind(not_matched_esc, nm)
}

#matched_all_esc = data.frame()
#not_matched_all_esc = data.frame()
#
#for (city in cities) {
#    tse_addresses = not_matched[not_matched$COD_MUNIC_IBGE==city,]
#    inep_addresses = all_esc[all_esc$'Código.do.Município'==city,]
#
#    m = stringdist_inner_join(
#        tse_addresses,
#        inep_addresses,
#        by=c('NM_LOCALVOTACAO_M' = 'NomeDaEscola'),
#        distance_col='bozodistance',
#        method='jw',
#        p=0,
#        max_dist=0.125)
#    
#    nm = stringdist_anti_join(
#        tse_addresses,
#        inep_addresses,
#        by=c('NM_LOCALVOTACAO_M' = 'NomeDaEscola'),
#        method='jw',
#        p=0,
#        max_dist=0.125)
#    
#    matched_all_esc = rbind(matched_all_esc, m)
#    not_matched_all_esc = rbind(not_matched_all_esc, nm)
#}

matched_esc$PlaceId = as.character(matched_esc$PlaceId)
ac$PlaceId = as.character(ac$PlaceId)

really_matched_esc = inner_join(matched_esc, ac_notmatched, by=c('PlaceId' = 'PlaceId'))
really_matched_esc = really_matched_esc %>% group_by(LOCAL_ID) %>% filter(bozodistance == min(bozodistance))

output_cols = c('LOCAL_ID',
                'PlaceId',
                'COD_MUNIC_IBGE',
                'NM_MUNIC_TSE',
                'NM_BAIRRO',
                'NM_LOCALVOTACAO',
                'IdEstabelecimento',
                'NM_LOCALVOTACAO_M',
                'IdEstabelecimento_M',
                'bozodistance')
matched = rbind(matched[,output_cols], really_matched_esc[,output_cols])

write.csv(matched[,output_cols], './matched.csv', row.names = F)

not_matched_esc = anti_join(not_matched_esc, matched, by=c('LOCAL_ID'='LOCAL_ID')) #remove hyphenation
output_cols = c(
    'LOCAL_ID',
    'COD_MUNIC_IBGE',
    'NM_MUNIC_TSE',
    'NM_BAIRRO',
    'NM_LOCALVOTACAO',
    'NM_LOCALVOTACAO_M')
write.csv(not_matched_esc[,output_cols], './not_matched.csv', row.names = F)

#debug_matched_esc = anti_join(matched_all_esc, matched, by=c('LOCAL_ID'='LOCAL_ID'))
#write.csv(debug_matched_esc[,c('COD_MUNIC_IBGE', 'NM_MUNIC_TSE', 'NM_LOCALVOTACAO', 'Nome.da.Escola', 'Endereço', 'Complemento', 'NM_BAIRRO')], './debug.csv', row.names=F)
