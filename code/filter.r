df = read.csv('./testoup.csv', header=T)

o = df[,c('LOCAL_ID', 'NM_MUNIC_TSE', 'NM_BAIRRO', 'NM_LOCALVOTACAO', 'IdEstabelecimento', 'NM_LOCALVOTACAO_M', 'IdEstabelecimento_M', 'bozodistance')]
o = o[order(o$LOCAL_ID, o$bozodistance),]
o = o[!duplicated(o$LOCAL_ID),]

matched = o[!is.na(o$bozodistance),]
notmatched = o[is.na(o$bozodistance),]

write.csv(matched, './analysis_m.csv', row.names = F)
write.csv(notmatched, './analysis_nm.csv', row.names = F)
