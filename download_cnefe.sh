#!/bin/bash

REPO=$(pwd)
echo $REPO

pushd data/cnefe
wget -r ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/$1/

mv ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/$1/* .
rm -rf ftp.ibge.gov.br

for city in *.zip; do
    unzip $city
done
rm *.zip

for city in *.TXT; do
    iconv -f latin1 -t UTF-8 $city > $city.utf8txt
done

for city in *.utf8txt; do
    rscript $REPO/code/read_ibge.r $city $city.csv
done

rm *.TXT
rm *.utf8txt

for city in *.csv; do
    head -1 $city > all${1}.lolcsv
    break
done

for city in *.csv; do
    sed 1d $city >> all${1}.lolcsv
done

mkdir $1
mv *.csv $1

mkdir all
mv all${1}.lolcsv all/all${1}.csv

popd
