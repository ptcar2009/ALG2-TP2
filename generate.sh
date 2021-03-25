cd casos
echo "nome;matricula;algoritmo;arquivo;valor;explorados;tempo" > ../saida.csv
for i in $(seq 10);do
    for alg in bnb back; do
        ../main $alg $(ls) >> ../saida.csv
    done
done
cd ..
