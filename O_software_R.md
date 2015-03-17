Como baixar, instalar e aprender a usar o R

# Baixando o software #

O endereço para baixar o software é o seguinte:

http://www.vps.fmvz.usp.br/CRAN/

# Aprendendo a linguagem #

A seguir são apresentados alguns elementos básicos do R e referências de leitura.

# Um loop #

i) o símbolo "<-" equivale a "=".

Ao ler:

ano\_inicial <- 2010

entenda "a variável _ano\_inicial_ é igual a _2010_"

ii) o símbolo "c()" significa "concatenar" e, quando os parênteses estão vazios, significa que a variável está sendo inicializada.

Ao ler:

anos <- c()

entenda "a variável _anos_ está sendo inicializada mas nenhum valor foi ainda atribuído a ela".

iii) vejamos agora como funciona um _loop_:

Ao ler:

for (i in 0:10) {anos <- ano\_inicial+i}

entenda "quando a variável _i_ assumir sucessivamente valores entre 1 e 10, a variável _anos_ assumirá valores iguais a _ano\_inicial_ (ou seja, 2010) mais _i_". Os valores da variável _anos_ serão, portanto, 2010, 2011,...,2021.

iv) Ao ler:

ano\_inicial <- 2010

anos  <-  c()

for (i in 0:10) {anos <- ano\_inicial+i}

entenda "a variável _anos_ assume os valores 2010, 2011,...,2021 e esses valores encontram-se armazenados sob o nome _anos_".

# Algumas funções #

i) read.table

Ao ler:

parametro <- read.table(file="c:/caminho do arquivo/nome do arquivo.txt")

entenda: "a variável _parâmetro_ é igual ao conteúdo do arquivo "c:/caminho do arquivo/nome do arquivo.txt"

ii) write.table

Ao ler:

write.table(ano,file="c:/caminho do arquivo/nome do arquivo.txt")

entenda: o conteúdo da variável _ano_ é gravada no arquivo "nome do arquivo.txt", localizado em "c:/caminho do arquivo/"

# Referências #

As seguintes fontes apresentam maiores informações sobre a linguagem:

Os seguinte livros, em formato eletrônico, estão disponíveis on-line:

R: a Language and Environment for Statistical Computing <http://cran.r-project.org/doc/manuals/refman.pdf>

An introduction to R <http://cran.r-project.org/doc/manuals/R-intro.pdf>