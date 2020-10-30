# Three Dragons - Board Game

Project for the Logic Programming course 2020

---
## Identificação
#### Turma 2, Grupo Three_Dragons_4
- João Diogo Martins Romão (up201806779)
- Rafael Valente Cristino (up201806680)


## Descrição do jogo

<!--Descrição do jogo e das suas regras (até 300 palavras). Incluir ligações usadas (página do jogo, livro de regras...).

NOTA: neste momento tem 382 palavras, acima do que é pedido. Secalhar remover algo / sintetizar mais.
-->

O tabuleiro do jogo é constituído por uma grelha quadrada 9x9, com uma *montanha* em cada canto e três *caves de dragão* no centro (para referência, ver a ligação 'Tabuleiro' em baixo).

Um dos conceitos chave é o de "Captura Custodial". A peça do jogador adversário é capturada quando é rodeada em lados opostos com duas peças, ou então com uma peça e uma *cave de dragão* ou com uma peça e uma *montanha*.

O jogo começa com o jogador que tem as peças brancas. As peças podem ser movidas ortogonalmente, qualquer numero de quadrados (como a torre do xadrez). Não podem ser ocupados os quadrado onde existirem *montanhas* ou *caves de dragão*.

Quando uma peça é capturada, é removida do tabuleiro.

O jogo termina quando um dos jogadores tiver apenas uma peça, sendo o que tiver mais peças no tabuleiro o vencedor.

**Notas:**
- Um jogador pode mover a sua peça entre duas peças adversárias sem ser capturado (não pode é deixar a peça no meio delas).
- Uma jogada pode capturar mais do que uma peça do adversário.

### Variantes

**Captura por poder**

Uso de dados em vez de peças brancas e pretas. O número de cada dado voltado para cima indica o poder de cada peça.

Adiciona-se um novo modo de captura: quando se termina um movimento e se deixa a peça junto a uma das peças do adversário, e a nossa peça tem poder maior que a outra.

*Notas:* terminar o movimento ao lado de uma peça do adversário com maior poder não resulta na captura da peça movida; Captura por poder apenas pode capturar uma peça de cada vez; Se ocorrer captura custodial e captura por poder ao mesmo tempo, o jogador escolhe qual prefere.

**Captura por poder + dragões**

Adiciona à variante de captura por poder.

Guardados três dados de lado (que serão os dragões). Cada uma das caves de dragão pode invocar um dragão apenas uma vez. 

Se o jogador for o primeiro a rodear uma dada cave de dragão em todos os lados, coloca-se um dragão por cima da cave, que passa a fazer parte do arsenal do jogador. 

As caves dos lados fazem aparecer dragões com 3 pontos de poder; a do centro um dragão com 5 pontos de poder.


#### Ligações
[Página do jogo](https://boardgamegeek.com/boardgame/306972/three-dragons)

[Regras](https://s3.amazonaws.com/geekdo-files.com/bgg269618?response-content-disposition=inline%3B%20filename%3D%22Three_Dragons_Rules_version_1.0.pdf%22&response-content-type=application%2Fpdf&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAJYFNCT7FKCE4O6TA%2F20201030%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20201030T224441Z&X-Amz-SignedHeaders=host&X-Amz-Expires=120&X-Amz-Signature=a1249e0aaec5e63d4d5dbb943466e9caf86304eeff722a67ed031a81b65bff46)

[Tabuleiro](https://s3.amazonaws.com/geekdo-files.com/bgg269617?response-content-disposition=inline%3B%20filename%3D%223_Dragons_Board_version_10.pdf%22&response-content-type=application%2Fpdf&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAJYFNCT7FKCE4O6TA%2F20201030%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20201030T224505Z&X-Amz-SignedHeaders=host&X-Amz-Expires=120&X-Amz-Signature=7a3d79c00c9bdc00b3cf21d592219fc5e3e503c5e252dc8e2f1f0436040e4c35)

## Representação interna do estado do jogo
<!--Indicação de como é representado o estado do jogo, incluindo tabuleiro, jogador atual, peças capturadas ou ainda por jogar / outras informações necessárias. Exemplos da representação em Prolog de estados *inicial*, *intermédio* e *final*. Indicação do significado de cada átomo.-->

## Visualização do estado do jogo
<!--Pequena descrição da implementação do predicado de visualização do estado de jogo. Até 200 palavras.-->


***Não esquecer: imagens ilustrativas da execução do código***

