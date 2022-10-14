# TODO
- [x] Ver com o professor se o tamanho da matrize é realmente *até* 9 (ou se é só para aceitar 4, 6 ou 9) e como funciona o dimensionamento das regiões
- [x] Parsear arquivos txt
    - Vide a.txt
    - Linha 1 tamanho da matriz NxN
    - As outras são células no formato 0+-
    
- [ ] Implementar as funções de validação baseadas nas regras do joojinho
- [ ] O número não pode estar na mesma linha ou coluna e retirar da lista de possibilidades.
    - [ ] Pega a posição (x,y) e sempre verifica  (x-1,y), (x,y-1),(x+1,y), (x, y+1)  que são os ao redor.
        - [ ] Sendo validado ao ser feito a solução

    

- [ ] Implementar o solucionador
    - [ ] O número não pode estar na mesma linha ou coluna e retirar da lista de possibilidades.
    - [ ] Pega a posição (x,y) e sempre verifica  (x-1,y), (x,y-1),(x+1,y), (x, y+1)  que são os ao redor.
        - [ ] Verificando segundo a lógica das operações
            - [ ] Caso não tenha um item no (x,y) passado, é tratado como exceção e pula.
            - [ ] Caso tenha, verifica a relação do entre o (x,y) e o (x’, y’) e retira da lista de possibilidades dependendo da análise ( < ou > ; ‘+ ou -’ )
    
    - [ ] Tendo a lista de possibilidades, escolhe um número possível e passa para a próxima posição dentro da lista.
    - [ ] Caso esteja em uma e está vazia as possibilidades para a posição (x,y), volta para a posição anterior e refaz a linha ou em comum com o elemento (x,y), para assim tentar liberar uma possibilidade.


Abordagem: 
Resolver por Linha, sempre seguindo na sequência, caso não encontre algo, volta fazendo recursão célula por célula
Resolver por Região
