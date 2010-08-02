#!/usr/bin/env python
# -*- encoding: utf-8

# Simulação de venda coberta levada até o vencimento
# Copyright © 2010 Elvis Pfützenreuter

from s_common import *
import math
from blackscholes import *

args, log = parse_args()

K = args[0]
s2 = args[1]
r = 0.09
r2 = r * args[2]
s = args[3]
s2 *= s
t = 30.0 / 365.0

retornos = []
retornos_acoes = []

# vende coberto 1 opcao
S = 1.0
premio = Call(S, K, r, s, t)

def calc_round():
    S = 1.0

    # acoes oscilaram
    market = gen_yield(r2, s, s2, t)
    S = S * math.exp(market)

    # vende as acoes, recompra opcao e apura o dinheiro
    payoff_acoes = S - 1.0
    payoff = premio + payoff_acoes - Call(S, K, r, s, 0.0)

    return (math.log(1.0 + payoff), math.log(1.0 + payoff_acoes))


for i in range(0, 100000):
    payoff, payoff_acoes = calc_round()
    retornos.append(payoff)
    retornos_acoes.append(payoff_acoes)

retorno_desvio = 0.0
retorno_acoes_desvio = 0.0
retorno_medio = sum(retornos) / len(retornos)
retorno_acoes_medio = sum(retornos_acoes) / len(retornos_acoes)

for i in range(0, len(retornos)):
    retorno_desvio += (retornos[i] - retorno_medio) ** 2
    retorno_acoes_desvio += (retornos_acoes[i] - retorno_acoes_medio) ** 2

retorno_desvio = math.sqrt(retorno_desvio / len(retornos))
retorno_acoes_desvio = math.sqrt(retorno_acoes_desvio / len(retornos_acoes))

# Anualizar
retorno_medio /= t
retorno_acoes_medio /= t
retorno_desvio /= math.sqrt(t)
retorno_acoes_desvio /= math.sqrt(t)

retorno_comp = retorno_medio - (retorno_desvio ** 2) / 2
retorno_acoes_comp = retorno_acoes_medio - (retorno_acoes_desvio ** 2) / 2

print "#",
print ("%.4f " * len(args)) % tuple(args),
print ("%.4f " * 6) % (retorno_medio * 100, retorno_desvio * 100,
                       retorno_comp * 100, retorno_acoes_medio * 100,
                       retorno_acoes_desvio * 100, retorno_acoes_comp * 100)
