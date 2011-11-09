#!/usr/bin/env clisp

(defvar *looptimes* 100000)

(defun square (x)
	(* x x)
)

(defun d1 (SS K r s tt)
	(/	(+	(log (/ SS K))
			(* tt (+ r (/ (square s) 2))))
		(* s (sqrt tt)))
)

;def d1(SS, K, r, s, t):
;    return (math.log(SS/float(K)) + (r+(s*s)/2.0)*t) / (s*math.sqrt(t))

(defun d2 (SS K r s tt)
	(-	(d1 SS K r s tt)
		(* s (sqrt tt)))
)

; def d2(SS, K, r, s, t):
;    return d1(SS, K, r, s, t) - s*math.sqrt(t)

(defconstant kd1 0.0498673470)
(defconstant kd3 0.0032776263)
(defconstant kd5 0.0000488906)
(defconstant kd2 0.0211410061)
(defconstant kd4 0.0000380036)
(defconstant kd6 0.0000053830)

(defun N (x)
	(let ((xp x) (p 0))

	(if (< x 0)
		(return-from N (- 1.0 (N (* x -1))))
	)

	; (setf xp x)
	(setf p (+ 1.0 (* kd1 xp)))
	(setf xp (* xp x))
	(setf p (+ p (* kd2 xp)))
	(setf xp (* xp x))
	(setf p (+ p (* kd3 xp)))
	(setf xp (* xp x))
	(setf p (+ p (* kd4 xp)))
	(setf xp (* xp x))
	(setf p (+ p (* kd5 xp)))
	(setf xp (* xp x))
	(setf p (+ p (* kd6 xp)))
	(setf p (* p p))
	(setf p (* p p))
	(setf p (* p p))
	(setf p (* p p))

	(- 1.0 (* 0.5 (/ 1.0 p)))
))

; def N(x):
;    if x < 0:
;        return 1.0 - N(-x)
;    xp = x
;    p = 1.0 + N.kd1 * xp
;    xp *= x
;    p += N.kd2 * xp
;    xp *= x
;    p += N.kd3 * xp
;    xp *= x
;    p += N.kd4 * xp
;    xp *= x
;    p += N.kd5 * xp
;    xp *= x
;    p += N.kd6 * xp
;    p *= p                
;    p *= p                
;    p *= p                
;    p *= p                
;    return 1.0 - 0.5 * (1.0 / p)

; N.kd1 = 0.0498673470
; N.kd3 = 0.0032776263
; N.kd5 = 0.0000488906
; N.kd2 = 0.0211410061
; N.kd4 = 0.0000380036
; N.kd6 = 0.0000053830

(defun Call (SS K r s tt)
	(if (or (< tt 0.0000000001) (< s 0.0000000001))
		(max (- SS K) 0.0)
		(-	(* SS (N (d1 SS K r s tt)))
			(* K (exp (* -1 r tt)) (N (d2 SS K r s tt)))
		)
	)
)

; (format t "~6$~%" (Call 1.0 1.0 0.12 0.25 (/ 2.0 12.0)))
; (format t "~6$~%" (Call 1.1 1.0 0.12 0.25 0.0))
; (format t "~6$~%" (Call 0.9 1.0 0.12 0.25 0.0))

; def Call(SS, K, r, s, t):
;    if t < 0.0000000001 or s < 0.00000000001:
;        return max(SS - K, 0.0)
;    return SS*N(d1(SS, K, r, s, t)) - K*math.exp(-r*t)*N(d2(SS, K, r, s, t))

(defun gaussian-random (&optional min max)
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by MIN and MAX. Gaussian random numbers form a standard
normal distribution around 0.0d0."
  (labels ((gauss ()
             (loop
                for x1 = (- (random 2.0d0) 1.0d0)
                for x2 = (- (random 2.0d0) 1.0d0)
                for w = (+ (expt x1 2) (expt x2 2))
                when (< w 1.0d0)
                do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                     (return (values (* x1 v) (* x2 v))))))
           (guard (x min max)
             (unless (<= min x max)
               (tagbody
                :retry
                  (multiple-value-bind (x1 x2) (gauss)
                    (when (<= min x1 max)
                      (setf x x1)
                      (go :done))
                    (when (<= min x2 max)
                      (setf x x2)
                      (go :done))
                    (go :retry))
                :done))
             x))
    (multiple-value-bind (g1 g2) (gauss)
      (values (guard g1 (or min g1) (or max g1))
              (guard g2 (or min g2) (or max g2))))))
; copied from http://common-lisp.net/~loliveira/darcs/alexandria/numbers.lisp

(defun randomgauss (avg dev)
	(let ((x (gaussian-random)))
	(+ avg (* x dev))
))

(defun gen_yield (u s s2 tt)
	(let ((sl (randomgauss s s2)))
	(setf sl (max 0.0 sl))
	(randomgauss (* u tt) (* sl (sqrt tt)))
))

; def gen_yield(u, s, s2, t):
;    # gera volatilidade com media "s" e desvio-padrao "s2"
;    sl = random.gauss(s, s2)
;    sl = max(0.0, sl)
;    # gera rendimento com media "u" e desvio-padrao "s"
;    return random.gauss(u * t, sl * math.sqrt(t))

(defun calc_round (premio r r2 s s2 tt)
	(let ((market (gen_yield r2 s s2 tt)) (SS NIL) (payoff_acoes NIL) (payoff NIL))
	(setf SS (exp market))
	(setf payoff_acoes (- SS 1.0))
	(setf payoff (- (+ premio payoff_acoes) (Call SS K r s 0.0)))

	; (format t "~6$ ~6$ ~6$ ~6$ ~6$: " premio SS payoff_acoes (Call SS K r s 0.0) payoff)

	(vector (log (+ 1.0 payoff)) (log (+ 1.0 payoff_acoes)))
))

; def calc_round(premio, r2, s, s2, t):
;    SS = 1.0
;
;    # acoes oscilaram
;    market = gen_yield(r2, s, s2, t)
;    SS = SS * math.exp(market)
;
;    # vende as acoes, recompra opcao e apura o dinheiro
;    payoff_acoes = SS - 1.0
;    payoff = premio + payoff_acoes - Call(SS, K, r, s, 0.0)
;
;    return (math.log(1.0 + payoff), math.log(1.0 + payoff_acoes))


(defvar args (vector 1.0 0.0 0.0 0.25))
; FIXME ler args da entrada padrao

(defvar K (aref args 0))
(defvar s2 (aref args 1))
(defvar r 0.09)
(defvar r2 (* r (aref args 2)))
(defvar s (aref args 3))
(defvar s2 (* s2 s))
(defvar tt (/ 30.0 365.0))

; K = args[0]
; s2 = args[1]
; r = 0.09
; r2 = r * args[2]
; s = args[3]
; s2 *= s
; t = 30.0 / 365.0

(defvar retornos (make-array *looptimes* :adjustable t :fill-pointer 0 :element-type 'float))
(defvar retornos_acoes (make-array *looptimes* :adjustable t :fill-pointer 0 :element-type 'float))

; retornos = []
; retornos_acoes = []

(defvar premio (Call 1.0 K r s tt))

; # vende coberto 1 opcao
; SS = 1.0
; premio = Call(SS, K, r, s, t)

(dotimes (i *looptimes*)
	(let ((ret (calc_round premio r r2 s s2 tt)))
	
	; (format t "~6$ ~6$~%" (aref ret 0) (aref ret 1))
	(vector-push (aref ret 0) retornos)
	(vector-push (aref ret 1) retornos_acoes)
))

; for i in range(0, 1000000):
;     payoff, payoff_acoes = calc_round(premio, r2, s, s2, t)
;     retornos.append(payoff)
;     retornos_acoes.append(payoff_acoes)

(defvar retorno_desvio 0.0)
(defvar retorno_acoes_desvio 0.0)
(defvar retorno_medio (/ (reduce #'+ retornos) *looptimes*))
(defvar retorno_acoes_medio (/ (reduce #'+ retornos_acoes) *looptimes*))

; retorno_desvio = 0.0
; retorno_acoes_desvio = 0.0
; retorno_medio = sum(retornos) / len(retornos)
; retorno_acoes_medio = sum(retornos_acoes) / len(retornos_acoes)

(dotimes (i *looptimes*)
	(let ( (retornos_i (aref retornos i))
		(retornos_acoes_i (aref retornos_acoes i)))

		(setf retorno_desvio (+ retorno_desvio
			(square (- retornos_i retorno_medio))))
		(setf retorno_acoes_desvio (+ retorno_acoes_desvio 
			(square (- retornos_acoes_i retorno_acoes_medio))))
	)
)

; for i in range(0, len(retornos)):
;    retorno_desvio += (retornos[i] - retorno_medio) ** 2
;    retorno_acoes_desvio += (retornos_acoes[i] - retorno_acoes_medio) ** 2

(setf retorno_desvio (sqrt (/ retorno_desvio *looptimes*)))
(setf retorno_acoes_desvio (sqrt (/ retorno_acoes_desvio *looptimes*)))

;retorno_desvio = math.sqrt(retorno_desvio / len(retornos))
;retorno_acoes_desvio = math.sqrt(retorno_acoes_desvio / len(retornos_acoes))

(setf retorno_medio (/ retorno_medio tt))
(setf retorno_acoes_medio (/ retorno_acoes_medio tt))
(setf retorno_desvio (/ retorno_desvio (sqrt tt)))
(setf retorno_acoes_desvio (/ retorno_acoes_desvio (sqrt tt)))

; # Anualizar
; retorno_medio /= t
; retorno_acoes_medio /= t
; retorno_desvio /= math.sqrt(t)
; retorno_acoes_desvio /= math.sqrt(t)

(defvar retorno_comp
	(-	retorno_medio
		(/ (square retorno_desvio) 2)))
(defvar retorno_acoes_comp
	(- 	retorno_acoes_medio 
		(/ (square retorno_acoes_desvio) 2)))

; retorno_comp = retorno_medio - (retorno_desvio ** 2) / 2
; retorno_acoes_comp = retorno_acoes_medio - (retorno_acoes_desvio ** 2) / 2

(format t "# ~{~4$ ~} ~{~4$ ~}"
	(coerce args 'list)
	(list (* 100 retorno_medio) (* 100 retorno_desvio)
	      (* 100 retorno_comp) (* 100 retorno_acoes_medio)
	      (* 100 retorno_acoes_desvio) (* 100 retorno_acoes_comp) ))

; print "#",
; print ("%.4f " * len(args)) % tuple(args),
; print ("%.4f " * 6) % (retorno_medio * 100, retorno_desvio * 100,
;                       retorno_comp * 100, retorno_acoes_medio * 100,
;                       retorno_acoes_desvio * 100, retorno_acoes_comp * 100)

