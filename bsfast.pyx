cdef extern from "math.h":
    double log(double)
    double exp(double)
    double sqrt(double)
    double sin(double)
    double cos(double)

cdef extern from "stdlib.h":
    int random()
    void srandom(unsigned int)

cdef extern from "time.h":
    unsigned int time(void*)

cpdef seed_random():
    cpdef unsigned int t = time(NULL)
    srandom(t)

cdef inline double max(double a, double b):
    cdef double x
    if a > b:
        x = a
    else:
        x = b
    return x

cpdef double d1(double S, double K, double r, double s, double t):
    return (log(S/K) + (r+s*s/2)*t) / (s*sqrt(t))

cpdef double d2(double S, double K, double r, double s, double t):
    return d1(S, K, r, s, t) - s*sqrt(t)

cdef double kd1 = 0.0498673470
cdef double kd3 = 0.0032776263
cdef double kd5 = 0.0000488906
cdef double kd2 = 0.0211410061
cdef double kd4 = 0.0000380036
cdef double kd6 = 0.0000053830
cdef double pi = 3.141592653689
cdef double twopi = 2.0 * 3.141592653689
cdef double max_random = (1 << 31) - 1

cpdef double N(double x):
    if x < 0:
        return 1 - N(-x)
    cdef double xp, p
    xp = x # x ** 1
    p = 1.0 + kd1 * xp
    xp *= x # x ** 2
    p += kd2 * xp
    xp *= x # x ** 3
    p += kd3 * xp
    xp *= x # x ** 4
    p += kd4 * xp
    xp *= x # x ** 5
    p += kd5 * xp
    xp *= x # x ** 6
    p += kd6 * xp
    p *= p # ** 2
    p *= p # ** 4
    p *= p # ** 8
    p *= p # ** 16
    return 1.0 - 0.5 * (1.0 / p) # ** -16

cpdef double dN(double x):
    cdef double n = exp(-(x*x/2))
    n /= sqrt(2.0 * pi)
    return n

cpdef double Call(double S, double K, double r, double s, double t):
    if t < 0.0000000001 or s < 0.00000000001:
        return max(S - K, 0.0)
    return S*N(d1(S, K, r, s, t)) - K*exp(-r*t)*N(d2(S, K, r, s, t))

cpdef double Put(double S, double K, double r, double s, double t):
    if t < 0.0000000001 or s < 0.00000000001:
        return max(K - S, 0.0)
    return - S*N(-d1(S, K, r, s, t)) + K*exp(-r*t)*N(-d2(S, K, r, s, t))

cdef double next_gauss = 0
cdef int has_next_gauss = 0

cpdef double gauss(double mu, double sigma):
    cdef double z, rand1, rand2, x2pi, g2rad
    global next_gauss, has_next_gauss
    if has_next_gauss == 0:
        rand1 = random() / max_random
        rand2 = random() / max_random
        x2pi = rand1 * twopi
        g2rad = sqrt(-2.0 * log(1.0 - rand2))
        z = cos(x2pi) * g2rad
        next_gauss = sin(x2pi) * g2rad
        has_next_gauss = 1
    else:
        has_next_gauss = 0
        z = next_gauss
    return mu + z * sigma

cpdef double gen_yield(double u, double s, double s2, double t):
    cdef double sl = max(0.0, gauss(s, s2))
    return gauss(u * t, sl * sqrt(t))

cpdef double gen_gap(double s_gap):
    return gauss(0.0, s_gap)
