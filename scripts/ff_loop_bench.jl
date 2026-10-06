# Timing test for the aspherical form factor loop of Tonto's HAR, in Julia:
# the port of scripts/ff_loop_bench.f90, for comparing compilers.
#
# For one atom:  f(k) = sum_i rho(i) exp(i k.r_i),  k = 1..n_k,  i = 1..n_pt.
# The versions carry the letters of the Fortran program where they match it:
#   A   k outside, points inside, complex exponential (cis)
#   D   loops swapped: points outside, k inside, Julia's own sin and cos
#   Ds  as D with @simd
#   Dc  as D with sincos, which shares the argument reduction
#   E   as D, sin and cos together by hand (sincos_poly): reduction to
#       [-pi/4,pi/4], the Cephes polynomials, integer rounding
#   F   as E with no integer arithmetic (sincos_real): rounding by adding
#       and subtracting 1.5*2^52. Julia does not reassociate floating point
#       unless asked, so this needs no special setting.
#   T   as D under LoopVectorization's @turbo, if that package is installed
# Each is timed on its second call, so compilation is not counted, and its
# largest relative difference from A is printed.
#
# Usage:  julia -O3 scripts/ff_loop_bench.jl [n_k n_pt]     defaults 8000 6000

using Random, Printf

const HAVE_TURBO = try
    @eval using LoopVectorization
    true
catch
    false
end

function version_A!(f, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    @inbounds for j in 1:n_k
        s = zero(ComplexF64)
        for i in 1:n_pt
            x = kv[j,1]*pt[i,1] + kv[j,2]*pt[i,2] + kv[j,3]*pt[i,3]
            s += rho[i]*cis(x)
        end
        f[j] = s
    end
    return f
end

function version_D!(re, im, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    fill!(re, 0.0); fill!(im, 0.0)
    @inbounds for i in 1:n_pt
        px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
        for j in 1:n_k
            x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
            re[j] += r*cos(x)
            im[j] += r*sin(x)
        end
    end
end

function version_Ds!(re, im, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    fill!(re, 0.0); fill!(im, 0.0)
    @inbounds for i in 1:n_pt
        px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
        @simd for j in 1:n_k
            x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
            re[j] += r*cos(x)
            im[j] += r*sin(x)
        end
    end
end

function version_Dc!(re, im, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    fill!(re, 0.0); fill!(im, 0.0)
    @inbounds for i in 1:n_pt
        px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
        @simd for j in 1:n_k
            x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
            sn, cs = sincos(x)
            re[j] += r*cs
            im[j] += r*sn
        end
    end
end

# The constants of sincos_poly and sincos_real in the Fortran program
const TWO_OVER_PI = 0.63661977236758134308
const BIG = 6755399441055744.0                      # 1.5*2^52
const P1 = 1.57079625129699707031                   # pi/2 in three parts
const P2 = 7.54978941586159635335e-8
const P3 = 5.39030285815811905290e-15
const S1 = -1.66666666666666307295e-1; const S2 = 8.33333333332211858878e-3
const S3 = -1.98412698295895385996e-4; const S4 = 2.75573136213857245213e-6
const S5 = -2.50507477628578072866e-8; const S6 = 1.58962301576546568060e-10
const C1 = 4.16666666666665929218e-2;  const C2 = -1.38888888888730564116e-3
const C3 = 2.48015872888517045348e-5;  const C4 = -2.75573141792967388112e-7
const C5 = 2.08757008419747316778e-9;  const C6 = -1.13585365213876817300e-11

@inline function sincos_poly(x::Float64)
    n  = round(x*TWO_OVER_PI)
    r  = ((x - n*P1) - n*P2) - n*P3
    z  = r*r
    sr = r + r*z*(S1 + z*(S2 + z*(S3 + z*(S4 + z*(S5 + z*S6)))))
    cr = 1.0 - 0.5*z + z*z*(C1 + z*(C2 + z*(C3 + z*(C4 + z*(C5 + z*C6)))))
    q  = unsafe_trunc(Int, n) & 3
    even = (q & 1) == 0
    s  = ifelse(even, sr, cr)
    c  = ifelse(even, cr, sr)
    s  = ifelse(q >= 2, -s, s)
    c  = ifelse((q == 1) | (q == 2), -c, c)
    return s, c
end

@inline function sincos_real(x::Float64)
    n  = (x*TWO_OVER_PI + BIG) - BIG                   # nearest integer
    q  = n - 4.0*((0.25*n - 0.375 + BIG) - BIG)        # n mod 4, in 0..3
    r  = ((x - n*P1) - n*P2) - n*P3
    z  = r*r
    sr = r + r*z*(S1 + z*(S2 + z*(S3 + z*(S4 + z*(S5 + z*S6)))))
    cr = 1.0 - 0.5*z + z*z*(C1 + z*(C2 + z*(C3 + z*(C4 + z*(C5 + z*C6)))))
    even = (q == 0.0) | (q == 2.0)
    s  = ifelse(even, sr, cr)
    c  = ifelse(even, cr, sr)
    s  = ifelse(q >= 2.0, -s, s)
    c  = ifelse((q == 1.0) | (q == 2.0), -c, c)
    return s, c
end

function version_E!(re, im, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    fill!(re, 0.0); fill!(im, 0.0)
    @inbounds for i in 1:n_pt
        px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
        @simd for j in 1:n_k
            x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
            sn, cs = sincos_poly(x)
            re[j] += r*cs
            im[j] += r*sn
        end
    end
end

function version_F!(re, im, kv, pt, rho)
    n_k = size(kv, 1); n_pt = size(pt, 1)
    fill!(re, 0.0); fill!(im, 0.0)
    @inbounds for i in 1:n_pt
        px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
        @simd for j in 1:n_k
            x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
            sn, cs = sincos_real(x)
            re[j] += r*cs
            im[j] += r*sn
        end
    end
end

if HAVE_TURBO
    @eval function version_T!(re, im, kv, pt, rho)
        n_k = size(kv, 1); n_pt = size(pt, 1)
        fill!(re, 0.0); fill!(im, 0.0)
        for i in 1:n_pt
            px = pt[i,1]; py = pt[i,2]; pz = pt[i,3]; r = rho[i]
            @turbo for j in 1:n_k
                x = kv[j,1]*px + kv[j,2]*py + kv[j,3]*pz
                re[j] += r*cos(x)
                im[j] += r*sin(x)
            end
        end
    end
end

function check_sincos()
    es = 0.0; ec = 0.0
    for ii in 1:1_000_000
        x = -200.0 + 400.0*(ii - 0.5)/1_000_000
        for fn in (sincos_poly, sincos_real)
            s, c = fn(x)
            es = max(es, abs(s - sin(x))); ec = max(ec, abs(c - cos(x)))
        end
    end
    @printf("E and F sin/cos against Julia's, |x| <= 200: sin within %.2f, cos within %.2f epsilon\n",
            es/eps(Float64), ec/eps(Float64))
end

function main()
    n_k  = length(ARGS) >= 1 ? parse(Int, ARGS[1]) : 8000
    n_pt = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 6000
    # k up to about 10 bohr^-1; points within 6 bohr; a positive density
    rng = MersenneTwister(12345)
    kv  = 20.0 .* (rand(rng, n_k, 3) .- 0.5)
    pt  = 12.0 .* (rand(rng, n_pt, 3) .- 0.5)
    rho = 1.0e-3 .* rand(rng, n_pt)
    fA = zeros(ComplexF64, n_k); re = zeros(n_k); im = zeros(n_k)
    terms = Float64(n_k)*n_pt

    # a small first call of everything, so that compilation is not timed
    ks = kv[1:8,:]; ps = pt[1:8,:]; rs = rho[1:8]; r8 = zeros(8); i8 = zeros(8)
    version_A!(zeros(ComplexF64, 8), ks, ps, rs)
    swapped = Any[("D   loops swapped, sin and cos         ", version_D!),
                  ("Ds  as D with @simd                    ", version_Ds!),
                  ("Dc  as D with sincos                   ", version_Dc!),
                  ("E   as D, own sin/cos, integer rounding", version_E!),
                  ("F   as E, real arithmetic only         ", version_F!)]
    HAVE_TURBO && push!(swapped, ("T   as D under @turbo                  ", version_T!))
    for (_, fn) in swapped
        Base.invokelatest(fn, r8, i8, ks, ps, rs)
    end

    @printf("Julia %s, %s; n_k = %d, n_pt = %d, %.2e terms\n", VERSION, Sys.CPU_NAME, n_k, n_pt, terms)
    println("                                          seconds   ns/term   largest rel. diff from A")
    t = @elapsed version_A!(fA, kv, pt, rho)
    @printf("%s%10.3f%10.3f%18.2e\n", "A   k outside, complex exponential     ", t, 1e9*t/terms, 0.0)
    scale = maximum(abs, fA)
    for (label, fn) in swapped
        t = @elapsed Base.invokelatest(fn, re, im, kv, pt, rho)
        @printf("%s%10.3f%10.3f%18.2e\n", label, t, 1e9*t/terms, maximum(abs, complex.(re, im) .- fA)/scale)
    end
    HAVE_TURBO || println("T   not run: LoopVectorization is not installed")
    check_sincos()
end

main()
