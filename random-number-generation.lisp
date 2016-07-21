;;;; sbcl-gsl/random-number-generation.lisp
;;;;
;;;; The library provides a large collection of random number generations which
;;;; can be accessed through a uniform interface. Environment variables allow
;;;; you to select different generators and seeds at runtime, so that you can
;;;; easily switch between generators without needing to recompile your program.

;;;; Copyright (C) 2016 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see http://www.gnu.org/licenses/.

(cl:defpackage "GSL-RNG"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL")
  (:export "GSL-RNG-TYPE"
           "GSL-RNG"
           "GSL-RNG-BOROSH13"
           "GSL-RNG-COVEYOU"
           "GSL-RNG-CMRG"
           "GSL-RNG-FISHMAN18"
           "GSL-RNG-FISHMAN20"
           "GSL-RNG-FISHMAN2X"
           "GSL-RNG-GFSR4"
           "GSL-RNG-KNUTHRAN"
           "GSL-RNG-KNUTHRAN2"
           "GSL-RNG-KNUTHRAN2002"
           "GSL-RNG-LECUYER21"
           "GSL-RNG-MINSTD"
           "GSL-RNG-MRG"
           "GSL-RNG-MT19937"
           "GSL-RNG-MT19937_1999"
           "GSL-RNG-MT19937_1998"
           "GSL-RNG-R250"
           "GSL-RNG-RAN0"
           "GSL-RNG-RAN1"
           "GSL-RNG-RAN2"
           "GSL-RNG-RAN3"
           "GSL-RNG-RAND"
           "GSL-RNG-RAND48"
           "GSL-RNG-RANDOM128_BSD"
           "GSL-RNG-RANDOM128_GLIBC2"
           "GSL-RNG-RANDOM128_LIBC5"
           "GSL-RNG-RANDOM256_BSD"
           "GSL-RNG-RANDOM256_GLIBC2"
           "GSL-RNG-RANDOM256_LIBC5"
           "GSL-RNG-RANDOM32_BSD"
           "GSL-RNG-RANDOM32_GLIBC2"
           "GSL-RNG-RANDOM32_LIBC5"
           "GSL-RNG-RANDOM64_BSD"
           "GSL-RNG-RANDOM64_GLIBC2"
           "GSL-RNG-RANDOM64_LIBC5"
           "GSL-RNG-RANDOM8_BSD"
           "GSL-RNG-RANDOM8_GLIBC2"
           "GSL-RNG-RANDOM8_LIBC5"
           "GSL-RNG-RANDOM_BSD"
           "GSL-RNG-RANDOM_GLIBC2"
           "GSL-RNG-RANDOM_LIBC5"
           "GSL-RNG-RANDU"
           "GSL-RNG-RANF"
           "GSL-RNG-RANLUX"
           "GSL-RNG-RANLUX389"
           "GSL-RNG-RANLXD1"
           "GSL-RNG-RANLXD2"
           "GSL-RNG-RANLXS0"
           "GSL-RNG-RANLXS1"
           "GSL-RNG-RANLXS2"
           "GSL-RNG-RANMAR"
           "GSL-RNG-SLATEC"
           "GSL-RNG-TAUS"
           "GSL-RNG-TAUS2"
           "GSL-RNG-TAUS113"
           "GSL-RNG-TRANSPUTER"
           "GSL-RNG-TT800"
           "GSL-RNG-UNI"
           "GSL-RNG-UNI32"
           "GSL-RNG-VAX"
           "GSL-RNG-WATERMAN14"
           "GSL-RNG-ZUF"
           "GSL-RNG-DEFAULT"
           "GSL-RNG-DEFAULT-SEED"
           "GSL-RNG-ALLOC"
           "GSL-RNG-SET"
           "GSL-RNG-FREE"
           "GSL-RNG-GET"
           "GSL-RNG-UNIFORM"
           "GSL-RNG-UNIFORM-POS"
           "GSL-RNG-UNIFORM-INT"
           "GSL-RNG-NAME"
           "GSL-RNG-MAX"
           "GSL-RNG-MIN"
           "GSL-RNG-STATE"
           "GSL-RNG-SIZE"
           "GSL-RNG-TYPES-SETUP"
           "GSL-RNG-ENV-SETUP"
           "GSL-RNG-MEMCPY"
           "GSL-RNG-CLONE"))

(cl:in-package "GSL-RNG")

;;; (struct gsl-rng-type)
;;; (struct gsl-rng)
;;;   The random number generator library use two special structs, gsl-rng-type which
;;;   holds static information about each type of generator and gsl-rng wihch describes
;;;   an instance of generator created from a given gsl-rng-type.
(define-alien-type nil
    (struct gsl-rng-type
            (name (* char))
            (max unsigned-long)
            (min unsigned-long)
            (size size-t)
            (set (* (function void (* t) unsigned-long)))
            (get (* (function unsigned-long (* t))))
            (get_double (* (function double (* t))))))

(define-alien-type nil
    (struct gsl-rng
            (type (* (struct gsl-rng-type)))
            (state (* t))))

;;; The library provides a large number of generators of different types, including
;;; simulation quality generators, generators provided for compatibility with other
;;; libraries and historical generators from the past.
(define-alien-variable gsl-rng-borosh13 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-coveyou (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-cmrg (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-fishman18 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-fishman20 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-fishman2x (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-gfsr4 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-knuthran (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-knuthran2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-knuthran2002 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-lecuyer21 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-minstd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-mrg (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-mt19937 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-mt19937_1999 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-mt19937_1998 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-r250 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ran0 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ran1 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ran2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ran3 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-rand (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-rand48 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random128_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random128_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random128_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random256_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random256_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random256_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random32_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random32_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random32_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random64_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random64_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random64_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random8_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random8_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random8_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random_bsd (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random_glibc2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-random_libc5 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-randu (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranf (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlux (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlux389 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlxd1 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlxd2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlxs0 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlxs1 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranlxs2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-ranmar (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-slatec (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-taus (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-taus2 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-taus113 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-transputer (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-tt800 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-uni (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-uni32 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-vax (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-waterman14 (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-zuf (* (struct gsl-rng-type)))

;;; gsl-rng-default
;;; gsl-rng-default-seed
(define-alien-variable gsl-rng-default (* (struct gsl-rng-type)))
(define-alien-variable gsl-rng-default-seed unsigned-long)

;;; (gsl-rng-alloc type)
;;;   This function returns a pointer to a newly-created instance of a random number
;;;   generator of type type.
(define-alien-routine gsl-rng-alloc
    (* (struct gsl-rng))
  (type (* (struct gsl-rng-type))))

;;; (gsl-rng-set r)
;;;   This function initialized (or 'seeds') the random number generator. If the generator
;;;   is seeded with the same value of s on two different runs, the same stream of random
;;;   numbers will be generated by successive calls to the routines below. If different
;;;   values of s >= 1 are supplied, then the generated streams of random numbers should
;;;   be completely different. If the seed s is zero then the standard seed from the original
;;;   implementation is used instead.
(define-alien-routine gsl-rng-set
    void
  (r (* (struct gsl-rng)))
  (s unsigned-long))

;;; (gsl-rng-free r)
;;;   This function frees all the memory associated with the generator r.
(define-alien-routine gsl-rng-free
    void
  (r (* (struct gsl-rng))))

;;; (gsl-rng-get r)
;;;   This function returns a random integer from the generator r. The minimum and
;;;   maximum values depend on the algorithm used, but all integers in the range [min,max]
;;;   are equally likely. The values of min and max can be determined using the auxiliary
;;;   functions (gsl-rng-max r) and (gsl-rng-min r).
(define-alien-routine gsl-rng-get
    unsigned-long
  (r (* (struct gsl-rng))))

;;; (gsl-rng-uniform r)
;;;   This function retruns a double precision floating point number uniformly distributed
;;;   in the range [0,1). The range includes 0.0 but excludes 1.0. The value is typically
;;;   obtained by dividing the result of (gsl-rng-get r) by (+ (gsl-rng-max r) 1.0d0) in
;;;   double precision. Some generators compute this ratio internally so that they can provide
;;;   floating point numbers with more than 32 bits of randomness (the maximum number
;;;   of bits that can be portably represented in a single unsigned-log)
(define-alien-routine gsl-rng-uniform
    double
  (r (* (struct gsl-rng))))

;;; (gsl-rng-uniform-pos r)
;;;   This function returns a positive double precision floating point number uniformly
;;;   distributed in the range (0,1), excluding both 0.0 and 1.0. The number is obtained
;;;   by sampling the generator with the algorithm of gsl-rng-uniform until a non-zero
;;;   value is obtained.
(define-alien-routine gsl-rng-uniform-pos
    double
  (r (* (struct gsl-rng))))

;;; (gsl-rng-uniform-int r n)
;;;   This functions a random integer from 0 to n-1 inclusive by scaling down
;;;   and/or discarding samples from the generator r. All integers in range [0,n-1]
;;;   are produced with equal probability. For generators with a non-zero minimum value
;;;   an offset is applied so that zero is returned with the correct probability.
(define-alien-routine gsl-rng-uniform-int
    unsigned-long
  (r (* (struct gsl-rng)))
  (n unsigned-long))

;;; (gsl-rng-name r)
;;;   This function return a pointer to the name of the generator.
(define-alien-routine gsl-rng-name
    c-string
  (r (* (struct gsl-rng))))

;;; (gsl-rng-max r)
;;;   gsl-rng-max returns the largest value that gsl-rng-get can return.
(define-alien-routine gsl-rng-max
    unsigned-long
  (r (* (struct gsl-rng))))

;;; (gsl-rng-min r)
;;;   gsl-rng-min returns the smallest value that gsl-rng-get can return. Usually this
;;;   value is zero. There are smallest generators with algorithms that cannot return zero,
;;;   and for these generators the minimum value is 1.
(define-alien-routine gsl-rng-min
    unsigned-long
  (r (* (struct gsl-rng))))

;;; (gsl-rng-state r)
;;; (gsl-rng-size r)
;;;   These functions return a pointer to the state of generator r and its size. You can
;;;   use this information to access the state directly.
(define-alien-routine gsl-rng-state
    (* t)
  (r (* (struct gsl-rng))))

(define-alien-routine gsl-rng-size
    size-t
  (r (* (struct gsl-rng))))

;;; (gsl-rng-types-setup)
;;;   This function return a pointer to an array of all the available generator types,
;;;   terminated by a null pointer. The function should be called once at the start of the
;;;   program, if needed.
(define-alien-routine gsl-rng-types-setup
    (array (* (struct gsl-rng-type)) nil))

;;; (gsl-rng-env-setup)
;;;   This function reads the environment variables GSL_RNG_TYPE and GSL_RNG_SEED and
;;;   uses their values to set the corresponding library variables gsl-rng-default and
;;;   gsl-rng-default-seed. These globa variables are defined as follows,
;;;
;;;     (gsl-rng-default (* (struct gsl-rng-type)))
;;;     (gsl-rng-default-seed unsigned-long)
;;;
;;;   The environment variable GSL_RNG_TYPE should be the name of a generator, such
;;;   as taus or mt19937. The environment variable GSL_RNG_SEED should contain the
;;;   desired seed value. It is converted to an unsigned-long using the C library
;;;   function strtoul.
;;;   If you don't specify a generator for GSL_RNG_TYPE then gsl-rng-mt19937 is used as
;;;   the default. The initial value of gsl-rng-default-seed is zero.
(define-alien-routine gsl-rng-env-setup
    (* (struct gsl-rng-type)))

;;; (gsl-rng-memcpy dest src)
;;;   This function copies the random number generator src into the pre-existing generator
;;;   dest, making dest into an exact copy of src. The two generators must be of the same
;;;   type.
(define-alien-routine gsl-rng-memcpy
    int
  (dest (* (struct gsl-rng)))
  (src (* (struct gsl-rng))))

;;; (gsl-rng-clone r)
;;;   This function returns a pointer to a newly created generator which is an exact copy
;;;   of the generator r.
(define-alien-routine gsl-rng-clone
    (* (struct gsl-rng))
  (r (* (struct gsl-rng))))
