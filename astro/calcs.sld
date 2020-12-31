;;; Astro Calcs Library
;; Provides astronomical calculations
;; Formulas and tables from:
;; Reda, I and Andreas, A. Solar Position Algorithm for Solar Radiation Applications (January 2008).
;;                         Golden, CO: National Renewable Energy Laboratory.
(define-library (astro calcs)
  (import (scheme base)
          (scheme inexact)
          (scheme cxr)
          (astro angles))
  (export calculate-heliocentric-latitude calculate-heliocentric-longitude
          calculate-heliocentric-radius-vector
          calculate-geocentric-latitude calculate-geocentric-longitude
          nutation-longitude nutation-obliquity ecliptic-true-obliquity)

  (begin
    ;; Table Reduction to Calculate Heliocentric Longitude, Latitude, and Radius Vector
    ;; Reduces tables to calculate the Heliocentric Longitude, Latitude, and Radius Vector for a
    ;; Julian Ephemereis Millenium, epoch 2000
    ;; Usage: (reduce-table table julian-ephemeris-millenium) -> result
    ;; table -- a table of earth periodic terms
    ;; julian-ephemeris-millenium -- the Julian Ephemeris Millenium to calculate the result for,
    ;;                               epoch 2000
    ;; result -- the result of the table reduction
    (define (reduce-table table julian-ephemeris-millenium)
      (apply +
             (map (lambda (x)
                    (apply (lambda (a b c)
                             (* a (cos (+ b (* julian-ephemeris-millenium c)))))
                           x))
                  table)))

    ;; Heliocentric Term Calculation
    ;; Calculate a heliocentric term from a list of periodic terms, epoch 2000
    ;; Usage: (calculate-heliocentric-term term-list julian-ephemeris-millenium degrees?) -> result
    ;; term-list -- a list of periodic terms
    ;; julian-ephemeris-millenium -- the Julian Ephemeris Millenium to calculate the result for,
    ;;                               epoch 2000
    ;; degrees? -- a boolean, whether or not to return the result in degrees
    ;; result -- the result of the calculation, in degrees or AU
    (define (calculate-heliocentric-term term-list julian-eph-millenium degrees?)
      (let kernel ((sum (car term-list))
                   (terms (cdr term-list))
                   (counter 1))
        (if (not (null? terms))
            (kernel (+ sum (* (car terms) (expt julian-eph-millenium counter)))
                    (cdr terms)
                    (+ counter 1))
            (let ((result (/ sum (expt 10 8))))
              (if degrees?
                  (let ((result (radians->degrees result)))
                    result)
                  result)))))

    ;; Heliocentric Longitude Calculation
    ;; Calculate the heliocentric longitude of the earth, epoch 2000
    ;; Usage: (calculate-heliocentric-longitude julian-eph-millenium) -> result
    ;; julian-eph-millenium -- the Julian Ephemeris Millenium to calculate the result for, epoch 2000
    ;; result -- the heliocentric longitude of the earth, in degrees
    (define (calculate-heliocentric-longitude julian-eph-millenium)
      (reduce-to-360 (calculate-heliocentric-term (list (reduce-table l0-table julian-eph-millenium)
                                                        (reduce-table l1-table julian-eph-millenium)
                                                        (reduce-table l2-table julian-eph-millenium)
                                                        (reduce-table l3-table julian-eph-millenium)
                                                        (reduce-table l4-table julian-eph-millenium)
                                                        (reduce-table l5-table julian-eph-millenium))
                                                  julian-eph-millenium #t)))

    ;; Heliocentric Latitude Calculation
    ;; Calculate the heliocentric longitude of the earth, epoch 2000
    ;; Usage: (calculate-heliocentric-latitude julian-eph-millenium) -> result
    ;; julian-eph-millenium -- the Julian Ephemeris Millenium to calculate the result for, epoch 2000
    ;; result -- the heliocentric latitude of the earth, in degrees
    (define (calculate-heliocentric-latitude julian-eph-millenium)
      (calculate-heliocentric-term (list (reduce-table b0-table julian-eph-millenium)
                                         (reduce-table b1-table julian-eph-millenium))
                                   julian-eph-millenium #t))

    ;; Heliocentric Radius Vector Calculation
    ;; Calculate the heliocentric radius vector of the earth, epoch 2000
    ;; Usage: (calculate-heliocentric-radius-vector julian-eph-millenium) -> result
    ;; julian-eph-millenium -- the Julian Ephemeris Millenium to calculate the result for, epoch 2000
    ;; result -- the heliocentric radius vector of the earth, in AU
    (define (calculate-heliocentric-radius-vector julian-eph-millenium)
      (calculate-heliocentric-term (list (reduce-table r0-table julian-eph-millenium)
                                         (reduce-table r1-table julian-eph-millenium)
                                         (reduce-table r2-table julian-eph-millenium)
                                         (reduce-table r3-table julian-eph-millenium)
                                         (reduce-table r4-table julian-eph-millenium))
                                   julian-eph-millenium #f))

    ;; Geocentric Longitude Conversion
    ;; Convert a heliocentric longitude of earth to a geocentric longitude of the sun
    ;; Usage: (calculate-geocentric-longitude heliocentric-longitude) -> geocentric-longitude
    ;; heliocentric-longitude -- the heliocentric longitude of the earth to convert
    ;; geocentric-longtidue -- the geocentric longitude of the sun
    (define (calculate-geocentric-longitude heliocentric-longitude)
      (let ((result (+ heliocentric-longitude 180)))
        (reduce-to-360 result)))

    ;; Geocentric Latitude Conversion
    ;; Convert a heliocentric latitude of earth to a geocentric latitude of the sun
    ;; Usage: (calculate-geocentric-latitude heliocentric-latitude) -> geocentric-latitude
    ;; heliocentric-latitude -- the heliocentric latitude of the earth to convert
    ;; geocentric-latitude -- the geocentric latitude of the sun
    (define (calculate-geocentric-latitude heliocentric-latitude)
      (- heliocentric-latitude))

    ;; Nutation Coefficient Generic Calculation
    ;; Calculate a nutation coefficient for a given Julian Ephemeris Century, epoch 2000
    ;; Usage: (coeffs a b c d julian-ephemeris-century) -> coefficent
    ;; a -- calculation coefficient
    ;; b -- calculation coefficient
    ;; c -- calculation coefficient
    ;; d -- calculation coefficient
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    (define (coeffs a b c d julian-ephemeris-century)
      (+ a
         (* b julian-ephemeris-century)
         (* c (expt julian-ephemeris-century 2))
         (/ (expt julian-ephemeris-century 3) d)))

    ;; Mean Elongation of Moon from Sun
    ;; Calculate the mean elongation of Moon from Sun nutation coefficient for a given Julian Ephemeris
    ;; Century, epoch 2000
    ;; Usage: (x0 julian-ephemeris-century) -> mean-elongation
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; mean-elongation -- the mean elongation of Moon from the Sun, in degrees
    (define (x0 julian-ephemeris-century)
      (coeffs 297.85036 445267.111480 -0.0019142 189474 julian-ephemeris-century))

    ;; Mean Anomaly of Sun
    ;; Calculate the mean anomaly of Sun nutation coefficient for a given Julian Ephemeris Century,
    ;; epoch 2000
    ;; Usage: (x1 julian-ephemeris-century) -> mean-anomaly
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; mean-elongation -- the mean anomaly of Sun, in degrees
    (define (x1 julian-ephemeris-century)
      (coeffs 357.52772 35999.050340 -0.0001603 -300000 julian-ephemeris-century))

    ;; Mean Anomaly of Moon
    ;; Calculate the mean anomaly of Moon nutation coefficient for a given Julian Ephemeris Century,
    ;; epoch 2000
    ;; Usage: (x2 julian-ephemeris-century) -> mean-anomaly
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; mean-elongation -- the mean anomaly of Moon, in degrees
    (define (x2 julian-ephemeris-century)
      (coeffs 134.96298 477198.867398 0.0086972 56250 julian-ephemeris-century))

    ;; Moon Argument of Latitude
    ;; Calculate the Moon argument of latitude nutation coefficient for a given Julian Ephemeris
    ;; Century, epoch 2000
    ;; Usage: (x3 julian-ephemeris-century) -> arg-latitude
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; arg-latitude -- the Moon argument of latitude, in degrees
    (define (x3 julian-ephemeris-century)
      (coeffs 93.27191 483202.017538 -0.0036825 327270 julian-ephemeris-century))

    ;; Moon Longitude of Ascending Node
    ;; Calculate the Moon longitude of the ascending node nutation coefficient for a given Julian
    ;; Ephemeris Century, epoch 2000, using the mean ecliptic orbit and the mean vernal equinox
    ;; Usage: (x4 julian-ephemeris-century) -> longitude-an
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; longitude-an -- the Moon longitude of the ascending node, in degrees
    (define (x4 julian-ephemeris-century)
      (coeffs 125.04452 -1934.136261 0.0020708 450000 julian-ephemeris-century))

    ;; All Nutation Coefficients
    ;; Calculate all nutation coefficients, in radians
    ;; Usage: (all-nutation-coeffs julian-ephemeris-century) -> coeffs
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculations, epoch 2000
    ;; coeffs -- a list of the nutation coefficients, in radians
    (define (all-nutation-coeffs julian-ephemeris-century)
      (map degrees->radians
           (list (x0 julian-ephemeris-century)
                 (x1 julian-ephemeris-century)
                 (x2 julian-ephemeris-century)
                 (x3 julian-ephemeris-century)
                 (x4 julian-ephemeris-century))))

    ;; Nutation Calculation
    ;; Generic function to calculate nutation
    ;; Usage: (nutation coeffs table julian-ephemeris-century type) -> result
    ;; coeffs -- a list of the nutation coefficients, in radians
    ;; table -- the nutation calculation table to reduce
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; type -- either 'longitude or 'obliquity, what coordinate of nutation to calculate
    (define (nutation coeffs table julian-ephemeris-century type)
      (/ (if (eq? type 'longitude)
             (apply +
                    (map (lambda (x)
                           (* (+ (caadr x) (* (cadadr x) julian-ephemeris-century))
                              (sin (apply + (map * coeffs (car x))))))
                         table))
             (if (eq? type 'obliquity)
                 (apply +
                        (map (lambda (x)
                               (* (+ (caaddr x) (* (cadr (caddr x)) julian-ephemeris-century))
                                  (cos (apply + (map * coeffs (car x))))))
                             table))
                 (error (string-append "Invalid nutation coordinate type,"
                                       "please use either 'longitude or 'obliquity"))))
         36000000))

    ;; Nutation in Longitude
    ;; Calculate the nutation in longitude, epoch 2000
    ;; Usage: (nutation-longitude julian-ephemeris-century) -> nutation-longitude
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; nutation-longitude -- the nutation longitude, in degrees
    (define (nutation-longitude julian-ephemeris-century)
      (nutation (all-nutation-coeffs julian-ephemeris-century)
                nutation-table
                julian-ephemeris-century
                'longitude))
    
    ;; Nutation in Obliquity
    ;; Calculate the nutation in obliquity, epoch 2000
    ;; Usage: (nutation-obliquity julian-ephemeris-century) -> nutation-obliquity
    ;; julian-ephemeris-century -- the Julian Ephemeris Century of the calculation, epoch 2000
    ;; nutation-obliquity -- the nutation obliquity, in degrees
    (define (nutation-obliquity julian-ephemeris-century)
      (nutation (all-nutation-coeffs julian-ephemeris-century)
                nutation-table
                julian-ephemeris-century
                'obliquity))

    ;; True Obliquity of the Ecliptic
    ;; Calculate the true obliquity of the ecliptic, epoch 2000
    ;; Usage: (ecliptic-true-obliquity julian-ephemeris-millenium nutation-obliquity) -> obliquity
    ;; julian-ephemeris-millenium -- the Julian Ephemeris Millenium of the calculation, epoch 2000
    ;; nutation-obliquity -- the nutation obliquity, in degrees
    ;; obliquity -- the true obliquity of the ecliptic, in degrees
    (define (ecliptic-true-obliquity julian-ephemeris-millenium nutation-obliquity)
      (let* ((u (/ julian-ephemeris-millenium 10))
             (mean-obliquity (+ 84381.448
                                (* -4680.93 u)
                                (* -1.55 (expt u 2))
                                (* 1999.25 (expt u 3))
                                (* -51.38 (expt u 4))
                                (* -249.67 (expt u 5))
                                (* -39.05 (expt u 6))
                                (* 7.12 (expt u 7))
                                (* 27.87 (expt u 8))
                                (* 5.79 (expt u 9))
                                (* 2.45 (expt u 10)))))
        (+ (/ mean-obliquity 3600) nutation-obliquity)))

    ;; Calculation Tables
    ;; Tables for Earth Periodic Terms
    ;; Heliocentric Longitude
    (define l0-table
      '((175347046 0 0)
        (3341656 4.6692568 6283.07585)
        (34894 4.6261 12566.1517)
        (3497 2.7441 5753.3849)
        (3418 2.8289 3.5231)
        (3136 3.6277 77713.7715)
        (2676 4.4181 7860.4194)
        (2343 6.1352 3930.2097)
        (1324 0.7425 11506.7698)
        (1273 2.0371 529.691)
        (1199 1.1096 1577.3435)
        (990 5.233 5884.927)
        (902 2.045 26.298)
        (857 3.508 398.149)
        (780 1.179 5223.694)
        (753 2.533 5507.553)
        (505 4.583 18849.228)
        (492 4.205 775.523)
        (357 2.92 0.067)
        (317 5.849 11790.629)
        (284 1.899 796.298)
        (271 0.315 10977.079)
        (243 0.345 5486.778)
        (206 4.806 2544.314)
        (205 1.869 5573.143)
        (202 2.458 6069.777)
        (156 0.833 213.299)
        (132 3.411 2942.463)
        (126 1.083 20.775)
        (115 0.645 0.98)
        (103 0.636 4694.003)
        (102 0.976 15720.839)
        (102 4.267 7.114)
        (99 6.21 2146.17)
        (98 0.68 155.42)
        (86 5.98 161000.69)
        (85 1.3 6275.96)
        (85 3.67 71430.7)
        (80 1.81 17260.15)
        (79 3.04 12036.46)
        (75 1.76 5088.63)
        (74 3.5 3154.69)
        (74 4.68 801.82)
        (70 0.83 9437.76)
        (62 3.98 8827.39)
        (61 1.82 7084.9)
        (57 2.78 6286.6)
        (56 4.39 14143.5)
        (56 3.47 6279.55)
        (52 0.19 12139.55)
        (52 1.33 1748.02)
        (51 0.28 5856.48)
        (49 0.49 1194.45)
        (41 5.37 8429.24)
        (41 2.4 19651.05)
        (39 6.17 10447.39)
        (37 6.04 10213.29)
        (37 2.57 1059.38)
        (36 1.71 2352.87)
        (36 1.78 6812.77)
        (33 0.59 17789.85)
        (30 0.44 83996.85)
        (30 2.74 1349.87)
        (25 3.16 4690.48)))

    (define l1-table
      '((628331966747 0 0)
        (206059 2.678235 6283.07585)
        (4303 2.6351 12566.1517)
        (425 1.59 3.523)
        (119 5.796 26.298)
        (109 2.966 1577.344)
        (93 2.59 18849.23)
        (72 1.14 529.69)
        (68 1.87 398.15)
        (67 4.41 5507.55)
        (59 2.89 5223.69)
        (56 2.17 155.42)
        (45 0.4 796.3)
        (36 0.47 775.52)
        (29 2.65 7.11)
        (21 5.34 0.98)
        (19 1.85 5486.78)
        (19 4.97 213.3)
        (17 2.99 6275.96)
        (16 0.03 2544.31)
        (16 1.43 2146.17)
        (15 1.21 10977.08)
        (12 2.83 1748.02)
        (12 3.26 5088.63)
        (12 5.27 1194.45)
        (12 2.08 4694)
        (11 0.77 553.57)
        (10 1.3 6286.6)
        (10 4.24 1349.87)
        (9 2.7 242.73)
        (9 5.64 951.72)
        (8 5.3 2352.87)
        (6 2.65 9437.76)
        (6 4.67 4690.48)))

    (define l2-table
      '((52919 0 0)
        (8720 1.0721 6283.0758)
        (309 0.867 12566.152)
        (27 0.05 3.52)
        (16 5.19 26.3)
        (16 3.68 155.42)
        (10 0.76 18849.23)
        (9 2.06 77713.77)
        (7 0.83 775.52)
        (5 4.66 1577.34)
        (4 1.03 7.11)
        (4 3.44 5573.14)
        (3 5.14 796.3)
        (3 6.05 5507.55)
        (3 1.19 242.73)
        (3 6.12 529.69)
        (3 0.31 398.15)
        (3 2.28 553.57)
        (2 4.38 5223.69)
        (2 3.75 0.98)))

    (define l3-table
      '((289 5.844 6283.076)
        (35 0 0)
        (17 5.49 12566.15)
        (3 5.2 155.42)
        (1 4.72 3.52)
        (1 5.3 18849.23)
        (1 5.97 242.73)))

    (define l4-table
      '((114 3.142 0)
        (8 4.13 6283.08)
        (1 3.84 12566.15)))

    (define l5-table
      '((1 3.14 0)))

    ;; Heliocentric Latitude
    (define b0-table
      '((280 3.199 84334.662)
        (102 5.422 5507.553)
        (80 3.88 5223.69)
        (44 3.7 2352.87)
        (32 4 1577.34)))

    (define b1-table
      '((9 3.9 5507.55)
        (6 1.73 5223.69)))

    ;; Heliocentric Radius Vector
    (define r0-table
      '((100013989 0 0)
        (1670700 3.0984635 6283.07585)
        (13956 3.05525 12566.1517)
        (3084 5.1985 77713.7715)
        (1628 1.1739 5753.3849)
        (1576 2.8469 7860.4194)
        (925 5.453 11506.77)
        (542 4.564 3930.21)
        (472 3.661 5884.927)
        (346 0.964 5507.553)
        (329 5.9 5223.694)
        (307 0.299 5573.143)
        (243 4.273 11790.629)
        (212 5.847 1577.344)
        (186 5.022 10977.079)
        (175 3.012 18849.228)
        (110 5.055 5486.778)
        (98 0.89 6069.78)
        (86 5.69 15720.84)
        (86 1.27 161000.69)
        (65 0.27 17260.15)
        (63 0.92 529.69)
        (57 2.01 83996.85)
        (56 5.24 71430.7)
        (49 3.25 2544.31)
        (47 2.58 775.52)
        (45 5.54 9437.76)
        (43 6.01 6275.96)
        (39 5.36 4694)
        (38 2.39 8827.39)
        (37 0.83 19651.05)
        (37 4.9 12139.55)
        (36 1.67 12036.46)
        (35 1.84 2942.46)
        (33 0.24 7084.9)
        (32 0.18 5088.63)
        (32 1.78 398.15)
        (28 1.21 6286.6)
        (28 1.9 6279.55)
        (26 4.59 10447.39)))

    (define r1-table
      '((103019 1.10749 6283.07585)
        (1721 1.0644 12566.1517)
        (702 3.142 0)
        (32 1.02 18849.23)
        (31 2.84 5507.55)
        (25 1.32 5223.69)
        (18 1.42 1577.34)
        (10 5.91 10977.08)
        (9 1.42 6275.96)
        (9 0.27 5486.78)))

    (define r2-table
      '((4359 5.7846 6283.0758)
        (124 5.579 12566.152)
        (12 3.14 0)
        (9 3.63 77713.77)
        (6 1.87 5573.14)
        (3 5.47 18849.23)))

    (define r3-table
      '((145 4.273 6283.076)
        (7 3.92 12566.15)))

    (define r4-table
      '((4 2.56 6283.08)))

    ;; Nutation Table
    (define nutation-table
      '(((0 0 0 0 1) (-171996 -174.2) (92025 8.9))
        ((-2 0 0 2 2) (-13187 -1.6) (5736 -3.1))
        ((0 0 0 2 2) (-2274 -0.2) (977 -0.5))
        ((0 0 0 0 2) (2062 0.2) (-895 0.5))
        ((0 1 0 0 0) (1426 -3.4) (54 -0.1))
        ((0 0 1 0 0) (712 0.1) (-7 0))
        ((-2 1 0 2 2) (-517 1.2) (224 -0.6))
        ((0 0 0 2 1) (-386 -0.4) (200 0))
        ((0 0 1 2 2) (-301 0) (129 -0.1))
        ((-2 -1 0 2 2) (217 -0.5) (-95 0.3))
        ((-2 0 1 0 0) (-158 0) (0 0))
        ((-2 0 0 2 1) (129 0.1) (-70 0))
        ((0 0 -1 2 2) (123 0) (-53 0))
        ((2 0 0 0 0) (63 0) (0 0))
        ((0 0 1 0 1) (63 0.1) (-33 0))
        ((2 0 -1 2 2) (-59 0) (26 0))
        ((0 0 -1 0 1) (-58 -0.1) (32 0))
        ((0 0 1 2 1) (-51 0) (27 0))
        ((-2 0 2 0 0) (48 0) (0 0))
        ((0 0 -2 2 1) (46 0) (-24 0))
        ((2 0 0 2 2) (-38 0) (16 0))
        ((0 0 2 2 2) (-31 0) (13 0))
        ((0 0 2 0 0) (29 0) (0 0))
        ((-2 0 1 2 2) (29 0) (-12 0))
        ((0 0 0 2 0) (26 0) (0 0))
        ((-2 0 0 2 0) (-22 0) (0 0))
        ((0 0 -1 2 1) (21 0) (-10 0))
        ((0 2 0 0 0) (17 -0.1) (0 0))
        ((2 0 -1 0 1) (16 0) (-8 0))
        ((-2 2 0 2 2) (-16 0.1) (7 0))
        ((0 1 0 0 1) (-15 0) (9 0))
        ((-2 0 1 0 1) (-13 0) (7 0))
        ((0 -1 0 0 1) (-12 0) (6 0))
        ((0 0 2 -2 0) (11 0) (0 0))
        ((2 0 -1 2 1) (-10 0) (5 0))
        ((2 0 1 2 2) (-8 0) (3 0))
        ((0 1 0 2 2) (7 0) (-3 0))
        ((-2 1 1 0 0) (-7 0) (0 0))
        ((0 -1 0 2 2) (-7 0) (3 0))
        ((2 0 0 2 1) (-7 0) (3 0))
        ((2 0 1 0 0) (6 0) (0 0))
        ((-2 0 2 2 2) (6 0) (-3 0))
        ((-2 0 1 2 1) (6 0) (-3 0))
        ((2 0 -2 0 1) (-6 0) (3 0))
        ((2 0 0 0 1) (-6 0) (3 0))
        ((0 -1 1 0 0) (5 0) (0 0))
        ((-2 -1 0 2 1) (-5 0) (3 0))
        ((-2 0 0 0 1) (-5 0) (3 0))
        ((0 0 2 2 1) (-5 0) (3 0))
        ((-2 0 2 0 1) (4 0) (0 0))
        ((-2 1 0 2 1) (4 0) (0 0))
        ((0 0 1 -2 0) (4 0) (0 0))
        ((-1 0 1 0 0) (-4 0) (0 0))
        ((-2 1 0 0 0) (-4 0) (0 0))
        ((1 0 0 0 0) (-4 0) (0 0))
        ((0 0 1 2 0) (3 0) (0 0))
        ((0 0 -2 2 2) (-3 0) (0 0))
        ((-1 -1 1 0 0) (-3 0) (0 0))
        ((0 1 1 0 0) (-3 0) (0 0))
        ((0 -1 1 2 2) (-3 0) (0 0))
        ((2 -1 -1 2 2) (-3 0) (0 0))
        ((0 0 3 2 2) (-3 0) (0 0))
        ((2 -1 0 2 2) (-3 0) (0 0))))))
