;;; Astro Time Libary
;; Provides Astronomical Time Calculations
(define-library (astro time)
  (import (scheme base)
          (scheme inexact)
          (astro angles))
  (export julian-date julian-ephemeris-date julian-century julian-millenium
          greenwich-mean-sidereal-time greenwich-apparent-sidereal-time local-sidereal-time)

  (begin
    ;;; Julian Date Section
    ;; Julian Date Calculation
    ;; Calculate the Julian Date given a year, month, and date
    ;; Usage: (julian-day year month day) -> julian-date
    ;; year -- the year of the date to convert
    ;; month -- the month of the date to convert
    ;; day -- the day of the date to convert, can include fraction of day to convert a time
    ;; julian-date -- the converted julian date
    (define (julian-date year month day)
      (let ((month (if (< 2 month)
                       month
                       (+ month 12)))
            (year (if (< 2 month)
                      year
                      (- year 1))))
        (let* ((date-type (calendar-type year month (truncate day)))
               (b (if (eq? date-type 'gregorian)
                      (let ((a (truncate-quotient year 100)))
                        (+ 2 (- a) (truncate-quotient a 4)))
                      (if (eq? date-type 'julian)
                          0
                          (error "The calendar type of the date entered cannot be determined")))))
          (+ (truncate (* 365.25 (+ year 4716)))
             (truncate (* 30.6001 (+ month 1)))
             day
             b
             -1524.5))))

    ;; Calendar Type
    ;; Determine whether a date uses the Julian or the Gregorian Calendar
    ;; Usage: (calendar-type year month day) -> calendar-type
    ;; year -- the year of the date to test
    ;; month -- the month of the date to test
    ;; day -- the day of the date to test
    ;; calendar-type -- either 'julian or 'gregorian
    (define (calendar-type year month day)
      (if (> year 1582)
          'gregorian
          (if (and (= year 1582)
                   (> month 10))
              'gregorian
              (if (and (= year 1582)
                       (= month 10)
                       (>= day 15))
                  'gregorian
                  (if (and (= year 1582)
                           (= month 10)
                           (and (> day 4) (< day 15)))
                      (error "The dates between 10/4/1582 and 10/15/1582 do not exist")
                      'julian)))))

    ;; Julian Ephemeris Date Calculation
    ;; Calculate the Julian Ephemeris Date using the Julian Date and the difference between
    ;; terrestrial time and universal time
    ;; Usage: (julian-ephemeris-date julian-date delta-t) -> julian-date
    ;; julian-date -- a Julian Date
    ;; delta-t -- the difference between UTC and TT to use for the calculation
    ;; julian-date -- the Julian Date converted into a Julian Ephemeris Date
    (define (julian-ephemeris-date julian-date delta-t)
      (+ julian-date (/ delta-t 86400)))

    ;; Julian Century Calculation
    ;; Calculate the Julian Century of a Julian Date based on the 2000 epoch
    ;; Usage: (julian-century julian-date) -> julian-century
    ;; julian-date -- a Julian Date
    ;; julian-century -- the Julian Century, epoch 2000
    (define (julian-century julian-date)
      (/ (- julian-date 2451545) 36525))

    ;; Julian Millenium Calculation
    ;; Calculate the Julian Millenium of a Julian Date based on the 2000 epoch
    ;; Usage: (julian-millenium julian-date) -> julian-millenium
    ;; julian-date -- a Julian Date
    ;; julian-millenium -- a Julian Millenium, epoch 2000
    (define (julian-millenium julian-date)
      (/ (julian-century julian-date) 10))

    ;;; Sidereal Time Section
    ;; Greenwich Mean Sidereal Time Calculation
    ;; Calculate the Greenwich Mean Sidereal Time, in degrees
    ;; Usage: (greenwich-mean-sidereal-time julian-date) -> angle
    ;; julian-date -- a Julian Date
    ;; angle -- Greenwich Mean Sidereal Time: the angle between Greenwich's meridian and the mean
    ;;          position of the vernal equinox
    (define (greenwich-mean-sidereal-time julian-date)
      (let ((jc (julian-century julian-date)))
        (reduce-to-360 (+ 280.46061837
                          (* 360.98564736629 (- julian-date 2451545))
                          (* 0.000387933 (expt jc 2))
                          (/ (expt jc 3) -38710000)))))

    ;; Greenwich Apparent Sidereal Time Calculation
    ;; Calculate the Greenwich Apparent Sidereal Time, in degrees
    ;; Usage: (greenwich-apparent-sidereal-time julian-date
    ;;                                          nutation-longitude ecliptic-obliquity) -> angle
    ;; julian-date -- a Julian Date
    ;; nutation-longitude -- the nutation in longitude
    ;; ecliptic-obliquity -- the true obliquity of the ecliptic
    ;; angle -- Greenwich Apparent Sidereal Time: the angle between Greenwich's meridian and the
    ;;          apparent position of the vernal equinox
    (define (greenwich-apparent-sidereal-time julian-date nutation-longitude ecliptic-obliquity)
      (+ (greenwich-mean-sidereal-time julian-date)
         (* nutation-longitude
            (cos (degrees->radians ecliptic-obliquity)))))

    ;; Local Sidereal Time
    ;; Convert Greenwich Sidereal Time, in degrees, to Local Sidereal Time, in degrees
    ;; Usage: (local-sidereal-time greenwich-sidereal-time longitude) -> angle
    ;; greenwich-sidereal-time -- the Greenwich Sidereal Time, in degrees
    ;; longitude -- the longitude of the observing site, in degrees east of Greenwich
    ;; angle -- Local Sidereal Time, in degrees: the angle between the local meridian and the position
    ;;                                           of the vernal equinox
    (define (local-sidereal-time greenwich-sidereal-time longitude)
      (reduce-to-360 (+ greenwich-sidereal-time longitude)))))
