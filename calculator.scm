(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme cxr)
        (srfi 13)
        (astro angles)
        (astro time))

(begin
  ;; Function Definitions
  (define (read-catalog catalog)
    (let ((file (open-input-file catalog)))
      (let kernel ((next (peek-char file))
                   (lst '()))
        (if (not (eof-object? next))
            (let ((line (string-tokenize (read-line file))))
              (kernel (peek-char file) (cons line lst)))
            (begin
              (close-port file)
              (reverse lst))))))

  (define (make-number item)
    (if (string->number item)
        (string->number item)
        item))

  (define (reformat-catalog catalog)
    (map (lambda (catalog-line)
           (let ((rest (cdddr catalog-line)))
             (list (car catalog-line)
                   (list (make-number (car rest))
                         (make-number (cadr rest)))
                   (list (make-number (caddr rest))
                         (make-number (cadddr rest))))))
         catalog))

  (define (catalog->catalog-hms catalog)
    (let ((first (car catalog)))
      (cons (list (car first)
                  (list (caadr first)
                        (cadadr first)
                        "RA_Seconds")
                  (list (caaddr first)
                        (car (cdaddr first))
                        "Dec_Seconds"))
            (map (lambda (catalog-line)
                   (list (car catalog-line)
                         (hm->hms (cadr catalog-line))
                         (hm->hms (caddr catalog-line))))
                 (cdr catalog)))))

  (define (catalog-name object)
    (car object))

  (define (catalog-ra object)
    (cadr object))

  (define (catalog-dec object)
    (caddr object))
  
  (define (leap-year? year)
    (if (= (remainder year 4) 0)
        (if (not (= (remainder year 100) 0))
            #t
            (if (= (remainder year 400) 0)
                #t
                #f))
        #f))

  (define (make-year year)
    (let kernel ((days (if (leap-year? year)
                           366
                           365))
                 (lst '()))
      (if (> days 0)
          (kernel (- days 1) (cons days lst))
          (map (lambda (x)
                 (day-num->date year x))
               lst))))

  (define (add-day-of-week year)
    (map (lambda (x) (list x (day-of-week x))) year))

  (define (day-num->date year day-num)
    (let ((ordinal-months (if (leap-year? year)
                              '(31 60 91 121 152 182 213 244 274 305 335 366)
                              '(31 59 90 120 151 181 212 243 273 304 334 365))))
      (let kernel ((prev-month 0)
                   (cur-month (car ordinal-months))
                   (next-months (cdr ordinal-months))
                   (counter 1))
        (if (<= day-num cur-month)
            (list year counter (exact (- day-num prev-month)))
            (kernel cur-month (car next-months) (cdr next-months) (+ counter 1))))))

  (define (date->day-num date)
    (let ((y (year date))
          (m (month date))
          (d (day date)))
      (let ((m (if (< 2 m)
                   m
                   (+ m 12)))
            (y (if (< 2 m)
                   y
                   (- y 1))))
        (let ((i (if (leap-year? y)
                     2
                     3))
              (year-diff (if (> m 12)
                             (if (leap-year? y)
                                 -366
                                 -365)
                             0)))
          (+ d (* 30 (- m 1)) (floor (* 0.6 (+ m 1))) (- i) year-diff)))))
        
  (define (fractional-day observing-time timezone)
    (/ (+ timezone
          (car observing-time)
          (/ (cadr observing-time) 60)
          (/ (caddr observing-time) 3600))
       24))

  (define (observing-window obs-year start end)
    (let ((start-frac (lambda (tz) (fractional-day start tz)))
          (end-frac (lambda (tz) (fractional-day end tz))))
      (map (lambda (row)
             (let* ((date (car row))
                    (tz (cadr row)))
               (let ((year (year date))
                     (month (month date))
                     (day (day date)))
                 (let* ((start-date (list year month (+ day (start-frac tz))))
                        (end-date (list year month (+ day (end-frac tz)))))
                   (let* ((start-jd (apply julian-date start-date))
                          (end-jd (apply julian-date end-date))
                          (start-jde (julian-ephemeris-date start-jd (estimate-delta-t start-date)))
                          (end-jde (julian-ephemeris-date end-jd (estimate-delta-t end-date))))
                     (list date start-jde end-jde))))))
           obs-year)))

  (define (estimate-delta-t date)
    (let ((t (- (year date) 2000)))
      (+ 62.92
         (* 0.32217 t)
         (* 0.005589 (expt t 2)))))

  (define (year date)
    (car date))

  (define (month date)
    (cadr date))

  (define (day date)
    (caddr date))

  (define (convert-to-gast window)
    (map (lambda (x)
           (let ((date (car x))
                 (start (cadr x))
                 (end (caddr x)))
             (list date
                   (greenwich-apparent-sidereal-time
                    start
                    (nutation-longitude (julian-century start))
                    (ecliptic-true-obliquity (julian-millenium start)
                                             (nutation-obliquity (julian-century start))))
                   (greenwich-apparent-sidereal-time
                    end
                    (nutation-longitude (julian-century end))
                    (ecliptic-true-obliquity (julian-millenium end)
                                             (nutation-obliquity (julian-century end)))))))
         window))
  
  (define (convert-to-lst window longitude)
    (map (lambda (x)
           (let ((date (car x))
                 (start (cadr x))
                 (end (caddr x)))
             (list date
                   (hms+ '(12 0 0) (degrees->hms (local-sidereal-time start longitude)))
                   (hms+ '(12 0 0) (degrees->hms (local-sidereal-time end longitude))))))
         window))

  (define (day-of-week date)
    (let* ((k (day date))
           (month (month date))
           (m (if (> (- month 2) 0)
                  (- month 2)
                  (+ month 10)))
           (year (if (> (- month 2) 0)
                     (year date)
                     (- (year date) 1)))
           (c (quotient year 100))
           (y (- year (* c 100))))
      (let ((weekday (remainder (+ k
                                   (floor (- (* 2.6 m) 0.2))
                                   (* -2 c)
                                   y
                                   (floor (/ y 4))
                                   (floor (/ c 4)))
                                7)))
        (if (>= weekday 0)
            weekday
            (+ 7 weekday)))))
  
  (define (add-timezone year timezone)
    (let kernel ((year year)
                 (previous-month 1)
                 (sunday-counter 0)
                 (tz timezone)
                 (new-year '()))
      (if (null? year)
          (reverse new-year)
          (let ((date (caar year))
                (weekday (cadar year)))
            (if (= (month date) previous-month)
                (if (= weekday 0)
                    (cond ((and (= (month date) 3)
                                (= sunday-counter 1))
                           (kernel (cdr year)
                                   (month date)
                                   (+ sunday-counter 1)
                                   (+ tz 1)
                                   (cons (list date (+ tz 1)) new-year)))
                          ((and (= (month date) 11)
                                (= sunday-counter 0))
                           (kernel (cdr year)
                                   (month date)
                                   (+ sunday-counter 1)
                                   (- tz 1)
                                   (cons (list date (- tz 1)) new-year)))
                          (else (kernel (cdr year)
                                        (month date)
                                        (+ sunday-counter 1)
                                        tz
                                        (cons (list date tz) new-year))))
                    (kernel (cdr year)
                            (month date)
                            sunday-counter
                            tz
                            (cons (list date tz) new-year)))
                (kernel (cdr year)
                        (month date)
                        0
                        tz
                        (cons (list date tz) new-year)))))))

  (define (transit-in-window? ra window)
    (let ((window-start (cadr window))
          (window-end (caddr window)))
      (hms<= (hms- ra window-start)
             (hms- window-end window-start))))
 

  (define (list-dates ra year)
    (let kernel ((year year)
                 (date-list '()))
      (if (not (null? year))
          (if (transit-in-window? ra (car year))
              (kernel (cdr year) (cons (caar year) date-list))
              (kernel (cdr year) date-list))
          (reverse date-list))))

  (define (get-observing-dates catalog year)
    (map (lambda (x)
           (let ((name (catalog-name x))
                 (ra (catalog-ra x))
                 (dec (catalog-dec x)))
             (list name
                   ra
                   dec
                   (list-dates ra year))))
         catalog))

  (define (next-day date)
    (let ((y (year date)))
      (day-num->date y (+ (date->day-num date) 1))))

  (define (date-list->date-range date-list)
    (let kernel ((first-day (car date-list))
                 (other-days (cdr date-list))
                 (current-range (list (car date-list) (car date-list)))
                 (date-range '()))
      (if (not (null? other-days))
          (let ((next (car other-days)))
            (if (= (apply julian-date (next-day first-day))
                   (apply julian-date next))
                (kernel next (cdr other-days) (list (car current-range) next) date-range)
                (kernel next (cdr other-days) (list next next) (cons current-range date-range))))
          (reverse (cons current-range date-range)))))

  (define (build-catalog-windows catalog)
    (map (lambda (x)
           (list (catalog-name x)
                 (catalog-ra x)
                 (catalog-dec x)
                 (date-list->date-range (cadddr x))))
         catalog))
  
  (define (observing-windows-to-string-list window-list)
    (let kernel ((window-list window-list)
                 (string-list '()))
      (if (not (null? window-list))
          (let ((start-date (caar window-list))
                (end-date (cadar window-list)))
            (let ((start-year (number->string (year start-date)))
                  (start-month (number->string (month start-date)))
                  (start-day (number->string (day start-date)))
                  (end-year (number->string (year end-date)))
                  (end-month (number->string (month end-date)))
                  (end-day (number->string (day end-date))))
              (kernel (cdr window-list)
                      (cons (string-concatenate (list start-year "-" start-month "-" start-day
                                                      " to " end-year "-" end-month "-" end-day))
                            string-list))))
          string-list)))

  (define (string-list-builder string-list)
    (let kernel ((string-list (cdr string-list))
                 (new-list (list (car string-list))))
      (if (not (null? string-list))
          (kernel (cdr string-list) (cons (car string-list)
                                          (cons ", " new-list)))
          (string-concatenate new-list))))

  (define (catalog-item-to-string catalog-item)
    (let ((name (catalog-name catalog-item))
          (ra (catalog-ra catalog-item))
          (dec (catalog-dec catalog-item))
          (window-strings (string-list-builder
                           (observing-windows-to-string-list (cadddr catalog-item)))))
      (let ((rah (number->string (car ra)))
            (ram (number->string (cadr ra)))
            (ras (number->string (caddr ra)))
            (decd (number->string (car dec)))
            (decm (number->string (cadr dec)))
            (decs (number->string (caddr dec))))
        (string-concatenate (list name " -- " rah "h " ram "m " ras "s -- "
                                  decd "\xB0; " decm "' " decs "\" -- "
                                  window-strings)))))

  (define (writelines lines f)
    (let kernel ((lines lines))
      (if (not (null? lines))
          (begin
            (write-string (car lines) f)
            (newline f)
            (kernel (cdr lines))))))

  ;; Actual Calculation
  (define catalog (catalog->catalog-hms (reformat-catalog (read-catalog "catalog.txt"))))
  (define observing-year (add-timezone (add-day-of-week (make-year 2021)) -6)) ; base timezone CST = -6
  (define observing-times (observing-window observing-year '(21 30 0) '(23 30 0))) ; 9:30 pm to 11:30 pm
  ;; Get local sidereal times for Austin -- longtidue -97.7997
  (define observing-times (convert-to-lst (convert-to-gast observing-times) -97.7997))
  (define catalog-with-observing-windows (build-catalog-windows
                                          (get-observing-dates (cdr catalog) observing-times)))

  ;; Prepare to export
  (define strings-to-print
    (cons (string-concatenate (list "Object Number -- Right Ascension -- Declination -- "
                                    "Dates where Meridian Transit is between "
                                    "9:30 and 11:30 pm local time"))
          (map catalog-item-to-string catalog-with-observing-windows)))

  ;; Build output file
  (define file (open-output-file "observing_dates.txt"))
  (writelines strings-to-print file)
  (close-output-port file))
