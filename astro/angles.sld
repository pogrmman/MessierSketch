;;; Astro Angles Library
;; Provides utilites for working with angles
(define-library (astro angles)
  (import (scheme base)
          (scheme cxr)
          (scheme inexact))
  (export pi reduce-to-360
          degrees->radians radians->degrees
          degrees->hms hms->degrees
          hms+ hms- hms* hms/)

  (begin
    ;; Pi
    (define pi 3.141592653589793238462)
    
    ;; Angle Reduction Calculation
    ;; Reduce an angle to the range 0 <= angle <= 360
    ;; Usage: (reduce-to-360 angle) -> reduced-angle
    ;; angle -- an angle, in degrees
    ;; reduced-angle -- the same angle, reduced such that it's between 0 and 360
    (define (reduce-to-360 angle)
      (let ((frac (abs (- (/ angle 360) (truncate (/ angle 360))))))
        (if (positive? angle)
            (* 360 frac)
            (- 360 (* 360 frac)))))

    ;; Degrees to Radians Conversion
    ;; Convert an angle in degrees to an angle in radians
    ;; Usage: (degrees->radians angle) -> converted-angle
    ;; angle -- an angle, in degrees
    ;; converted-angle -- an angle, in radians
    (define (degrees->radians angle)
      (* angle (/ pi 180)))

    ;; Radians to Degrees Conversion
    ;; Convert an angle in radians to an angle in degrees
    ;; Usage: (radians->degrees angle) -> converted-angle
    ;; angle -- an angle, in radians
    ;; converted-angle -- an angle, in degrees
    (define (radians->degrees angle)
      (* angle (/ 180 pi)))

    ;; Degrees to Hours:Minutes:Seconds Conversion
    ;; Convert an angle in degrees to an angle in hours:minutes:seconds
    ;; Usage: (degrees->hms angle) -> converted-angle
    ;; angle -- an angle, in degrees
    ;; converted-angle -- an angle, in hours:minutes:seconds
    (define (degrees->hms angle)
      (let* ((hours (/ angle 15))
             (minutes (* 60 (- hours (truncate hours))))
             (seconds (* 60 (- minutes (truncate minutes)))))
        (list (truncate hours)
              (truncate minutes)
              seconds)))

    ;; Hours:Minutes:Seconds to Degrees Conversion
    ;; Convert an angle in hours:minutes:seconds to an angle in degrees
    ;; Usage: (hms->degrees angle) -> converted-angle
    ;; angle -- an angle, in hours:minutes:seconds
    ;; converted-angle -- an angle, in degrees
    (define (hms->degrees angle)
      (let ((hours (car angle))
            (minutes (cadr angle))
            (seconds (caddr angle)))
        (cond ((and (= minutes 0)
                    (= seconds 0))
               (* hours 15))
              ((= minutes 0)
               (* (+ hours (/ seconds 3600)) 15))
              ((= seconds 0)
               (* (+ hours (/ minutes 60)) 15))
              (else
               (* (+ hours (/ minutes 60) (/ seconds 3600) 15))))))

    ;; Hours:Minutes:Seconds Addition
    ;; Add two angles in hours:minutes:seconds format
    ;; Usage: (hms+ angle angle2) -> sum-angle
    ;; angle -- an angle, in hours:minutes:seconds
    ;; angle2 -- an angle, in hours:minutes:seconds
    ;; sum-angle -- the sum of angle and angle2, in hours:minutes:seconds
    (define (hms+ angle angle2)
      (degrees->hms (reduce-to-360 (+ (hms->degrees angle) (hms->degrees angle2)))))

    ;; Hours:Minutes:Seconds Subtraction
    ;; Subtract two angles in hours:minutes:seconds format
    ;; Usage: (hms- angle angle2) -> difference-angle
    ;; angle -- an angle, in hours:minutes:seconds
    ;; angle2 -- an angle, in hours:minutes:seconds
    ;; difference-angle -- the difference of angle and angle2, in hours:minutes:seconds
    (define (hms- angle angle2)
      (degrees->hms (reduce-to-360 (- (hms->degrees angle) (hms->degrees angle2)))))

    ;; Hours:Minutes:Seconds Multiplication
    ;; Multiply two angles in hours:minutes:seconds format
    ;; Usage: (hms* angle angle2) -> product-angle
    ;; angle -- an angle, in hours:minutes:seconds
    ;; angle2 -- an angle, in hours:minutes:seconds
    ;; product-angle -- the product of angle and angle2, in hours:minutes:seconds
    (define (hms* angle angle2)
      (degrees->hms (reduce-to-360 (* (hms->degrees angle) (hms->degrees angle2)))))

    ;; Hours:Minutes:Seconds Division
    ;; Divide two angles in hours:minutes:seconds format
    ;; Usage: (hms/ angle angle2) -> quotient-angle
    ;; angle -- an angle, in hours:minutes:seconds
    ;; angle2 -- an angle, in hours:minutes:seconds
    ;; quotient-angle -- the quotient of angle and angle2, in hours:minutes:seconds
    (define (hms/ angle angle2)
      (degrees->hms (reduce-to-360 (/ (hms->degrees angle) (hms->degrees angle2)))))))
