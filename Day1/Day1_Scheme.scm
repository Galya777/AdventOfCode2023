(define (read-calibration-document-from-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port)) (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line port) (cons line lines)))))))

(define (sum-calibration-values calibration-document)
  (define (parse-line line)
    (let ((first-digit (string->number (substring line 0 1))))
      (if (and (integer? first-digit) (< first-digit 10))
          first-digit
          0)))

  (apply + (map parse-line calibration-document)))

;; Example usage
(define calibration-document (read-calibration-document-from-file "/home/galya777/IdeaProjects/AdventOfCode2023/Day1_input.txt"))
(display (sum-calibration-values calibration-document))
(newline)
