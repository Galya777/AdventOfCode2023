(define (read-map)
  (with-input-from-file "input.txt"
    (lambda ()
      (let ((column (string->number (read-line)))
            (row (string->number (read-line))))
        (let loop ((rows '())
                   (count 0))
          (if (= count row)
              (reverse rows)
              (loop (cons (string-take (read-line) column) rows) (+ count 1)))))))

(define (main)
  (let* ((data-map (read-map))
         (sum-digits (apply + (map (lambda (c) (string->number (string c))) (apply append data-map)))))
    (display sum-digits)
    (newline)))

(main)
