(in-package :cl-toml)

(defun literal-string-char-p (char)
  (not (or (eql char #\') (eql char #\newline))))

(defun literal-string-p (object)
  (and (stringp object)
       (every #'literal-string-char-p object)))

(defun bare-key-character-p (c)
  (or (alphanumericp c)
      (member c '(#\_ #\-))))

(defun bare-key-p (x)
  (and (stringp x)
       (every #'bare-key-character-p x)))

(deftype literal-string ()
  '(and string (satisfies literal-string-p)))
