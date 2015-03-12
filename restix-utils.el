;; restix-util.el, v0.0.1
;; autor: adriano santos
;; licença: MIT

(defun restix-get (url &optional ssl req-headers &rest fields)
  "Faz requisição GET para URL indicando se é SSL com alist de REQ-HEADERS e cons de FIELDS"
  (unless
      (string-match "^https?://" url)
    (setq url (concat "http" (if ssl "s" "") "://" url)))
  (setq url 
	(concat url (if
			fields
			(concat (if (string-match "\?" url) "&" "?")
				(restix-params-encoded fields)) "")))
  (restix-request-internal url "GET" req-headers fields))

(defun restix-params-encoded (params)
  "Retorna query string de paramêtros PARAMS"
  (mapconcat 
   (lambda (p)
     (concat (url-hexify-string (car p))
	     "="
	     (url-hexify-string (cdr p))))
   params
   "&"))

(defun restix-request-internal (url req-method req-headers fields)
  "Executa comando curl para URL usando REQ-METHOD com alist de REQ-HEADERS e cons de FIELDS"
  ;; type checking
  (let ((template-error "Argumento inválido para %s em restix-request-internal"))
    (if (not (stringp url))
	(error (format template-error "URL")))
    (if (or (not (stringp req-method))
	    (not (member req-method '("GET" "POST" "PUT" "DELETE"))))  
	(error (format template-error "REQ-METHOD")))
    (if (not (listp req-headers))
	(error (format template-error "REQ-HEADERS"))
      (mapcar (lambda (c)
		(if (not (consp c))
		    (error (format template-error "REQ-HEADERS"))
		  (if (or (not (stringp (car c)))
			  (not (stringp (cdr c))))
		      (error (format template-error "REQ-HEADERS")))))
	      req-headers))
    (mapcar (lambda (f)
	      (if (not (stringp f))
		  (error (format template-error "FIELDS"))))
	    fields)
    (let ((cmd (format "curl -skLX %s" req-method)))
      (if (> (length req-headers) 0)
	  (setq cmd
		(concat cmd
			" "
			(mapconcat (lambda (c)
				     (format "-H \"%s: %s;\"" (car c) (cdr c)))
				   req-headers
				   " "))))
      (if (> (length fields) 0)
	  (setq cmd
		(concat cmd
			" "
			(mapconcat (lambda (f)
				     (format "-d %S" f))
				   fields
				   " "))))
      (setq cmd
	    (concat cmd " " url))
      (newline)
      (message "Executando: %s ..." cmd)
      (mapcar (lambda (n)
		(insert (if (= 0 (% n 2)) "-" "=")))
	      (number-sequence 0 77))
      (newline)
      (insert (shell-command-to-string cmd))
      (newline)
      (mapcar (lambda (n)
		(insert (if (= 0 (% n 2)) "-" "=")))
	      (number-sequence 0 77)))))

(provide 'restix-utils)
