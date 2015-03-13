;; restix-util.el, v0.0.1
;; autor: adriano santos
;; licença: MIT

(defun restix-post-json (url &optional ssl req-headers &rest fields)
  "Faz requisição POST application/json para URL indicando se é SSL com alist de REQ-HEADERS e cons de FIELDS"
  (setq url (restix-http-ssl-internal url ssl))
  (push '("Content-Type" . "application/json") req-headers)
  (restix-request-internal url "POST" req-headers fields))

(defun restix-post-form (url &optional ssl req-headers &rest fields)
  "Faz requisição POST application/x-www-urlencoded para URL indicando se é SSL com alist de REQ-HEADERS e cons de FIELDS"
  (setq url (restix-http-ssl-internal url ssl))
  (restix-request-internal url "POST" req-headers (list (restix-params-encoded fields))))

(defun restix-get (url &optional ssl req-headers &rest fields)
  "Faz requisição GET para URL indicando se é SSL com alist de REQ-HEADERS e cons de FIELDS"
  (setq url (restix-http-ssl-internal url ssl))
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

(defun restix-json-stringify-region ()
  "Transforma conteúdo de region em texto para passagem de parâmetro json"
  (interactive)
  (let ((b0 (region-beginning))
	(b1 (region-end))
	(bcurr (current-buffer))
	json)
    (with-temp-buffer
      (insert-buffer-substring bcurr b0 b1)
      
      (replace-regexp "[\n\t ]*\\([{}]\\)[\n\t ]*"
		      "\\1"
		      nil (point-min) (point-max))
      (replace-regexp "[\n\t ]*\\([0-9A-Za-z_]+\\|\"[^\"]+\"\\)[\n\t ]*:[\n\t ]*\\([0-9]+\\|true\\|false\\|\"[^\"]+\"\\)[\n\t ]*\\([,}]?\\)[\n\t ]*"
		      "\\1:\\2\\3"
		      nil (point-min) (point-max))
      (replace-regexp "\\([0-9A-Za-z_]+\\):" "\"\\1\":" nil (point-min) (point-max))
      (replace-string "\"" "\\\"" nil (point-min) (point-max))
      (setq json (concat "\"" (buffer-string) "\"")))
    (restix-print-result-internal json b1)))

(defun restix-print-result-internal (msg &optional pos)
  "Imprime mensagem MSG dentro de quadro padrão após POS ou no ponto atual"
  (let ((br (mapconcat (lambda (n)
			 (if (= 0 (% n 2)) "-" "="))
		       (number-sequence 0 77)
		       "")))
    (save-excursion
      (if (numberp pos)
	  (goto-char pos))
      (newline)
      (insert br)
      (newline)
      (insert msg)
      (newline)
      (insert br))))

(defun restix-http-ssl-internal (url ssl)
  "Retorna string com URL com prefixo \"http://\" ou \"https://\" se for ssl"
  (unless
      (string-match "^https?://" url)
    (setq url (concat "http" (if ssl "s" "") "://" url)))
  url)

(defun restix-request-internal (url req-method req-headers fields)
  "Executa comando curl para URL usando REQ-METHOD com alist de REQ-HEADERS e cons de FIELDS"
  ;; type checking
  (let ((template-error "Argumento inválido para %s em restix-request-internal"))
    (if (not (stringp url))
	(error (format template-error "URL")))
    (if (or (not (stringp req-method))
	    (not (member req-method '("GET" "POST" "PUT" "DELETE"))))  
	(error (format template-error "REQ-METHOD")))
    (if (nlistp req-headers)
	(error (format template-error "REQ-HEADERS"))
      (mapcar (lambda (c)
		(if (atom c)
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
				     (format "-H \"%s: %s\"" (car c) (cdr c)))
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
      (message "Executando: %s ..." cmd)
      (restix-print-result-internal (shell-command-to-string cmd)))))

(provide 'restix-util)
