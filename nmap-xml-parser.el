;;; NMAP XML Reader for Emacs

(defun map-xml-to-lisp (file)
    (let* ((nmap-xml-object (with-temp-buffer
			 (insert-file-contents file)
			 (libxml-parse-xml-region (point-min) (point-max))))
	   (host-nodes (dom-by-tag nmap-xml-object 'host)))
      host-nodes))

(defun get-address (number)
  (cdaadr (assoc 'address (elt (map-xml-to-lisp "~/telekom2.xml") number))))

(defun get-status (number)
  (cdaadr (assoc 'status (elt (map-xml-to-lisp "~/telekom2.xml") number))))

(defun get-hostname (number)
  (cdaadr (assoc 'hostname (assoc 'hostnames (elt (map-xml-to-lisp "~/telekom2.xml") number)))))

(defun make-service-list (number)
  (setq length-ports-list (length (assoc 'ports (elt (map-xml-to-lisp "~/telekom2.xml") number)))
	port-list (assoc 'ports (elt (map-xml-to-lisp "~/telekom2.xml") number))
	num 2
	services-list ()
	serv-name-list ())
  (while (< num length-ports-list)
    (setq num (1+ num))
    (push (cdr (assoc 'portid (cadr(elt (assoc 'ports (elt (map-xml-to-lisp "~/telekom2.xml") number))num)))) services-list)
    (push (cdaadr (assoc 'service (cdddr (elt (assoc 'ports (elt (map-xml-to-lisp "~/telekom2.xml") number)) num)))) serv-name-list))
  (setq service-port-list (combine2lists serv-name-list services-list "/")))

(defun make-host-list (ip-list)
  (while ip-list
    (setq zahl (pop ip-list))
    (push (list (get-address zahl) (get-hostname zahl) (get-status zahl) (make-service-list zahl)) hosts-list))
  hosts-list)

(defun combine2lists (list1 list2 combinator)
  (setq serv ())
  (while list1
    (push (format "%s%s%s" (pop list1) combinator (pop list2)) serv))
    (mapconcat 'identity serv " "))
