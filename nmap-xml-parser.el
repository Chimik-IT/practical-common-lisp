(defun map-xml-to-lisp (file)
    (let* ((nmap-xml-object (with-temp-buffer
			 (insert-file-contents file)
			 (libxml-parse-xml-region (point-min) (point-max))))
	   (host-nodes (dom-by-tag nmap-xml-object 'host)))
      host-nodes))

(defun get-address (number)
  (cdaadr (assoc 'address (elt (map-xml-to-lisp "~/telekom.xml") number))))

(defun get-hostname (number)
  (cdaadr (assoc 'hostname (assoc 'hostnames (elt (map-xml-to-lisp "~/telekom.xml") number)))))

(defun make-service-list (number)
  (setq length-ports-list (length (assoc 'ports (elt (map-xml-to-lisp "~/telekom.xml") number)))
	port-list (assoc 'ports (elt (map-xml-to-lisp "~/telekom.xml") number))
	num 2
	services-list ()
	serv-name-list ())
  (while (< num length-ports-list)
    (setq num (1+ num))
    (push (cdr (assoc 'portid (cadr(elt (assoc 'ports (elt (map-xml-to-lisp "~/telekom.xml") number))num)))) services-list))
  services-list)

(defun make-host-list (ip-list)
  (while ip-list
    (setq num (car ip-list)
	  address (get-address num)
	  services-list (make-service-list num))
    (push (list address services-list) hosts-list)
    (setq ip-list (cdr ip-list)))
  hosts-list)
(setq hosts-list ())
(make-host-list '(25 63 78))
hosts-list

(setq ip-list '(25 63 78))
ip-list
(while ip-list
  (print (car ip-list))
  (setq ip-list (cdr ip-list)))
(let ((address (get-address number))
      (service-list (make-service-list number)))
  (list address service-list)
  )


(cdaadr (assoc 'service (cdr(elt (assoc 'ports (elt (map-xml-to-lisp "~/telekom.xml") 26))3))))
