;;;; metztli.lisp

(in-package #:metztli)


(defparameter *server* nil)

(defun start-web-server (&key (port 8090))
  "Start the web server"
  (setf *server* (start (make-instance 'easy-acceptor :port port ))))

(defun stop-web-server ()
  "Stop the web server"
  (stop *server*))

(defun restart-web-server ()
  "Restart the web server"
  (stop-web-server)
  (start-web-server))

(defmacro with-html (title &body body)
  "The basic structure of a web page HTML => HEAD => BODY"
  `(with-yaclml-output-to-string
     (<:html :doctype "html"
	     (<:head
	      (<:meta (@ "charset"
			 "utf-8"))
	      (<:meta :name "viewport"
		      :content "width=device-width, initial-scale=1")
	      (<:link :rel "stylesheet"
		      :type "text/css"
		      :href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css")
	      (<:link :rel "stylesheet"
		      :type "text/css"
		      :href "https://getbootstrap.com/docs/5.0/examples/cover/cover.css")
	      (<:title (<:as-html ,title)))
	     (<:body
	      ,@body))))

(defun index ()
  "The index webpage"
  (with-html "index"
    (<:div :class "container pt-5 d-flex flex-column"
	   (<:div :class "row align-items-center justify-content-center gx-0"
		  (<:div :class "col-12 col-md-5 col-lg-4"
			 (<:div :class "card shadow"
				(<:div :class "card-header bg-primary text-white"
				       (<:h3 "Sign in "))
				(<:div :class "card-body"
				       (<:div :class "form-floating mb-3"
					      (<:input :class "form-control"
						       :id "floatingInput"
						       :type "email"
						       :placeholder "name@example.com")
					      (<:label :for "floatingInput"
						       "Email address"))
				       (<:div :class "form-floating mb-3"
					      (<:input :class "form-control"
						       :id "floatingPassword"
						       :type "email"
						       :placeholder "Password")
					      (<:label :for "floatingPassword"
						       "Password")))
				(<:div :class "card-footer d-flex justify-content-between bg-white"
				       (<:button :class "btn btn-primary w-100"
						 "Login"))))))
    (<:style "body{text-shadow:none;box-shadow: none !important;}")))

(defun cover ()
  "The cover bootstrap webpage"
  (with-html "Cover"
   (<:div :class "d-flex h-100 text-center text-white bg-dark v-100"
	  (<:div :class "cover-container d-flex w-100 h-100 p-3 mx-auto flex-column"
		 (<:header 
			   (<:div
			    (<:h3 :class "float-md-start mb-0"
				  "Cover")
			    (<:nav :class "nav nav-masthead justify-content-center float-md-end"
				   (<:a :class "nav-link active"
					:href "javascript:void(0);"
					"Home")
				   (<:a :class "nav-link"
					:href "javascript:void(0);"
					"Features")
				   (<:a :class "nav-link"
					:href "javascript:void(0);"
					"Contact"))))
		 (<:main :class "px-3 mt-5"
			 (<:h1 "Hunchentoot")
			 (<:p "Hunchentoot is a web server written in Common Lisp and at the same time a toolkit for building dynamic websites.")
			 (<:p :class "lead"
			      (<:a :class "btn btn-lg btn-secondary fw-bold border-white bg-white"
				   :href "javascript:void(0);"
				   "Learn more")))
		 (<:footer :class "mt-auto text-white-50"
			   (<:p
			    "By "
			    (<:a :class "text-white"
				:href "javascrip:void(0);"
				"Ikkiware"))))
	  (<:style "body,html {height: 100% !important; }"))))

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'index)
       (create-prefix-dispatcher  "/index.html" 'index)
       (create-prefix-dispatcher  "/cover.html" 'cover)))
