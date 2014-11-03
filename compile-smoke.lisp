#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:parasol-build
  (:use #:cl)
  (:nicknames #:org.shirakumo.parasol.build))
(in-package #:org.shirakumo.parasol.build)

(defvar *smokeqt-git* "git://anongit.kde.org/smokeqt")
(defvar *smoke-git* "")

(defmacro with-cd ((dir &optional new) &body body)
  (let ((old (gensym "OLD"))
        (new (or new (gensym "NEW"))))
    `(let* ((,old (uiop:getcwd))
            (,new (merge-pathnames ,dir ,old)))
       (uiop:chdir ,new)
       (unwind-protect
            (progn ,@body)
         (uiop:chdir ,old)))))

(defmacro with-temporary-directory ((dir) &body body)
  `(let ((,dir (merge-pathnames
                #+unix "/tmp/parasol-build/"
                #-unix (error "Platform nor supported."))))
     (ensure-directories-exist ,dir)
     (unwind-protect
          (with-cd (,dir)
            ,@body)
       (uiop:delete-directory-tree ,dir :validate (constantly T)))))

(defmacro with-file-changing ((stream pathname) &body body)
  (let ((outstream (gensym "OUTSTREAM"))
        (result (gensym "RESULT")))
    `(let ((,result (with-open-file (,stream ,pathname :direction :input :if-does-not-exist :error)
                      ,@body)))
       (when ,result
         (with-open-file (,outstream ,pathname :direction :output :if-exists :supersede)
           (write-string ,result ,outstream))))))

(defun run-program (&rest args)
  (uiop:run-program args :output :interactive :error-output :interactive))

(defun git-clone (url)
  (run-program "git" "clone" "--depth" "1" url))

(defun build-smokeqt ()
  (print "Cloning smokeqt...")
  (git-clone *smokeqt-git*)
  (with-cd ("smokeqt/" dir)    
    (print "Patching qtcore/smokeconfig.xml")
    (with-file-changing (stream (merge-pathnames "qtcore/smokeconfig.xml" dir))
      (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
             (lquery:*lquery-master-document* (plump:parse stream)))
        (lquery:$ "classList" (append "<class>QThread</class>"))
        (lquery:$ (serialize) (node))))
    
    (print "Building...")
    (ensure-directories-exist (merge-pathnames "build/" dir))
    (with-cd ("build/" dir)
      (run-program "cmake" ".."
                   "-DCMAKE_BUILD_TYPE=Release"
                   "-DCMAKE_INSTALL_PREFIX=/usr"
                   "-DWITH_Qwt5=OFF")
      (run-program "make"))

    (print "Installing...")
    (ensure-directories-exist (merge-pathnames "install/" dir))
    (with-cd ("build/" dir)
      (run-program "make" "DESTDIR=../install/" "install"))))

(defun build-in-temp (&rest buildfuncs)
  (with-temporary-directory (dir)
    (loop for func in buildfuncs
          do (funcall func))))
