;;; some rudimentary tests
(in-package :xhtml-generator)

(rt:deftest xhtml-generator.princ
    (with-output-to-string (out)
      (with-xhtml (out)
        (:princ "123")))
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
123")

