#!/opt/local/bin/gosh
(use srfi-1)
(use sxml.sxpath)
(use net.twitter)

;;ファイル名
(define file-name "ファイル名")

;;自分のスクリーンネーム
(define screen-name "マイネーム")

;;Twitter
(define *cred* (make <twitter-cred>
					 :consumer-key "consumer-key"
					 :consumer-secret "consumer-secret"
					 :access-token "access-token"
					 :access-token-secret "access-token-secret"))

(define (user-get)
  (let ((sxml (twitter-home-timeline/sxml *cred* :count 100)))
	(remove (^(x)(equal? x screen-name)) ((sxpath "//screen_name/text()")sxml))))

(define (main args)
  (let ((lst (user-get)))
	(with-output-to-file file-name (pa$ print lst))))
