#!/opt/local/bin/gosh
(use srfi-1)
(use srfi-13 :only (string-null?))
(use math.mt-random)
(use mecab)
(use net.twitter)
(use rfc.http)
(use rfc.uri)
(use sxml.ssax)
(use sxml.sxpath)

(define rand (make <mersenne-twister> :seed (sys-time)))
(define m (mecab-new2 ""))

;;Twitter
(define *cred* (make <twitter-cred>
					 :consumer-key "Cvxu3yFKWCOzjIoFY0yfg"
					 :consumer-secret "KL2sMfwR1iot3LtePAzhLm6JIK4eMhIyUaVC6SlHSs"
					 :access-token "447972405-L1fuyeCYWFjytoNJCNutY2VgU0aWclOmHHZSio8U"
					 :access-token-secret "38c6qSo00Yq4ZVLr4gwfgoTjulSIHcxOLpOtNx7Yds"))

;;ツイート取得
(define (tweet-get)
  (print "tweet-getting...")
  (let* ((sxml (twitter-home-timeline/sxml *cred* :count 300));:screen-name user))
		 (tweet ((sxpath "//text/text()")sxml)))
	(print "tweet-get ok!")
	tweet))

;;正規表現
(define (remove-tweet str)
  (let loop ((str str))
	(cond
	  ((rxmatch #/\[.+?\]/ str)
	   (loop (regexp-replace-all #/\[.+?\]/ str "")))
	  ((rxmatch #/(RT|QT)\s*@?\w+.*$/ str)
	   (loop (regexp-replace-all #/(RT|QT)\s*@?\w+.*$/ str "")))
	  ((rxmatch #/(\.?\s*@\w+)+\s?/ str)
	   (loop (regexp-replace-all #/(\.?\s*@\w+)+\s?/ str "")))
	  ((rxmatch #/.*https?:\/\/\S+.*$/ str)
	   (loop (regexp-replace-all #/.*https?:\/\/\S+.*$/ str "")))
	  ((rxmatch #/((@\w+)+)?.*\d+.\d+.*(ポスト|リプ数).*$/ str)
	   (loop (regexp-replace-all #/((@\w+)+)?.*\d+.\d+.*(ポスト|リプ数).*$/ str "")))
	  ((rxmatch #/.*#?(?i:NowPlaying).*/ str)
	   (loop (regexp-replace-all #/.*#?(?i:NowPlaying).*/ str "")))
	  ((rxmatch #/(&|(\.?\s*\w+;)+)/ str)
	   (loop (regexp-replace-all #/(&|(\.?\s*\w+;)+)/ str "")))
      ((rxmatch #/.*#.*/ str)
	   (regexp-replace-all #/.*#.*/ str ""))
	  (else str))))


;;形態素解析
(define (allnodes node)
  (if (mecab-node-null? node) 
      '() 
      (cons node (allnodes (mecab-node-next node)))))

(define (morp str)
  (let* ((node (mecab-sparse-tonode m str))
		 (result (remove string-null? (map (lambda (x) (mecab-node-surface x))(allnodes node)))))
	result))

;;テーブル作成
(define (table word)
  (let loop ((word word)(lst '()))
	(if (or (null? (cdr word)) (null? (cddr word)))
	  (reverse (cons (list* (car word) "" "") lst))
	  (loop (cdr word) (cons (list* (car word) (cadr word) (caddr word)) lst)))))

(define (remove-lst lst key)
  (map (^(x)(remove (^(y)(equal? y key))x))lst))

;;マルコフ連鎖
(define (markov lst ls)
  (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (print "solve: "solve)
	  (print "key: "key)
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (remove-lst lst hoge) (cddr hoge) (cons (cadr hoge) (cons (car hoge) result))))))))

;;意味あるのかはわからん
(define (check lst)
  (if (equal? (car lst) #\っ)
	(list->string (reverse (cons #\た lst)))
	(list->string (reverse lst))))

(define (main args)
  (let* ((str (map (lambda(x)(remove-tweet x)) (tweet-get)))
		 (word (remove null? (map (lambda(x)(morp x))str)))
		 (key (map (lambda(x)(car x)) word))
		 (ls (list-ref key (mt-random-integer rand (length key))))
		 (tweet (apply string-append (markov (map (lambda(x)(table x)) word) ls))))
	(print "tweet: "tweet)
	(twitter-update/sxml *cred* tweet)))
