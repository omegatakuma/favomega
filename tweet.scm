#!/opt/local/bin/gosh
(use srfi-1 :only (remove filter-map alist-delete))
(use srfi-13 :only (string-null?))
(use sxml.sxpath :only (sxpath))
(use math.mt-random)
(use mecab)
(use net.twitter)

(define rand (make <mersenne-twister> :seed (sys-time)))
(define m (mecab-new2 ""))

;;Twitter
(define *cred* (make <twitter-cred>
					 :consumer-key "Cvxu3yFKWCOzjIoFY0yfg"
					 :consumer-secret "KL2sMfwR1iot3LtePAzhLm6JIK4eMhIyUaVC6SlHSs"
					 :access-token "447972405-L1fuyeCYWFjytoNJCNutY2VgU0aWclOmHHZSio8U"
					 :access-token-secret "38c6qSo00Yq4ZVLr4gwfgoTjulSIHcxOLpOtNx7Yds"))

;;ツイート取得
(define (tweet-get2)
  (print "tweet-getting...")
  (let* ((sxml (twitter-home-timeline/sxml *cred* :count 300));:screen-name user))
		 (tweet ((sxpath "//text/text()")sxml)))
	tweet))

(define (tweet-get user)
  (print "tweet-getting...")
  (let* ((sxml (twitter-user-timeline/sxml *cred* :screen-name user :count 200));:screen-name user))
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
	  ((rxmatch #/(&|(\.?\s*\w+\;)+)/ str)
	   (loop (regexp-replace-all #/(&|(\.?\s*\w+\;)+)/ str "")))
      ((rxmatch #/.*#.*/ str)
	   (regexp-replace-all #/.*#.*/ str ""))
	  ((rxmatch #/現在の経験値:\d+\s+現在のレベル:\d+だよ\(\^\^\)/ str)
	   (loop (regexp-replace-all #/現在の経験値:\d+\s+現在のレベル:\d+だよ\(\^\^\)/ "現在の経験値:73 現在のレベル:8だよ(^^)" "")))
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
	  (reverse lst)
	  (loop (cdr word) (cons (list* (car word) (cadr word) (caddr word)) lst)))))

(define (table-s word)
    (let loop ((word word)(lst '()))
	(if (null? (cdr word))
	  (reverse lst)
	  (loop (cdr word) (acons (car word) (cadr word) lst)))))

(define (remove-lst lst key)
  (map (^(x)(remove (^(y)(equal? y key))x))lst))

;;マルコフ連鎖
(define (markov lst ls)
  (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (print "key: "key)	  
	  (print "solve: "solve)
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (remove-lst lst hoge) (cddr hoge) (cons (cadr hoge) (cons (car hoge) result))))))))

(define (markov-t lst ls)
    (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (print "key: "key)	  
	  (print "solve: "solve)
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (map(lambda(x)(alist-delete (car hoge)x))lst) (cddr hoge) (cons (cadr hoge) (cons (car hoge) result))))))))

(define (markov-s lst ls)
  (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (print "key: "key)	  
	  (print "solve: "solve)
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (map(lambda(x)(alist-delete (car hoge)x))lst) (cdr hoge) (cons (car hoge) result)))))))

(define (markov-st lst ls)
  (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (print "key: "key)	  
	  (print "solve: "solve)
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (remove-lst lst hoge) (cdr hoge) (cons (car hoge) result)))))))

;;意味あるのかはわからん
(define (check str)
  (let ((lst (reverse (string->list str))))
	(cond ((or (equal? (car lst) #\っ) 
			   (equal? (car lst) #\し)
			   (equal? (car lst) #\れ))
		   (list->string (reverse (cons #\た lst))))
		  (else (list->string (reverse lst))))))

(define (main args)
  (let* ((str (map (lambda(x)(remove-tweet x)) (tweet-get "omegatakuma")))
		 (word (remove null? (map (lambda(x)(morp x))str)))
		 (key (map (lambda(x)(car x)) word))
		 (ls (list-ref key (mt-random-integer rand (length key))))
		 (tweet (apply string-append (markov (map (lambda(x)(table x)) word) ls)))
		 (tweet-t (apply string-append (markov-t (map (lambda(x)(table x)) word) ls)))
		 (tweet-s (apply string-append (markov-s (map (lambda(x)(table-s x)) word) ls)))
		  (tweet-st (apply string-append (markov-st (map (lambda(x)(table-s x)) word) ls))))

	(print "tweet(remove-lst): "tweet)
	(print "tweet-t(alist-delete): "tweet-t)
	(print "tweet-s(remove-lst): "tweet-s)
	(print "tweet-st(alist-delete): "tweet-st)))
