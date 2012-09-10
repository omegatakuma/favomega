#!/opt/local/bin/gosh
(use srfi-1 :only (remove filter-map alist-delete))
(use srfi-13 :only (string-null?))
(use sxml.sxpath :only (sxpath))
(use math.mt-random)
(use mecab)
(use net.twitter)

(define file-name "ファイル名")

(define rand (make <mersenne-twister> :seed (sys-time)))
(define m (mecab-new2 ""))

;;Twitter
(define *cred* (make <twitter-cred>
					 :consumer-key "consumer-key"
					 :consumer-secret "consumer-secret"
					 :access-token "access-token"
					 :access-token-secret "access-token-secret"))

;;ツイート取得
(define (tweet-get user)
  (let* ((sxml (twitter-user-timeline/sxml *cred* :screen-name user :count 200));:screen-name user))
                 (tweet ((sxpath "//text/text()")sxml)))
        tweet))

(define (twitter-lst)
  (let* ((lst (with-input-from-file file-name (pa$ read)))
		 (num (mt-random-integer rand (length lst))))
	(list-ref lst num)))

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
      ((rxmatch #/.*(#|＃).*/ str)
	   (loop (regexp-replace-all #/.*#.*/ str "")))
	  ((rxmatch #/【.*】/ str)
	   (loop (regexp-replace-all #/【.*】/ str "")))
	  ((rxmatch #/レベルアップしたよ! 現在の経験値:\d+ 現在のレベル:\d+だよ\(\^\^\)/ str)
	   (loop (regexp-replace-all #/レベルアップしたよ! 現在の経験値:\d+ 現在のレベル:\d+だよ\(\^\^\)/ str "")))
	  ((rxmatch #/えへ+(\(\^\^\))+/ str)
	   (loop (regexp-replace-all #/えへ+(\(\^\^\))+/ str "")))
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

;;マルコフ連鎖
(define (markov lst ls)
  (let loop ((lst lst)(key ls)(result '()))
	(let ((solve (filter-map (lambda(x)(assoc key x))lst)))
	  (if (null? solve)
		  (reverse (cons key result))
		(let* ((num (mt-random-integer rand (length solve)))
			   (hoge (list-ref solve num)))
		  (loop (map(^(x)(alist-delete (car hoge) x))lst) (cddr hoge) (cons (cadr hoge) (cons (car hoge) result))))))))

;;意味あるのかはわからん
(define (check str)
  (let ((lst (reverse (string->list str))))
	(cond ((or (equal? (car lst) #\っ) 
			   (equal? (car lst) #\し)
			   (equal? (car lst) #\れ))
		   (list->string (reverse (cons #\た lst))))
		  (else (list->string (reverse lst))))))

(define (main args)
  (let* ((user (twitter-lst))
		 (str (map (lambda(x)(remove-tweet x)) (tweet-get user)))
		 (word (remove null? (map (lambda(x)(morp x))str)))
		 (key (map (lambda(x)(car x)) word))
		 (ls (list-ref key (mt-random-integer rand (length key))))
		 (tweet (apply string-append (markov (map (lambda(x)(table x)) word) ls))))
	(twitter-update/sxml *cred* (check tweet))))
