;; recommend running this script from terminal (rather than emacs)
;; because feedback is provided on download process
;; curl will overwite files by default (which is preferred behavior in this case

(import (only (wak irregex)
              irregex-split)
        (only (wak ssax parsing)
              ssax:xml->sxml)
        (only (wak sxml-tools sxml-tools)
              sxml:content))

;; Get HTML files for scraping ---------------------------------------------

;; CSUG and TSPL did not have sitemap.xml files
;; (at least not by appending sitemap.xml at end of URL)
;; used this site to generate sitemaps: https://www.xml-sitemaps.com/
;; https://cisco.github.io/ChezScheme/csug9.5/
;; https://scheme.com/tspl4/

;; fragile, position-based approach
;; vulnerable to change in shape of sitemap.xml
(define (sitemap->urlset path)
  (sxml:content
   (list-ref
    (ssax:xml->sxml (open-input-file path) '())
    2)))

(define (url-list->url url-list)
  (cadr (list-ref url-list 1)))

(define (extract-pagename url)
  (car (reverse (irregex-split #\/ url))))

;; shelling out with `system` calls; requires curl
(define (download-pages base-url output-folder pagenames)
  (for-each
   (lambda (page)
     (system
      (string-append "curl " base-url page " --output " output-folder "/" page)))
   pagenames))

(define urlset-csug (sitemap->urlset "sitemap-csug.xml"))
(define url-csug (map url-list->url urlset-csug))
(define base-url-csug (car url-csug))
(define pagenames-csug (map extract-pagename (cdr url-csug)))
(download-pages base-url-csug "html-csug" pagenames-csug)

(define urlset-tspl (sitemap->urlset "sitemap-tspl.xml"))
(define url-tspl (map url-list->url urlset-tspl))
(define base-url-tspl (car url-tspl))
(define pagenames-tspl (map extract-pagename (cdr url-tspl)))
(download-pages base-url-tspl "html-tspl" pagenames-tspl)

