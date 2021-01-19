;;; orgmdb.el --- An OMDb API client for Emacs with some org-mode related convenience functions

;; Copyright (C) 2021 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamert@protonmail.com>
;; Version: 0.1
;; URL: https://github.com/isamert/orgmdb
;; Package-Requires: ((emacs "24.1") (dash "2.11.0") (s "1.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple mode to interact with OMDb API with some extra convenient functions.

;;; Code:

(require 's)
(require 'dash)

;;;###autoload
(defvar orgmdb-omdb-apikey
  nil
  "OMDb API key.")

;;;###autoload
(defvar orgmdb-fill-property-list
  '(genre runtime director imdb-id)
  "List of properties for `orgmdb-fill-movie-properties'.
When `orgmdb-fill-movie-properties' is called, these properties will be
  fetched and set to the headers property drawer.  See `orgmdb-{property-name}'
  functions for what the property names can be.")

;;;###autoload
(defvar orgmdb-omdb-url
  "http://www.omdbapi.com"
  "OMDb URL.")

(defun orgmdb--mk-url ()
  "Build OMDb url to do a request."
  (format  "%s/?apikey=%s" orgmdb-omdb-url orgmdb-omdb-apikey))

(defun orgmdb--request (url params)
  "Send a GET request to given URL and return the response body.
PARAMS should be an alist.  Pairs with nil values are skipped."
  (--> params
   (-map (lambda (x) (when (cadr x) (format "%s=%s" (car x) (url-hexify-string (cadr x))))) it)
   (--keep it it)
   (-reduce (lambda (acc x) (format "%s&%s" acc x)) it)
   (if (string-match-p "?" url)
       (format "%s&%s" url it)
     (format "%s?%s" url it))
   (with-current-buffer (url-retrieve-synchronously it)
     (let ((result (buffer-string)))
       (kill-buffer)
       result))
   (split-string it "\n\n")
   (cadr it)))

;;;###autoload
(defun orgmdb (&rest args)
  "Search for a movie in OMDb with ARGS.
Some call examples:
  (orgmdb :title \"in the mood for love\")
  (orgmdb :title \"in the mood for love\" :year 2000)
  (orgmdb :title \"in the mood for love\" :type \"movie\")
  (orgmdb :imdb \"tt0118694\")"
  (interactive)
  (--> (orgmdb--mk-url)
       (orgmdb--request it `(("t" ,(plist-get args :title))
                             ("i" ,(plist-get args :imdb))
                             ("y" ,(format "%s" (plist-get args :year)))
                             ("type" ,(plist-get args :type))))
       (json-read-from-string it)))

(defun orgmdb--get (f r &optional d)
  "Get key F from response R and default to D if F does not exist or is null/na."
  (let ((result (alist-get f r d)))
    (if (equal result "N/A")
        d
      result)))

;;;###autoload
(defun orgmdb-title (r &optional d)
  "Get title from omdb response R and default to D it does not exits."
  (orgmdb--get 'Title r d))

;;;###autoload
(defun orgmdb-year (r &optional d)
  "Get year from omdb response R and default to D it does not exits."
  (orgmdb--get 'Year r d))

;;;###autoload
(defun orgmdb-genre (r &optional d)
  "Get genre from omdb response R and default to D it does not exits."
  (orgmdb--get 'Genre r d))

;;;###autoload
(defun orgmdb-rated (r &optional d)
  "Get rated from omdb response R and default to D it does not exits."
  (orgmdb--get 'Rated r d))

;;;###autoload
(defun orgmdb-runtime (r &optional d)
  "Get runtime from omdb response R and default to D it does not exits."
  (orgmdb--get 'Runtime r d))

;;;###autoload
(defun orgmdb-released (r &optional d)
  "Get released from omdb response R and default to D it does not exits."
  (orgmdb--get 'Released r d))

;;;###autoload
(defun orgmdb-imdb-id (r &optional d)
  "Get imdb-id from omdb response R and default to D it does not exits."
  (orgmdb--get 'imdbID r d))

;;;###autoload
(defun orgmdb-imdb-rating (r &optional d)
  "Get imdb-rating from omdb response R and default to D it does not exits."
  (orgmdb--get 'imdbRating r d))

;;;###autoload
(defun orgmdb-poster (r &optional d)
  "Get poster from omdb response R and default to D it does not exits."
  (orgmdb--get 'Poster r d))

;;;###autoload
(defun orgmdb-director (r &optional d)
  "Get director from omdb response R and default to D it does not exits."
  (orgmdb--get 'Title r d))

;;;###autoload
(defun orgmdb-actors (r &optional d)
  "Get actors from omdb response R and default to D it does not exits."
  (orgmdb--get 'Actors r d))

;;;###autoload
(defun orgmdb-plot (r &optional d)
  "Get plot from omdb response R and default to D it does not exits."
  (orgmdb--get 'Plot r d))

;;;###autoload
(defun orgmdb-country (r &optional d)
  "Get country from omdb response R and default to D it does not exits."
  (orgmdb--get 'Country r d))

;;;###autoload
(defun orgmdb-awards (r &optional d)
  "Get awards from omdb response R and default to D it does not exits."
  (orgmdb--get 'Awards r d))

;;;###autoload
(defun orgmdb-metascore (r &optional d)
  "Get metascore from omdb response R and default to D it does not exits."
  (orgmdb--get 'Metascore r d))

(defun orgmdb--detect-title-from-header ()
  (-if-let (header (org-entry-get nil "ITEM"))
       (save-match-data
         (if (string-match "\\(.+\\)(\\([0-9]\\{4\\}\\))" header)
             `(,(s-trim (match-string 1 header)) ,(match-string 2 header))
           `(,(s-trim header) nil)))
     `(,(read-string "Movie name: ") ,(read-string "Movie year (can be empty): "))))

;;;###autoload
(defun orgmdb-movie-properties (title &optional year)
  "Open new org-buffer containing movie info and poster of given TITLE of YEAR.
When called interactively on an org header, it will automatically detect
 TITLE and YEAR for the format \"Movie Name (YEAR)\". Otherwise it'll simply
 ask for inputs."
  (interactive (orgmdb--detect-title-from-header))
  (let ((info (orgmdb :title title :year year))
        (poster-file (make-temp-file "~/.cache/orgmdb_poster_" nil ".jpg")))
    (shell-command-to-string (format "curl '%s' > %s" (orgmdb-poster info) poster-file))
    (switch-to-buffer (format "*orgmdb: %s*" (orgmdb-title info)))
    (org-mode)
    (insert (format "* %s (%s)\n" (orgmdb-title info) year))
    (insert (format "[[file:%s]]\n\n" poster-file))
    (org-display-inline-images)
    (insert (format "- Genre :: %s\n" (orgmdb-genre info)))
    (insert (format "- Rated :: %s\n" (orgmdb-rated info)))
    (insert (format "- Runtime :: %s\n" (orgmdb-runtime info)))
    (insert (format "- Released :: %s\n" (orgmdb-released info)))
    (insert (format "- Director :: %s\n" (orgmdb-director info)))
    (insert (format "- Actors :: %s\n" (orgmdb-actors info)))
    (insert (format "- Country :: %s\n" (orgmdb-country info)))
    (insert (format "- Awards :: %s\n" (orgmdb-awards info)))
    (insert (format "- Metascore :: %s\n" (orgmdb-metascore info)))
    (insert (format "- IMDb Rating :: %s\n" (orgmdb-imdb-rating info)))
    (insert (format "\n- Plot :: %s\n" (orgmdb-plot info)))))

(defun orgmdb-fill-movie-properties (title &optional year)
  "When called on an org header, fetches and sets pre-defined properties.
See `orgmdb-fill-property-list' for which properties will be set."
  (interactive (orgmdb--detect-title-from-header))
  (let ((info (orgmdb :title title :year year)))
    (--map (-as-> (format "orgmdb-%s" it) fn
                 (intern fn)
                 (funcall fn info)
                 (org-entry-put nil (upcase (format "%s" it)) fn))
           orgmdb-fill-property-list)))

(provide 'orgmdb)
;;; orgmdb.el ends here
