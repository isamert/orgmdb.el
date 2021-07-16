;;; orgmdb.el --- An OMDb API client with some convenience functions -*- lexical-binding: t -*-

;; Copyright (C) 2021 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamert@protonmail.com>
;; Version: 0.1
;; URL: https://github.com/isamert/orgmdb
;; Package-Requires: ((emacs "25.1") (dash "2.11.0") (s "1.12.0") (org "8.0.0"))

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
(require 'seq)
(require 'org)
(require 'json)

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

(defconst orgmdb--types '("movie" "series" "episode"))

(defun orgmdb--url-retrieve-sync (url)
  "Retrieve URL synchronously as string."
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((result (decode-coding-string (buffer-string) 'utf-8)))
      (kill-buffer)
      result)))

(defun orgmdb--request (url params)
  "Send a GET request to given URL and return the response body.
PARAMS should be an alist.  Pairs with nil values are skipped."
  (->> params
    (--filter (cadr it))
    (url-build-query-string)
    (format "%s/?%s" url)
    (orgmdb--url-retrieve-sync)
    (s-split "\n\n")
    (cadr)))

;;;###autoload
(defun orgmdb (&rest args)
  "Search for a movie in OMDb with ARGS.
Some call examples:
  (orgmdb :title \"in the mood for love\")
  (orgmdb :title \"in the mood for love\" :year 2000)
  (orgmdb :title \"in the mood for love\" :type 'movie)
  (orgmdb :imdb \"tt0118694\")"
  (->> (orgmdb--request
        orgmdb-omdb-url
        `(("t" ,(plist-get args :title))
          ("i" ,(plist-get args :imdb))
          ("y" ,(plist-get args :year))
          ("type" ,(plist-get args :type))
          ("apikey" ,(s-trim orgmdb-omdb-apikey))))
    (json-read-from-string)))

(defun orgmdb--get (f r &optional d)
  "Get key F from response R and default to D if F does not exist or is null/na."
  (let ((result (alist-get f r d)))
    (if (equal result "N/A")
        d
      result)))

(defun orgmdb--score-of (rating-type r)
  "Get RATING-TYPE from R.
Right now RATING-TYPE could be one of the following (as omdb API
only returns these):
- Internet Movie Database
- Rotten Tomatoes
- Metacritic"
  (->>
   (alist-get 'Ratings r)
   (seq-find (lambda (it) (s-equals? (alist-get 'Source it) rating-type)))
   (alist-get 'Value)))

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
(defun orgmdb-imdb (r &optional d)
  "Get IMDb score from omdb response R and default to D it does not exits.
The difference between this function and `orgmdb-imdb-rating' is
that this returns in the \"X/10\" format while
`orgmdb-imdb-rating' returns just \"X\"."
  (or (orgmdb--score-of "Internet Movie Database" r) d))

;;;###autoload
(defun orgmdb-poster (r &optional d)
  "Get poster from omdb response R and default to D it does not exits."
  (orgmdb--get 'Poster r d))

;;;###autoload
(defun orgmdb-director (r &optional d)
  "Get director from omdb response R and default to D it does not exits."
  (orgmdb--get 'Director r d))

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
(defun orgmdb-language (r &optional d)
  "Get language from omdb response R and default to D if it does not exits."
  (orgmdb--get 'Language r d))

;;;###autoload
(defun orgmdb-imdb-votes (r &optional d)
  "Get IMDb vote count from omdb response R and default to D if it does not exits."
  (orgmdb--get 'imdbVotes r d))

;;;###autoload
(defun orgmdb-dvd (r &optional d)
  "Get DVD release date from omdb response R and default to D if it does not exits."
  (orgmdb--get 'DVD r d))

;;;###autoload
(defun orgmdb-box-office (r &optional d)
  "Get box office from omdb response R and default to D if it does not exits."
  (orgmdb--get 'BoxOffice r d))

;;;###autoload
(defun orgmdb-production (r &optional d)
  "Get production firm from omdb response R and default to D if it does not exits."
  (orgmdb--get 'Production r d))

;;;###autoload
(defun orgmdb-website (r &optional d)
  "Get website from omdb response R and default to D if it does not exits."
  (orgmdb--get 'Website r d))

;;;###autoload
(defun orgmdb-awards (r &optional d)
  "Get awards from omdb response R and default to D it does not exits."
  (orgmdb--get 'Awards r d))

;;;###autoload
(defun orgmdb-type (r &optional d)
  "Get type from omdb response R and default to D it does not exits."
  (orgmdb--get 'Type r d))

;;;###autoload
(defun orgmdb-season (r &optional d)
  "Get season from omdb response R and default to D it does not exits."
  (orgmdb--get 'Season r d))

;;;###autoload
(defun orgmdb-episode (r &optional d)
  "Get episode from omdb response R and default to D it does not exits."
  (orgmdb--get 'Episode r d))

;;;###autoload
(defun orgmdb-metascore (r &optional d)
  "Get metascore from omdb response R and default to D it does not exits."
  (orgmdb--get 'Metascore r d))

;;;###autoload
(defun orgmdb-writer (r &optional d)
  "Get writer from omdb response R and default to D if it does not exits."
  (orgmdb--get 'Writer r d))

;;;###autoload
(defun orgmdb-metacritic (r &optional d)
  "Get metacritic score from omdb response R and default to D it does not exits.
The difference between this function and `orgmdb-metascore' is
that this returns in the \"X/100\" format while
`orgmdb-metascore' returns just \"X\"."
  (or (orgmdb--score-of "Metacritic" r) d))

;;;###autoload
(defun orgmdb-tomatometer (r &optional d)
  "Get tomatometer score from omdb response R and default to D it does not exits."
  (or (orgmdb--score-of "Rotten Tomatoes" r) d))

(defun orgmdb--ask-for-type ()
  "Simply ask for a type."
  (completing-read "Type: " orgmdb--types))

(defun orgmdb--ask-for-title-and-year ()
  "Simply ask for title and year from interactively."
  `(,(read-string "Title: ")
    ,(read-string "Year (can be empty): ")))

(defun orgmdb--detect-type-from-header ()
  "Detect whether current heading is a movie or a series or an episode."
  (interactive)
  (let* ((type-prop (org-entry-get nil "TYPE"))
         (type-tag (--first (member it orgmdb--types) (org-get-tags))))
    (cond
     (type-tag type-tag)
     (type-prop type-prop)
     (t (orgmdb--ask-for-type)))))

(defun orgmdb--detect-params-from-header ()
  "Get parameters for `orgmdb' function from current org header.
If not on a org header, simpy ask from user."
  (interactive)
  (-let* ((header (or (org-entry-get nil "ITEM") ""))
          (imdb-id (or (car (s-match "tt[0-9]\\{7,8\\}" header)) (org-entry-get nil "IMDB-ID")))
          (type (when (not imdb-id)
                  (orgmdb--detect-type-from-header)))
          ((title year) (when (not imdb-id)
                          (if (s-blank? header)
                              (orgmdb--ask-for-title-and-year)
                            (-if-let ((_ title year) (s-match "\\(.+\\)(\\([0-9]\\{4\\}\\))" header))
                                `(,(s-trim title) ,year)
                              `(,(s-trim (car (s-split "(" header)))))))))
    `(:imdb ,imdb-id :type ,type :title ,title :year ,year)))

(defun orgmdb-is-response-successful (response)
  "Check if the returned RESPONSE is successful or not."
  (not (string-equal (alist-get 'Response response) "False")))

(defun orgmdb--ensure-response-is-successful (response)
  (when (not (orgmdb-is-response-successful response))
    (user-error "The search did not return any results")))

;;;###autoload
(defun orgmdb-movie-properties (&rest args)
  "Open new org-buffer containing movie info and poster of given ARGS.
ARGS should be in the same form with `orgmdb' function.

If this function is called on an org heading then it'll try to
detect parameters based on that heading.  If not, it'll simply ask
for title and year.  See `orgmdb-fill-movie-properties' function
for check how parameter detection works."
  (interactive (orgmdb--detect-params-from-header))
  (let ((info (apply #'orgmdb args))
        (poster-file (make-temp-file "~/.cache/orgmdb_poster_" nil ".jpg")))
    (orgmdb--ensure-response-is-successful info)
    (shell-command-to-string (format "curl '%s' > %s" (orgmdb-poster info) poster-file))
    (switch-to-buffer (format "*orgmdb: %s*" (orgmdb-title info)))
    (org-mode)
    (insert (format "* %s (%s)\n" (orgmdb-title info) (orgmdb-year info)))
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

(defun orgmdb--fill-properties (info should-set-title)
  (orgmdb--ensure-response-is-successful info)
  (--map (-as-> (format "orgmdb-%s" it) fn
                (intern fn)
                (apply fn `(,info "N/A"))
                (org-entry-put nil (upcase (format "%s" it)) fn))
         orgmdb-fill-property-list)
  (when should-set-title
    (org-edit-headline
     (pcase (orgmdb-type info)
       ("movie" (format "%s (%s)" (orgmdb-title info) (orgmdb-year info)))
       ("series" (format "%s (%s)" (orgmdb-title info) (orgmdb-year info)))
       ("episode" (format "S%sE%s - %s" (orgmdb-season info) (orgmdb-episode info) (orgmdb-title info))))))
  (message "Done."))

;;;###autoload
(defun orgmdb-fill-movie-properties (should-set-title)
  "Fetch and set pre-defined properties to current org header.
See `orgmdb-fill-property-list' for which properties will be set.

It will automatically try to automatically detect what to search
for based on the current org header.  Following formats are
recognized:

- TITLE
- TITLE (YEAR)
- IMDB-ID

It also checks if the header contains one of the following tags:
\"movie\", \"series\", \"episode\" or checks if the TYPE property
of the current header is one of these. If so, searches for that
particular type only.

If the format of the header is unrecognized, it will ask for
title and year from user.

If SHOULD-SET-TITLE is non-nil, then also set the text of current
org header using the following formats:

- For movies: TITLE (YEAR)
- For series: TITLE (YEAR-YEAR)
- For episodes: S01E01 - TITLE"
  (interactive "P")
  (orgmdb--fill-properties
   (apply #'orgmdb (orgmdb--detect-params-from-header))
   should-set-title))

;;;###autoload
(defun orgmdb-fill-movie-properties-with-title (should-set-title)
  "Fetch and set pre-defined properties to current org header.
Like `orgmdb-fill-movie-properties' but instead of automatically
detecting what to search for, it asks for title and year."
  (interactive "P")
  (-let ((type (orgmdb--ask-for-type))
         ((title year) (orgmdb--ask-for-title-and-year)))
    (orgmdb--fill-properties
     (orgmdb :type type :title title :year year)
     should-set-title)))

;;;###autoload
(defun orgmdb-fill-movie-properties-with-imdb-id (should-set-title)
  "Fetch and set pre-defined properties to current org header.
Like `orgmdb-fill-movie-properties' but instead of automatically
detecting what to search for, it asks for IMDb id."
  (interactive "P")
  (orgmdb--fill-properties
   (orgmdb :imdb (read-string "IMDb id: "))
   should-set-title))

(provide 'orgmdb)
;;; orgmdb.el ends here
