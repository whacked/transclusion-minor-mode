-- calibreBuildDefaultQuery
-- :name calibre-build-default-query
SELECT
  b.id
, b.author_sort
, b.path
, d.name
, d.format
, b.pubdate
, b.title
FROM data AS d
  LEFT OUTER JOIN books AS b ON d.book = b.id
WHERE
   (lower(b.author_sort) LIKE :book_author_sort_LIKE
    OR lower(b.title) LIKE :book_title_LIKE)
AND upper(d.format) = 'EPUB'
LIMIT :limit
