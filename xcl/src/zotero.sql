-- zoteroQueryByTagsAndCreatorsAndFilenames
-- :name zotero-query-by-tags-and-creators-and-filenames
SELECT
 items.itemID
,items.key,items.dateModified
,itemDataValues.value AS title
,tags
,creators
,attachmentItems.key AS attachmentKey
,itemAttachments.path AS attachmentPath
FROM items
-- get the entry title
 JOIN itemData ON itemData.itemID = items.itemID
 JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID
 JOIN fields ON fields.fieldID = itemData.fieldID
-- get tags
 LEFT OUTER JOIN
 (SELECT itemTags.itemID, GROUP_CONCAT(tags.name, ', ') AS tags
  FROM tags
  JOIN itemTags ON itemTags.tagID = tags.tagID
  GROUP BY itemTags.itemID)
 AS tagsQ ON tagsQ.itemID = items.itemID
-- get authors (creators)
 LEFT OUTER JOIN
 (SELECT itemCreators.itemID, GROUP_CONCAT(creators.lastName, ', ') AS creators
  FROM itemCreators
  JOIN creators ON creators.creatorID = itemCreators.creatorID
  GROUP BY itemCreators.itemID)
 AS creatorsQ on creatorsQ.itemID = items.itemID
-- get attachments
 JOIN itemAttachments ON itemAttachments.parentItemID = items.itemID
 JOIN items AS attachmentItems ON attachmentItems.itemID = itemAttachments.itemID
 WHERE (   LOWER(tags)     = LOWER(:tag)
        OR LOWER(creators) LIKE LOWER(:creator_LIKE)
        OR LOWER(itemAttachments.path) LIKE LOWER(:itemAttachments_path_LIKE))
 AND fields.fieldName = 'title'
 LIMIT :limit

-- zoteroQueryByAttributes
-- :name zotero-query-by-attributes
 SELECT
  items.itemID
 ,items.key
 ,fields.fieldName
 ,itemDataValues.value
 ,attachmentItems.key AS attachmentKey
 ,itemAttachments.path AS attachmentPath
 FROM items
 JOIN itemData ON itemData.itemID = items.itemID
 JOIN itemDataValues ON itemDataValues.valueID = itemData.valueID
 JOIN fields ON fields.fieldID = itemData.fieldID
 LEFT OUTER JOIN itemTags ON itemTags.itemID = items.itemID
-- get attachments
 JOIN itemAttachments ON itemAttachments.parentItemID = items.itemID
 JOIN items AS attachmentItems ON attachmentItems.itemID = itemAttachments.itemID
 WHERE 1
  -- TODO: possibly support doi-only
  -- AND fields.fieldName = 'DOI'
 AND LOWER(itemDataValues.value) LIKE LOWER(:itemDataValues_value_LIKE)
 GROUP BY items.itemID, fields.fieldName
 ORDER BY items.itemID ASC
 LIMIT :limit

-- zoteroQueryByFulltext
-- :name zotero-query-by-fulltext
SELECT
-- NOTE, the target item is the PARENT, not the attachment item!
  parentItems.itemID
 ,parentItems.key,parentItems.dateModified
 ,itemDataValues.value AS title
 ,fulltextWords.word
 ,attachmentItems.key AS attachmentKey
 ,itemAttachments.path AS attachmentPath
 FROM fulltextWords
 JOIN fulltextItemWords ON fulltextWords.wordID = fulltextItemWords.wordID
 JOIN items AS attachmentItems ON fulltextItemWords.itemID = attachmentItems.itemID
 JOIN itemTypes ON attachmentItems.itemTypeID = itemTypes.itemTypeID
-- get the parent item
 JOIN itemAttachments ON itemAttachments.itemID = attachmentItems.itemID
 JOIN items AS parentItems ON itemAttachments.parentItemID = parentItems.itemID
-- get the title of the PARENT item
 JOIN itemData ON parentItems.itemID = itemData.itemID
 JOIN itemDataValues ON itemData.valueID = itemDataValues.valueID
 JOIN fields ON itemData.fieldID = fields.fieldID
 WHERE 1
 AND itemTypes.typeName = 'attachment'
 AND fields.fieldName = 'title'
 AND fulltextWords.word = LOWER(:fullTextWord)
 GROUP BY parentItems.itemID
 LIMIT 20
