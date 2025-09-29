-- Minimal seed data to exercise UI and Tag Browser
BEGIN TRANSACTION;

INSERT OR IGNORE INTO authors(name, sort) VALUES
 ('Isaac Asimov','Asimov, Isaac'),
 ('Ursula K. Le Guin','Le Guin, Ursula K.'),
 ('Frank Herbert','Herbert, Frank');

INSERT OR IGNORE INTO series(name) VALUES
 ('Foundation'),
 ('Earthsea'),
 ('Dune');

INSERT OR IGNORE INTO publishers(name) VALUES
 ('Ace Books'),
 ('Tor'),
 ('Gollancz');

INSERT OR IGNORE INTO tags(name) VALUES
 ('Science Fiction'),
 ('Fantasy'),
 ('Classic');

-- Books shell rows (paths are placeholders; mark missing to prevent open attempts)
INSERT OR IGNORE INTO books(id,title,author,path,missing,sortable_title,author_sort)
VALUES
 (1001,'Foundation','Isaac Asimov','/tmp/foundation.epub',1,'foundation','Asimov, Isaac'),
 (1002,'A Wizard of Earthsea','Ursula K. Le Guin','/tmp/earthsea.epub',1,'wizard of earthsea, a','Le Guin, Ursula K.'),
 (1003,'Dune','Frank Herbert','/tmp/dune.epub',1,'dune','Herbert, Frank');

-- Link tags
INSERT OR IGNORE INTO books_tags_link(book_id, tag_id)
SELECT 1001, id FROM tags WHERE name IN ('Science Fiction','Classic');
INSERT OR IGNORE INTO books_tags_link(book_id, tag_id)
SELECT 1002, id FROM tags WHERE name IN ('Fantasy','Classic');
INSERT OR IGNORE INTO books_tags_link(book_id, tag_id)
SELECT 1003, id FROM tags WHERE name IN ('Science Fiction','Classic');

-- Link authors
INSERT OR IGNORE INTO books_authors_link(book_id, author_id)
SELECT 1001, id FROM authors WHERE name = 'Isaac Asimov';
INSERT OR IGNORE INTO books_authors_link(book_id, author_id)
SELECT 1002, id FROM authors WHERE name = 'Ursula K. Le Guin';
INSERT OR IGNORE INTO books_authors_link(book_id, author_id)
SELECT 1003, id FROM authors WHERE name = 'Frank Herbert';

COMMIT;

