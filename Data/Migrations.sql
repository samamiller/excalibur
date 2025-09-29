-- Schema migrations for Excalibur (idempotent)
-- Creates core entity tables and link tables, plus indexes.

BEGIN TRANSACTION;

-- Schema versioning table
CREATE TABLE IF NOT EXISTS schema_version (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  version INTEGER NOT NULL
);

INSERT INTO schema_version (id, version)
SELECT 1, 1
WHERE NOT EXISTS (SELECT 1 FROM schema_version WHERE id = 1);

-- Base books table (created in code previously); ensure exists
CREATE TABLE IF NOT EXISTS books (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  author TEXT NULL,
  path TEXT NOT NULL,
  tags TEXT NULL,
  comments TEXT NULL,
  checksum TEXT NULL,
  added_at TEXT NULL,
  missing INTEGER NOT NULL DEFAULT 0
);

-- Authors, Series, Publishers, Tags
CREATE TABLE IF NOT EXISTS authors (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  sort TEXT NULL
);

CREATE TABLE IF NOT EXISTS series (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS publishers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

-- Link tables
CREATE TABLE IF NOT EXISTS books_authors_link (
  book_id INTEGER NOT NULL,
  author_id INTEGER NOT NULL,
  role TEXT NULL,
  PRIMARY KEY (book_id, author_id),
  FOREIGN KEY (book_id) REFERENCES books(id) ON DELETE CASCADE,
  FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS books_tags_link (
  book_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  PRIMARY KEY (book_id, tag_id),
  FOREIGN KEY (book_id) REFERENCES books(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
);

-- Optional series link: one-to-many via nullable books.series_id
CREATE INDEX IF NOT EXISTS idx_books_series ON books(series_id);
-- Note: SQLite doesn't support ADD CONSTRAINT; enforce FK via pragma at connection if needed.

-- Indexes
CREATE INDEX IF NOT EXISTS idx_books_sortable_title ON books(sortable_title);
CREATE INDEX IF NOT EXISTS idx_books_author_sort ON books(author_sort);
CREATE UNIQUE INDEX IF NOT EXISTS ux_books_path ON books(path);
CREATE INDEX IF NOT EXISTS idx_books_authors_book ON books_authors_link(book_id);
CREATE INDEX IF NOT EXISTS idx_books_authors_author ON books_authors_link(author_id);
CREATE INDEX IF NOT EXISTS idx_books_tags_book ON books_tags_link(book_id);
CREATE INDEX IF NOT EXISTS idx_books_tags_tag ON books_tags_link(tag_id);

UPDATE schema_version SET version = 2 WHERE id = 1;

COMMIT;
