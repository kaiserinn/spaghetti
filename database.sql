DROP DATABASE IF EXISTS spaghetti;
CREATE DATABASE spaghetti;
USE spaghetti;

CREATE OR REPLACE TABLE pasta (
    id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT,
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    edit_key VARCHAR(255),
    view_key VARCHAR(255),
    slug VARCHAR(255) NOT NULL UNIQUE,
    created_at DATETIME NOT NULL DEFAULT NOW(),
    updated_at DATETIME NOT NULL DEFAULT NOW()
);

-- Dummy Data
INSERT INTO pasta (title, content, slug, view_key, edit_key)
    VALUES ('New Title', 'New content.', 'slug1', '', ''),
           ('Edit Code', 'Content.', 'slug2', '', 'edit'),
           ('View Code', 'Content.', 'slug3', 'view', '');
