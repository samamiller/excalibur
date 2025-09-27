Excalibur MVP

What’s included
- Minimal SQLite library (`excalibur.db`) with `books` table.
- Avalonia desktop UI listing books and an “Add book(s)” button.
- Adds selected files as books (title from filename, author empty).

Run
- Prereqs: .NET 9 SDK
- Build/Run: `dotnet run`

Usage
- Click “Add book(s)” and select ebook files (epub/pdf/mobi/azw3).
- The list updates immediately and persists in `excalibur.db` in CWD.

Notes
- This is a minimal slice based on PRD: library management + add, persistent storage, simple list view, dark theme.
- Next steps: metadata editing dialog, basic search/filter, import folder, simple viewer (EPUB/PDF), and CLI.
