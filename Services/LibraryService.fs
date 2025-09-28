namespace Excalibur.Services

open System.IO
open Excalibur.Domain
open Excalibur.Data

module LibraryService =
    let initialize () = LibraryRepository.ensureDb ()

    let loadBooks () = LibraryRepository.getBooks ()

    let addBookFromPath (path: string) (author: string option) : bool =
        let title = Path.GetFileNameWithoutExtension path
        LibraryRepository.addBook title author path

    let syncMissingFlags () =
        loadBooks ()
        |> List.iter (fun book ->
            let exists = File.Exists book.path

            if exists = book.missing then
                LibraryRepository.setMissing book.id (not exists))

    let updateAuthorTags (ids: int list) (author: string option) (tags: string option) =
        if ids.Length > 0 then
            LibraryRepository.updateAuthorTags ids author tags

    let updateMetadata
        (id: int)
        (title: string option)
        (author: string option)
        (tags: string option)
        (comments: string option)
        =
        LibraryRepository.updateMetadata id title author tags comments

    let deleteBooks (ids: int list) = LibraryRepository.deleteBooks ids
