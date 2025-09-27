namespace Excalibur.Domain

type Book =
    { id: int
      title: string
      author: string option
      path: string
      tags: string option
      comments: string option
      checksum: string option
      added_at: string option
      missing: bool }
